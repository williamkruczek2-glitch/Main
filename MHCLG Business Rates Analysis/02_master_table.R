# 02_master_table.R -------------------------------------------------------------
# Master table build layer. Extracted from 01_init.R for clarity.
#
# Contents: positional and machine-name readers, per-year dispatch, D2/D5 join,
# allowance-identity check, and build_master_table() itself.
#
# Depends on helpers defined in 01_init.R (encoding detection, England
# validation, apply_writeoff_sign, parse_gbp, etc.). 01_init.R MUST be sourced
# first. The runner run_master() lives in 03_run_master.R.

# =============================================================================
# MASTER TABLE LAYER
# =============================================================================
# One long table, keyed on year x authority, holding every variable Metrics 1-3
# need. Built once in memory, narrow by design. M1/M2/M3 select from it.
#
# THREE source formats are handled (see 00_source_tests.R):
#   - positional  : 2013-14 to 2017-18. No machine names. Columns identified by
#                   DESCRIPTION text, read from the total column of each 4-group.
#   - machine_split: 2018-19, 2019-20. Names on row 3, D2 + D5 joined on Ecode.
#   - machine_flat : 2020-21 to 2023-24. Consolidated single file, names row 1.

# Canonical fields the master table carries, by metric.
# --- Encoding + numeric helpers -----------------------------------------------
# QA CALLOUT: encoding must be explicit -----------------------------------------
# Source files are a mix of cp1252 and genuine UTF-8, and the difference is not
# knowable from the year or format era: 2020-21 to 2024-25 are cp1252 (raw 0x92
# curly apostrophe), while 2025-26 is true UTF-8 (the same apostrophe as
# 0xE2 0x80 0x99). Reading cp1252 as UTF-8 either errors or leaves raw bytes in
# authority names, which then break any nchar()-based operation downstream.
#
# Tests the WHOLE file for UTF-8 validity rather than scanning a fixed prefix
# for specific bytes. Two failures of the previous approach drove this:
#   - Window: it read only the first 50,000 bytes. "King's Lynn and West
#     Norfolk" sits ~1MB into the consolidated files, so the 0x92 was never
#     seen and cp1252 files were misreported as UTF-8 for 2020-21 to 2024-25.
#   - Byte list: it tested only 0x92 and 0x96. The VOA Stock of Properties
#     files carry 0xF4 ("Ynys Mon"), which is a legal UTF-8 lead byte and so
#     cannot be detected by membership alone - only by validity.
# Any file that fails UTF-8 validation is treated as cp1252, which is the only
# other encoding these publications use.
detect_file_encoding <- function(path) {
  raw <- readBin(path, "raw", n = file.size(path))
  raw <- raw[raw != as.raw(0)]
  txt <- rawToChar(raw)
  Encoding(txt) <- "bytes"
  if (validUTF8(txt)) "UTF-8" else "latin1"
}

# QA CALLOUT: assert no raw bytes survived the read ----------------------------
# Backstop for the above. If detection is ever wrong again, the failure mode is
# silent at load and only surfaces later as an nchar() error in View() or a
# chart label. Halting here names the year and authority instead.
assert_valid_encoding <- function(tbl, year) {
  chr_cols <- names(tbl)[map_lgl(tbl, is.character)]
  bad <- keep(chr_cols, \(c) any(!validUTF8(tbl[[c]]), na.rm = TRUE))
  if (length(bad) > 0) {
    for (c in bad) {
      idx <- which(!validUTF8(tbl[[c]]))
      message("  Year ", year, ", column '", c, "': ", length(idx),
              " invalid value(s), e.g. ", tbl[[c]][idx[1]])
    }
    stop("Year ", year, ": invalid multibyte string(s) after load in column(s): ",
         paste(bad, collapse = ", "),
         ". detect_file_encoding() returned the wrong encoding for this file.",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Parse "16,627,384" or "-21,053" to numeric. Blank / "-" -> NA.
parse_gbp <- function(x) {
  x <- str_replace_all(as.character(x), ",", "")
  x <- str_trim(x)
  suppressWarnings(as.numeric(dplyr::na_if(x, "")))
}


master_fields <- list(
  identity = c("la_code", "la_name", "ons_code"),
  m1 = c("net_rates_payable", "wo_allowance", "wo_excess"),
  m2 = c("change_noncoll", "noncoll_ob", "noncoll_chrgd",
         "noncoll_collfund", "noncoll_cb"),
  m3 = c("sums_outstanding", "sums_owing")
)

# --- Positional format --------------------------------------------------------
# QA CALLOUT: description-anchored, appeals-asserting -------------------------
# The 2013-14 to 2017-18 files have NO variable names. Columns come in 4-groups
# (BAA / DA / blank / Total); the description sits on the FIRST column of the
# group, the value we want is the TOTAL, three columns to the right.
#
# The appeals-provision columns sit immediately after the write-off columns and
# are an order of magnitude LARGER (13-14: appeals £479m + £1,266m vs write-offs
# £162m). A one-group offset would silently pull appeals into Metric 1. So each
# mapping is asserted against the description text: the write-off column MUST say
# "written off" and MUST NOT say "Appeal". If the layout ever drifts, this halts
# on a labelled mismatch instead of ingesting the wrong series.
#
# Note: the old form has NO writeoffs_excnoncoll (Line 4) block - it did not
# exist until 2018-19. wo_excess is therefore NA for positional years, flagged,
# not zero. See caveat register.

# POSITIONAL READERS (2013-14 to 2017-18) ======================================
# QA CALLOUT: auto-detected layout, per-year verified specs, appeals guard ------
# The old dropdown CSVs have NO machine variable names. Columns are identified by
# DESCRIPTION text; the value we want is the TOTAL sub-column of a repeating
# group. Inspection found FIVE genuinely different description sets across the
# five years (verified against the 2016 tax gap report), so each year has its own
# spec. The structural PARAMETERS (which rows hold the descriptions, the Ecode
# labels, the data, and the group width) are AUTO-DETECTED per file rather than
# hardcoded - hardcoding row offsets is how the wrong column gets read.
#
# APPEALS GUARD: appeals provisions sit next to the write-off columns and are an
# order of magnitude larger. A one-group offset would silently pull appeals into
# Metric 1 and no downstream check would catch it. Every field is asserted
# against its description: it MUST match its expected text and MUST NOT contain
# "Appeal". A drift halts the build.
#
# Line 4 (writeoffs_excnoncoll) note: absent in 2013-14 only. Present from
# 2014-15 onward (as a "(Less): any sums" / "in excess" column).

# Auto-detect the four structural parameters from a raw positional file.
detect_positional_layout <- function(raw) {
  n_probe <- min(8L, nrow(raw))
  rowchr <- function(i) as.character(raw[i, ])
  
  # Ecode label row: contains a cell exactly "Ecodes" or "Ecode".
  code_row <- NA_integer_
  for (i in seq_len(n_probe)) {
    if (any(str_trim(rowchr(i)) %in% c("Ecodes", "Ecode"))) { code_row <- i; break }
  }
  if (is.na(code_row)) stop("Positional: no Ecode(s) label row found.", call. = FALSE)
  labels <- str_trim(rowchr(code_row))
  code_col <- which(labels %in% c("Ecodes", "Ecode"))[1]
  
  # Description row: first row mentioning "ayable" (Net Rates Payable / Sum payable).
  desc_row <- NA_integer_
  for (i in seq_len(n_probe)) {
    if (any(str_detect(coalesce(rowchr(i), ""), "ayable"))) { desc_row <- i; break }
  }
  if (is.na(desc_row)) stop("Positional: no description row found.", call. = FALSE)
  
  # Data row: first row after code_row whose code_col holds a 5-char E-code.
  data_row <- NA_integer_
  for (i in (code_row + 1L):min(code_row + 5L, nrow(raw))) {
    v <- str_trim(as.character(raw[[code_col]][i]))
    if (!is.na(v) && str_starts(v, "E") && nchar(v) == 5) { data_row <- i; break }
  }
  if (is.na(data_row)) stop("Positional: no data row found.", call. = FALSE)
  
  # Group width: the sub-number row (between desc and data) repeats 1,2,3(,4).
  width <- NA_integer_
  for (i in (desc_row + 1L):(data_row - 1L)) {
    seq_i <- coalesce(str_trim(rowchr(i)), "")
    ones <- which(seq_i == "1")
    if (length(ones) >= 2 && sum(seq_i == "2") >= 2) { width <- ones[2] - ones[1]; break }
  }
  if (is.na(width)) stop("Positional: could not detect group width.", call. = FALSE)
  
  list(code_row = code_row, code_col = code_col, desc_row = desc_row,
       data_row = data_row, width = width)
}

# Per-year field specs. Each entry: the exact description substring to match, and
# the appeals guard text that must NOT appear. Keyed by the year's D2 filename
# stem so the correct set is picked deterministically.
positional_specs <- list(
  "13_14" = list(
    net_rates_payable = list(desc = "Net Rates Payable"),
    wo_allowance      = list(desc = "Sums written off"),
    change_noncoll    = list(desc = "Change in allowance"),
    sums_outstanding  = list(desc = "Sums outstanding from ratepayers", width1 = TRUE),
    sums_owing        = list(desc = "Sums owed to ratepayers", width1 = TRUE)
  ),
  "14_15" = list(
    net_rates_payable = list(desc = "Net Rates Payable"),
    wo_allowance      = list(desc = "Losses On Collection-Write-offs"),
    wo_excess         = list(desc = "Losses On Collection-(Less): any sums"),
    change_noncoll    = list(desc = "Losses On Collection-Add/(Less): allowance"),
    sums_outstanding  = list(desc = "Debtors and Pre-Payments - outstanding", width1 = TRUE)
  ),
  "15_16" = list(
    net_rates_payable = list(desc = "Sum payable by rate payers"),
    wo_allowance      = list(desc = "Write-offs charged to the allowance"),
    wo_excess         = list(desc = "Any sums written off or written back"),
    change_noncoll    = list(desc = "Change in allowance for non collection"),
    sums_outstanding  = list(desc = "Sums outstanding from ratepayers", width1 = TRUE),
    sums_owing        = list(desc = "Sums owed to ratepayers", width1 = TRUE)
  ),
  "16_17" = list(
    net_rates_payable = list(desc = "Sum payable by rate payers"),
    wo_allowance      = list(desc = "Write-offs charged to the allowance"),
    wo_excess         = list(desc = "Any sums written off or written back"),
    change_noncoll    = list(desc = "Change in allowance for non collection"),
    sums_outstanding  = list(desc = "Sums outstanding from ratepayers", width1 = TRUE),
    sums_owing        = list(desc = "Sums owed to ratepayers", width1 = TRUE)
  ),
  "17_18" = list(
    net_rates_payable = list(desc = "Sum payable by rate payers"),
    wo_allowance      = list(desc = "Write-offs charged to the allowance"),
    wo_excess         = list(desc = "Any sums written off or written back"),
    change_noncoll    = list(desc = "Change in allowance for non collection"),
    sums_outstanding  = list(desc = "Sums outstanding from ratepayers", width1 = TRUE),
    sums_owing        = list(desc = "Sums owed to ratepayers", width1 = TRUE)
  )
)

read_nndr3_positional <- function(path, year, spec_key) {
  spec <- positional_specs[[spec_key]]
  if (is.null(spec)) {
    stop("Year ", year, ": no positional spec registered for '", spec_key, "'.",
         call. = FALSE)
  }
  
  enc <- detect_file_encoding(path)
  raw <- suppressWarnings(read_csv(
    path, col_names = FALSE, skip = 0, show_col_types = FALSE,
    locale = locale(encoding = enc), name_repair = "minimal"
  ))
  
  L <- detect_positional_layout(raw)
  desc_row <- as.character(raw[L$desc_row, ])
  data <- raw[L$data_row:nrow(raw), ]
  
  message("  Year ", year, " (positional ", spec_key, ", width ", L$width, "):")
  out <- tibble(la_code = str_trim(as.character(data[[L$code_col]])))
  
  for (field in names(spec)) {
    dtext <- spec[[field]]$desc
    desc_col <- which(str_detect(coalesce(desc_row, ""), fixed(dtext)))[1]
    if (is.na(desc_col)) {
      stop("Year ", year, " (positional ", spec_key, "): no column described '",
           dtext, "'. Layout differs; do not guess.", call. = FALSE)
    }
    found_desc <- desc_row[desc_col]
    
    # APPEALS GUARD: refuse if the resolved column is an appeals column.
    if (str_detect(found_desc, regex("Appeal", ignore_case = TRUE))) {
      stop("Year ", year, " (positional ", spec_key, "): column for '", field,
           "' resolved to '", found_desc, "', an APPEALS column. Refusing to ",
           "ingest - appeals/write-off mix guard.", call. = FALSE)
    }
    
    # QA CALLOUT: per-field width override. Most fields sit in repeating
    # BAA/DA/(blank)/Total groups, so the value is the TOTAL sub-column. But the
    # arrears fields ("Debtors and Pre-Payments" / "Sums outstanding") are single
    # columns (width 1) - reading them at the file-wide offset lands on an empty
    # column and silently sums to zero (the 2013-18 arrears = 0 bug). Fields
    # flagged width1 in the spec use offset 0.
    field_width <- if (isTRUE(spec[[field]]$width1)) 1L else L$width
    total_col <- desc_col + field_width - 1L
    message("    ", field, " <- col ", total_col, " ('",
            str_trunc(found_desc, 40), "')")
    out[[field]] <- parse_gbp(data[[total_col]])
  }
  
  # Fields not present in this vintage.
  out$la_name <- NA_character_
  for (f in c("wo_excess", "sums_owing", "noncoll_ob", "noncoll_chrgd",
              "noncoll_collfund", "noncoll_cb")) {
    if (!f %in% names(out)) out[[f]] <- NA_real_
  }
  
  # QA CALLOUT: sign normalisation for 2013-14 ----------------------------------
  # 2013-14 files write-offs as POSITIVE values (verified: £161.9m positive),
  # whereas 2014-15 onward - and all machine years - file them as NEGATIVE
  # deductions. The global apply_writeoff_sign() flip assumes negative-filed. So
  # 2013-14's write-off fields are negated HERE to match the common convention;
  # after that the single global flip turns them back positive uniformly. Without
  # this, 2013-14 would come out as -£161.9m.
  if (spec_key == "13_14") {
    out$wo_allowance <- -out$wo_allowance
    if ("wo_excess" %in% names(out)) out$wo_excess <- -out$wo_excess
  }
  
  out |>
    filter(!is.na(la_code), str_starts(la_code, "E"), la_code != "E0000") |>
    mutate(source_format = paste0("positional_", spec_key))
}

# Positional D5 reader ---------------------------------------------------------
# Verified TOTAL columns from the supplied 2013-14 to 2017-18 D5 files.
# D5 supplies allowance opening/closing balances for Metric 2 and the preferred
# balance-sheet arrears fields for Metric 3. D2 remains the source for Metric 1
# and change_noncoll.
positional_d5_specs <- list(
  "13_14" = list(
    data_row = 8L, code_col = 2L, name_col = 5L,
    columns = c(
      sums_outstanding = 32L, sums_owing = 37L,
      noncoll_ob = 42L, noncoll_collfund = 47L, noncoll_cb = 52L
    )
  ),
  "14_15" = list(
    data_row = 8L, code_col = 2L, name_col = 5L,
    columns = c(
      sums_outstanding = 34L, sums_owing = 39L,
      noncoll_ob = 44L, noncoll_chrgd = 49L,
      noncoll_collfund = 54L, noncoll_cb = 59L
    )
  ),
  "15_16" = list(
    data_row = 6L, code_col = 2L, name_col = 3L,
    columns = c(
      sums_outstanding = 27L, sums_owing = 32L,
      noncoll_ob = 37L, noncoll_chrgd = 42L,
      noncoll_collfund = 47L, noncoll_cb = 52L
    )
  ),
  "16_17" = list(
    data_row = 6L, code_col = 2L, name_col = 3L,
    columns = c(
      sums_outstanding = 27L, sums_owing = 32L,
      noncoll_ob = 37L, noncoll_chrgd = 42L,
      noncoll_collfund = 47L, noncoll_cb = 52L
    )
  ),
  "17_18" = list(
    data_row = 5L, code_col = 2L, name_col = 3L,
    columns = c(
      sums_outstanding = 37L, sums_owing = 42L,
      noncoll_ob = 47L, noncoll_chrgd = 52L,
      noncoll_collfund = 57L, noncoll_cb = 62L
    )
  )
)

read_nndr3_positional_d5 <- function(path, year, spec_key) {
  spec <- positional_d5_specs[[spec_key]]
  if (is.null(spec)) {
    stop("Year ", year, ": no positional D5 spec for ", spec_key, ".",
         call. = FALSE)
  }
  
  raw <- suppressWarnings(read_csv(
    path, col_names = FALSE, show_col_types = FALSE,
    locale = locale(encoding = detect_file_encoding(path)),
    name_repair = "minimal",
    # QA CALLOUT: force character, or readr silently NAs the closing balance ------
    # These files carry small integers in the header rows (row/col numbers like
    # 52, 47, 5) ABOVE the data. With default type guessing readr inspects those
    # leading cells, guesses INTEGER for the column, then hits the data values
    # "404,530" / "-429,959": the thousands comma is not an accepted grouping mark
    # so every value parses to NA - before parse_gbp ever runs. noncoll_cb came
    # back 100% NA for exactly this reason (verified: raw cell = "404,530", column
    # guessed integer). Reading everything as character defers all numeric parsing
    # to parse_gbp(), which strips commas correctly. The D2 reader is unaffected
    # only because its target columns happened to guess character.
    col_types = cols(.default = col_character())
  ))
  
  if (nrow(raw) < spec$data_row || ncol(raw) < max(spec$columns)) {
    stop("Year ", year, " D5 does not match the verified layout.",
         call. = FALSE)
  }
  
  data <- raw[spec$data_row:nrow(raw), , drop = FALSE]
  out <- tibble(
    la_code = str_trim(as.character(data[[spec$code_col]])),
    la_name = str_squish(as.character(data[[spec$name_col]]))
  )
  
  for (field in names(spec$columns)) {
    out[[field]] <- parse_gbp(data[[spec$columns[[field]]]])
  }
  if (!"noncoll_chrgd" %in% names(out)) out$noncoll_chrgd <- NA_real_
  
  # QA CALLOUT: allowance-stock sign normalisation for 2013-14 ------------------
  # The D5 allowance reconciliation is filed as a NEGATIVE liability in 2014-15
  # onward and in all machine years (verified England closing balance: 14-15
  # -£674m, 15-16 -£1,335m, ... ), which is the convention 10_metric2.R relies on
  # when it computes allowance_cb = -noncoll_cb. 2013-14 alone files these stock
  # fields POSITIVE (verified England closing balance +£630m). Left as-is, the
  # single negation in Metric 2 would render 2013-14 as -£630m - a sign flip at
  # the very start of the series. Negate the 2013-14 stock fields here so the
  # whole series shares one convention and Metric 2's negation stays uniform.
  # Mirrors the equivalent 2013-14 D2 write-off normalisation above. See caveat
  # register (allowance-stock sign, positional years).
  if (spec_key == "13_14") {
    for (f in c("noncoll_ob", "noncoll_chrgd", "noncoll_collfund", "noncoll_cb")) {
      if (f %in% names(out)) out[[f]] <- -out[[f]]
    }
  }
  
  out |>
    filter(!is.na(la_code), str_starts(la_code, "E"), la_code != "E0000") |>
    distinct(la_code, .keep_all = TRUE)
}

# --- Machine-name formats -----------------------------------------------------
# Shared canonical rename for both machine-name variants.
machine_rename <- c(
  la_code           = "Ecode",
  la_name           = "Local_Authority",
  net_rates_payable = "netrates_tot",
  wo_allowance      = "writeoffs_noncoll_tot",
  wo_excess         = "writeoffs_excnoncoll_tot",
  change_noncoll    = "change_noncoll_tot",
  noncoll_ob        = "noncoll_ob_tot",
  noncoll_chrgd     = "noncoll_chrgd_tot",
  noncoll_collfund  = "noncoll_collfund_tot",
  noncoll_cb        = "noncoll_cb_tot",
  sums_outstanding  = "sums_outstanding",
  sums_owing        = "sums_owing",
  # ons_code is optional: only the recent consolidated files carry ONSCode. Where
  # absent it is simply not selected (present<- filters to existing columns), and
  # the master_fields loop backfills it as NA. Needed for the choropleth join.
  ons_code          = "ONSCode"
)

# Read one machine-name file (flat or a D2/D5 half).
# QA CALLOUT: the key column differs between the two machine variants ------------
# Flat consolidated files (2020-21 on) name the key column "Ecode" on row 1.
# Split D2/D5 files (2018-19, 2019-20) carry VALUE names on row 3, but the
# identity labels (Status, Ecode, LA name) sit on a LOWER row, and column 1 is
# STATUS, not the code - the Ecode is column 2. Hardcoding position 1 as the
# code therefore picks up Status ("F"/"P"/blank), and the str_starts("E")
# filter silently drops every row. So identity columns are located by SEARCHING
# the label row for "Ecode"/"LA name", never by position. Verified against the
# real files: 2018-19 D2 = 326 authorities, 2019-20 D2 = 317.
read_machine_file <- function(path, year, header_row = 1L, split = FALSE) {
  enc <- detect_file_encoding(path)
  
  if (split) {
    # Read raw (no header) so we can find the identity labels wherever they sit.
    allrows <- suppressWarnings(read_csv(
      path, col_names = FALSE, show_col_types = FALSE,
      locale = locale(encoding = enc), name_repair = "minimal"
    ))
    var_names <- as.character(allrows[header_row, ])   # value names (row 3)
    
    # Find the identity label row: the row containing an "Ecode" cell.
    id_row_idx <- which(apply(allrows, 1, \(r) any(str_trim(as.character(r)) == "Ecode")))
    id_row_idx <- id_row_idx[id_row_idx > header_row][1]
    if (is.na(id_row_idx)) {
      stop("Year ", year, " (split): no identity row containing 'Ecode' found in ",
           basename(path), ". Layout differs; not read.", call. = FALSE)
    }
    id_labels <- str_trim(as.character(allrows[id_row_idx, ]))
    code_col <- which(id_labels == "Ecode")[1]
    name_col <- which(str_detect(id_labels, regex("^LA name$", ignore_case = TRUE)))[1]
    if (is.na(name_col)) name_col <- code_col + 1L   # name sits next to code
    
    # Build a header: value names from row 3, code/name from the label row.
    header <- var_names
    header[code_col] <- "la_code"
    header[name_col] <- "la_name"
    raw <- allrows[(id_row_idx + 1L):nrow(allrows), ]
    names(raw) <- header
    
    # QA CALLOUT: assert the key column really is codes, not Status. If the
    # located column is wrong, most values will not start with "E" and we halt
    # rather than silently filtering to zero rows.
    code_vals <- str_trim(as.character(raw[["la_code"]]))
    e_share <- mean(str_starts(code_vals, "E"), na.rm = TRUE)
    if (is.na(e_share) || e_share < 0.5) {
      stop("Year ", year, " (split): located key column '", id_labels[code_col],
           "' but only ", round(100 * e_share), "% of values start with 'E'. ",
           "Wrong column - refusing to ingest zero rows silently.", call. = FALSE)
    }
    
  } else {
    raw <- suppressWarnings(read_csv(
      path, skip = header_row - 1L, show_col_types = FALSE,
      locale = locale(encoding = enc), name_repair = "minimal"
    ))
    names(raw)[names(raw) == "Ecodes"] <- "Ecode"
    names(raw)[names(raw) == "Ecode"] <- "la_code"
    names(raw)[names(raw) == "Local_Authority"] <- "la_name"
  }
  
  value_rename <- machine_rename[!names(machine_rename) %in% c("la_code", "la_name")]
  present <- value_rename[value_rename %in% names(raw)]
  
  keep <- c(
    intersect(c("la_code", "la_name"), names(raw)),
    unname(present)
  )
  got <- raw |>
    select(all_of(keep)) |>
    rename(!!!setNames(unname(present), names(present)))
  
  # QA CALLOUT: ons_code is an IDENTIFIER, not a value --------------------------
  # value_cols must exclude every character identifier. ons_code holds GSS codes
  # like "E07000223"; running parse_gbp() over it calls as.numeric() on a
  # non-numeric string and silently turns every code into NA. That destroyed the
  # boundary join and rendered the whole choropleth grey. Identifiers are listed
  # explicitly so a new one cannot be swept into the numeric coercion.
  id_cols <- c("la_code", "la_name", "ons_code")
  value_cols <- setdiff(names(got), id_cols)
  
  out <- got |>
    mutate(
      la_code = str_trim(as.character(la_code)),
      across(all_of(value_cols), parse_gbp)
    ) |>
    filter(!is.na(la_code), str_starts(la_code, "E"), la_code != "E0000")
  
  # Assert the identifier survived, so this cannot regress silently.
  if ("ons_code" %in% names(out)) {
    out$ons_code <- str_trim(as.character(out$ons_code))
    if (all(is.na(out$ons_code))) {
      warning("Year ", year, ": ons_code is entirely NA after load. ",
              "Maps for this year will be blank.", call. = FALSE)
    }
  }
  out
}

# --- Per-year dispatch + join -------------------------------------------------
# QA CALLOUT: format is chosen per year, and the join is asserted ---------------
# The master builder decides format from cfg$year_formats (explicit, not
# guessed). For split years it reads D2 and D5 and joins on la_code, then
# asserts the two halves covered the SAME authorities - a failed join means
# mismatched files, which is the 19-20 duplicate class of error.
load_master_year <- function(year) {
  fmt <- cfg$year_formats[[as.character(year)]]
  if (is.null(fmt)) stop("No format registered for year ", year,
                         " in cfg$year_formats.", call. = FALSE)
  
  message("Year ", year, " (", fmt$type, "):")
  
  if (fmt$type == "machine_flat") {
    tbl <- read_machine_file(file.path(cfg$dir_data, fmt$file), year, header_row = 1L)
    
  } else if (fmt$type == "machine_split") {
    d2 <- read_machine_file(file.path(cfg$dir_data, fmt$d2), year,
                            header_row = 3L, split = TRUE)
    d5 <- read_machine_file(file.path(cfg$dir_data, fmt$d5), year,
                            header_row = 3L, split = TRUE)
    d5 <- d5 |> select(-any_of("la_name"))
    tbl <- assert_join(d2, d5, year)
    
  } else if (fmt$type == "positional") {
    d2 <- read_nndr3_positional(
      file.path(cfg$dir_data, fmt$d2), year, fmt$spec
    )
    d5 <- read_nndr3_positional_d5(
      file.path(cfg$dir_data, fmt$d5), year, fmt$spec
    )
    
    # D2 remains the source for Metric 1 and change_noncoll. D5 supplies the
    # allowance stock fields for Metric 2 and the balance-sheet arrears fields.
    # QA CALLOUT: strip D2's placeholder allowance-stock columns before the join -
    # read_nndr3_positional() backfills noncoll_ob/noncoll_chrgd/noncoll_collfund/
    # noncoll_cb as NA_real_ placeholders, since D2 never sources them - D5 does.
    # Left in, full_join() finds those SAME NAMES on both sides and silently
    # suffixes them to .x/.y rather than erroring, so the plain "noncoll_cb"
    # column no longer exists after the join. The "guarantee every canonical
    # field" backfill a few lines below then treats it as genuinely missing and
    # creates a fresh NA_real_ column - discarding D5's real values with no
    # warning anywhere. VERIFIED: this was the actual cause of allowance_cb and
    # allowance_change reading NA for all five positional years; both readers
    # were individually correct, but their outputs collided at this join. Fixed
    # by dropping D2's placeholder versions before the join, exactly as
    # sums_outstanding/sums_owing (D2's OWN placeholders for D5-sourced fields)
    # are already dropped on the line below.
    d2 <- d2 |> select(-any_of(c("sums_outstanding", "sums_owing",
                                 "noncoll_ob", "noncoll_chrgd",
                                 "noncoll_collfund", "noncoll_cb")))
    d5 <- d5 |> select(-any_of("la_name"))
    tbl <- assert_join(d2, d5, year)
    
  } else {
    stop("Unknown format type '", fmt$type, "' for year ", year, ".", call. = FALSE)
  }
  
  # Guarantee every canonical field exists, even if this vintage lacks it.
  # QA CALLOUT: ons_code is character; the rest are numeric. Backfilling it as
  # NA_real_ would clash on bind_rows with a year that has real codes, and break
  # the map join. Type the fill to the field.
  all_fields <- unlist(master_fields, use.names = FALSE)
  for (f in setdiff(all_fields, names(tbl))) {
    tbl[[f]] <- if (f %in% c("la_name", "ons_code")) NA_character_ else NA_real_
  }
  # Ensure ons_code is character even where present, so years stack cleanly.
  if ("ons_code" %in% names(tbl)) tbl$ons_code <- as.character(tbl$ons_code)
  
  assert_valid_encoding(tbl, year)
  
  tbl |>
    mutate(year = year, financial_year = label_fy(year), .before = 1) |>
    apply_writeoff_sign()
}

# Join D2 + D5 and assert they described the same authorities.
assert_join <- function(d2, d5, year) {
  only_d2 <- setdiff(d2$la_code, d5$la_code)
  only_d5 <- setdiff(d5$la_code, d2$la_code)
  if (length(only_d2) > 0 || length(only_d5) > 0) {
    message("    D2-only codes: ", paste(head(only_d2, 5), collapse = ", "))
    message("    D5-only codes: ", paste(head(only_d5, 5), collapse = ", "))
    stop("Year ", year, ": D2 and D5 cover different authorities (",
         length(only_d2), " only in D2, ", length(only_d5),
         " only in D5). Files may be mismatched or a duplicate.", call. = FALSE)
  }
  full_join(d2, d5, by = "la_code")
}

# QA CALLOUT: the cross-format identity check ----------------------------------
# noncoll_chrgd (D5, Part 5) is the same quantity as wo_allowance (Line 3, D2,
# Part 2) but filed with the OPPOSITE sign: D2 files it as a deduction
# (negative), D5 as a positive charge to the allowance. Verified on 2022-23:
# wo_noncoll -21,053 vs noncoll_chrgd +21,053, and ob + chrgd + cf = cb to the
# pound. The check therefore compares magnitudes. A mismatch here means the
# D2/D5 join paired the wrong columns - the strongest alignment check for a
# machine-split year. Positional years have no noncoll_chrgd and are skipped.
#
# NOTE for Metric 2: the D5 allowance fields carry the opposite sign to the D2
# write-off fields. This must be reconciled explicitly when M2 is built, not
# assumed away. Flagged in the caveat register.
check_allowance_identity <- function(master, tol = 1) {
  both <- master |>
    filter(!is.na(noncoll_chrgd), !is.na(wo_allowance)) |>
    mutate(diff = abs(abs(noncoll_chrgd) - abs(wo_allowance))) |>
    filter(diff > tol)
  if (nrow(both) > 0) {
    print(both |> select(year, la_code, wo_allowance, noncoll_chrgd, diff) |> head(10))
    warning(nrow(both), " authority-years where |D5 noncoll_chrgd| != |D2 Line 3|. ",
            "Join alignment suspect.", call. = FALSE)
  } else {
    message("  Allowance identity holds (|Line 3| == |noncoll_chrgd|) on all ",
            "machine-split rows checked.")
  }
  invisible(both)
}

# --- Build the master table ---------------------------------------------------
build_master_table <- function(years = cfg$master_years) {
  message(strrep("-", 70))
  message("BUILDING MASTER TABLE")
  message(strrep("-", 70))
  
  parts <- map(years, load_master_year)
  names(parts) <- as.character(years)
  
  # QA CALLOUT: every requested year must contribute a plausible row count ------
  # The row-count-reconciles check downstream compares the master against its
  # OWN per-year sum, so a year contributing zero rows still reconciles - which
  # is exactly how the split-file column-shift bug hid (2018-19, 2019-20 read to
  # zero rows, total still matched). Assert per-year presence here instead.
  # NNDR3 England has ~296-326 billing authorities depending on year; anything
  # under 250 means the reader dropped rows.
  row_counts <- map_int(parts, nrow)
  bad <- names(row_counts)[row_counts < 250]
  if (length(bad) > 0) {
    print(tibble(year = names(row_counts), rows = row_counts))
    stop("Year(s) ", paste(bad, collapse = ", "), " returned implausibly few ",
         "rows (<250). The reader is dropping authorities - do not proceed.",
         call. = FALSE)
  }
  message("  Per-year row counts (all >= 250): ",
          paste(names(row_counts), row_counts, sep = "=", collapse = ", "))
  
  master <- list_rbind(parts)
  
  # Sign convention is applied per year inside load_master_year(). A positive
  # value is a write-off after the flip.
  master
}