# 14_stock_of_properties.R ------------------------------------------------------
# Joins VOA Stock of Properties (hereditament counts by billing authority) onto
# the master table across the full 2013-25 series.
#
# The sector files are wide panels, not single-year cuts: each carries 31 March
# 2011 to 31 March 2025 as columns, and includes abolished authorities with
# their historic data. The SOP side therefore already covers the whole series.
#
# JOIN KEY: ons_code, NOT the SOP "BA Code". SOP's BA Code and the NNDR3 Ecode
# are different numbering schemes sharing only a county prefix (Adur is 3805 in
# SOP, E3831 in NNDR3); only 28 of 296 collide by coincidence. On ons_code the
# 296 live English LAUA rows match the 296 NNDR3 authorities exactly.
#
# Master carries ons_code only from 2020-21 (see load_master_year() in
# 02_master_table.R), so build_la_ons_crosswalk() reconstructs it for earlier
# years - three layers, documented there.
#
# YEAR ALIGNMENT: opening stock. SOP 31 March Y maps to master year Y, the stock
# at the start of financial year Y/Y+1. Set cfg$sop_year_alignment to "closing"
# to map 31 March Y to master year Y-1 instead.
#
# Depends on: 00_config.R, 01_init.R, 02_master_table.R.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")

# --- Config additions ---------------------------------------------------------

cfg$dir_sop <- cfg$dir_sop <- file.path(cfg$dir_data, "SOP")

cfg$sop_files <- c(
  total      = "ndr_stock_of_properties_2025_total.csv",
  retail     = "ndr_stock_of_properties_2025_retail.csv",
  office     = "ndr_stock_of_properties_2025_office.csv",
  industrial = "ndr_stock_of_properties_2025_industrial.csv",
  other      = "ndr_stock_of_properties_2025_other.csv",
  change_in  = "ndr_stock_of_properties_2025_change_in.csv"
)


cfg$sop_header_row     <- 7L
cfg$sop_year_alignment <- "opening"
cfg$sop_ons_overrides  <- file.path(cfg$dir_lookup, "la_code_ons_overrides.csv")

cfg$sop_source_label <- paste(
  "VOA/HMRC Valuation Office, Non-domestic rating: stock of properties,",
  "31 March 2025 edition"
)

# =============================================================================
# 1. READING
# =============================================================================
# Counts are published rounded to the nearest 10, so authority sums do not
# reconcile exactly to the published England row (~190 at 31 March 2025).
# [z] = not applicable, [c] = suppressed (value 1-4); both parse to NA.
parse_sop_count <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, ",", "")
  x[str_detect(x, "^\\[.*\\]$")] <- NA_character_
  suppressWarnings(as.numeric(dplyr::na_if(x, "")))
}

# Header carries note references ("BA Code [note 5b]") and embedded newlines.
strip_note <- function(x) {
  x |> str_replace_all("\\[note[^]]*\\]", "") |>
    str_replace_all("\\s+", " ") |> str_trim()
}

read_sop_raw <- function(path) {
  if (!file.exists(path)) {
    stop("Stock of Properties file not found: ", path,
         "\nExpected in ", cfg$dir_sop, "/ - see cfg$sop_files.", call. = FALSE)
  }
  raw <- read_csv(
    path, skip = cfg$sop_header_row - 1L, show_col_types = FALSE,
    name_repair = "minimal",
    locale = locale(encoding = detect_file_encoding(path)),
    col_types = cols(.default = col_character())
  )
  names(raw) <- strip_note(names(raw))
  raw[, names(raw) != "" & !is.na(names(raw)), drop = FALSE]
}

# QA CALLOUT: three filters, all required -------------------------------------
# Geography == "LAUA" drops 29 county/met aggregate rows; the E prefix drops 22
# Welsh authorities; the abolished flag separates 36 defunct authorities from
# the 296 live ones. Abolished rows are KEPT by default because they carry the
# pre-reorganisation history the early years of the series need.
filter_sop_authorities <- function(raw, keep_abolished = TRUE) {
  needed <- c("Geography", "ONS area code", "ONS area name", "Abolished year")
  missing_cols <- setdiff(needed, names(raw))
  if (length(missing_cols) > 0) {
    message("  Columns present: ", paste(names(raw), collapse = " | "))
    stop("SOP file is missing expected column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- raw |>
    mutate(across(all_of(needed), \(x) str_trim(as.character(x)))) |>
    filter(Geography == "LAUA", str_starts(`ONS area code`, "E"))
  if (!keep_abolished) out <- out |> filter(`Abolished year` == "[z]")
  out
}

# Sector files are wide by snapshot date; pivot to one row per authority-year.
read_sop_sector <- function(key) {
  dat <- read_sop_raw(file.path(cfg$dir_sop, cfg$sop_files[[key]])) |>
    filter_sop_authorities()
  
  year_cols <- names(dat)[str_detect(names(dat), "^31 March \\d{4}$")]
  if (length(year_cols) == 0) {
    message("  Columns present: ", paste(names(dat), collapse = " | "))
    stop("No '31 March YYYY' columns found in the ", key, " file.", call. = FALSE)
  }
  
  dat |>
    select(ons_code = `ONS area code`, sop_name = `ONS area name`,
           abolished_year = `Abolished year`, all_of(year_cols)) |>
    pivot_longer(all_of(year_cols), names_to = "snapshot", values_to = "n") |>
    mutate(snapshot_year = as.integer(str_extract(snapshot, "\\d{4}")),
           n = parse_sop_count(n),
           sector = key) |>
    select(-snapshot)
}

load_sop_sectors <- function() {
  keys <- c("total", "retail", "office", "industrial", "other")
  long <- map(keys, read_sop_sector) |> list_rbind()
  
  wide <- long |>
    pivot_wider(names_from = sector, values_from = n, names_prefix = "sop_n_")
  
  sector_cols <- paste0("sop_n_", c("retail", "office", "industrial", "other"))
  chk <- wide |>
    mutate(sector_sum = rowSums(across(all_of(sector_cols)), na.rm = TRUE),
           diff = sector_sum - sop_n_total) |>
    filter(!is.na(diff), abs(diff) > 0)
  if (nrow(chk) > 0) {
    message("  NOTE: ", nrow(chk), " authority-year(s) where the four sectors ",
            "do not sum to the published total. Largest differences:")
    print(chk |> arrange(desc(abs(diff))) |>
            select(ons_code, sop_name, snapshot_year, sop_n_total,
                   sector_sum, diff) |> head(10))
  }
  wide
}

# QA CALLOUT: the change file is NOT annual -----------------------------------
# It covers 1 April 2023 to 31 March 2025 - the 2023 rating list period to date,
# two years cumulative. Returned as a separate table and deliberately NOT joined
# to the year panel, which would imply an annual figure. Deletions and mergers
# are filed negative.
load_sop_change <- function() {
  dat <- read_sop_raw(file.path(cfg$dir_sop, cfg$sop_files[["change_in"]])) |>
    filter_sop_authorities()
  
  rename_map <- c(
    sop_chg_open       = "Rateable properties, 1 April 2023",
    sop_chg_insertions = "Insertions",
    sop_chg_deletions  = "Deletions",
    sop_chg_splits     = "Splits",
    sop_chg_mergers    = "Mergers",
    sop_chg_net        = "Net change",
    sop_chg_close      = "Rateable properties, 31 March 2025"
  )
  present <- rename_map[rename_map %in% names(dat)]
  if (length(present) < length(rename_map)) {
    message("  Columns present: ", paste(names(dat), collapse = " | "))
    stop("Change file is missing expected column(s): ",
         paste(setdiff(rename_map, names(dat)), collapse = ", "), call. = FALSE)
  }
  
  dat |>
    select(ons_code = `ONS area code`, sop_name = `ONS area name`,
           all_of(unname(present))) |>
    rename(!!!present) |>
    mutate(across(starts_with("sop_chg_"), parse_sop_count),
           sop_deletion_rate = if_else(!is.na(sop_chg_open) & sop_chg_open > 0,
                                       abs(sop_chg_deletions) / sop_chg_open,
                                       NA_real_),
           sop_change_period = "1 April 2023 to 31 March 2025")
}

# =============================================================================
# 2. la_code -> ons_code CROSSWALK
# =============================================================================
# Master carries ons_code only for machine_flat years (2020-21 on). Three
# layers, in precedence order: manual override, bootstrap, name match.
#
# The bootstrap resolves every authority still live in 2020-21, which includes
# everything abolished in 2021 and 2023. Only the 12 authorities abolished in
# 2019 (8 Dorset/BCP, 4 Buckinghamshire) fall outside it.

normalise_la_name <- function(x) {
  x |> as.character() |> str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("\\b(ua|bc|dc|mbc|cc|city of|borough of|district of|the)\\b", " ") |>
    str_replace_all("[^a-z0-9]+", " ") |> str_squish()
}

load_ons_overrides <- function(path = cfg$sop_ons_overrides) {
  if (!file.exists(path)) {
    return(tibble(la_code = character(), ons_code = character()))
  }
  ov <- read_csv(path, show_col_types = FALSE, name_repair = "minimal") |>
    mutate(across(everything(), \(x) str_trim(as.character(x))))
  missing_cols <- setdiff(c("la_code", "ons_code"), names(ov))
  if (length(missing_cols) > 0) {
    stop("Override lookup is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  message("  Overrides loaded: ", nrow(ov))
  ov |> select(la_code, ons_code) |> distinct()
}

build_la_ons_crosswalk <- function(master, sop_authorities) {
  all_codes <- master |>
    group_by(la_code) |>
    summarise(la_name = last(na.omit(la_name)),
              years = paste(range(year), collapse = "-"), .groups = "drop")
  
  # Layer 1: bootstrap from years where master holds both identifiers.
  boot <- master |>
    filter(!is.na(ons_code), ons_code != "") |>
    distinct(la_code, ons_code)
  
  clash <- boot |> count(la_code) |> filter(n > 1)
  if (nrow(clash) > 0) {
    print(boot |> semi_join(clash, by = "la_code") |> arrange(la_code))
    stop("Bootstrap found la_code(s) mapping to more than one ons_code.",
         call. = FALSE)
  }
  message("  Layer 1 (bootstrap from master): ", nrow(boot), " of ",
          nrow(all_codes), " authority code(s) resolved.")
  
  # Layer 2: name match for whatever the bootstrap missed. Printed for review
  # rather than applied silently.
  unresolved <- all_codes |> anti_join(boot, by = "la_code")
  sop_names <- sop_authorities |>
    distinct(ons_code, sop_name) |>
    mutate(key = normalise_la_name(sop_name))
  
  name_hits <- unresolved |>
    mutate(key = normalise_la_name(la_name)) |>
    inner_join(sop_names, by = "key", relationship = "many-to-many") |>
    select(la_code, la_name, ons_code, sop_name, years)
  
  ambiguous <- name_hits |> count(la_code) |> filter(n > 1)
  if (nrow(ambiguous) > 0) {
    message("  Layer 2: ", nrow(ambiguous), " code(s) matched more than one ",
            "SOP name and were NOT applied - add them to ",
            cfg$sop_ons_overrides)
    print(name_hits |> semi_join(ambiguous, by = "la_code"), n = Inf)
    name_hits <- name_hits |> anti_join(ambiguous, by = "la_code")
  }
  if (nrow(name_hits) > 0) {
    message("  Layer 2 (name match): ", nrow(name_hits),
            " code(s) resolved. Review these:")
    print(name_hits |> select(la_code, la_name, sop_name, ons_code, years),
          n = Inf)
  }
  
  # Layer 3: manual overrides take precedence over both.
  overrides <- load_ons_overrides()
  
  crosswalk <- bind_rows(
    boot |> mutate(source = "bootstrap"),
    name_hits |> select(la_code, ons_code) |> mutate(source = "name_match")
  ) |>
    anti_join(overrides, by = "la_code") |>
    bind_rows(overrides |> mutate(source = "override")) |>
    distinct(la_code, .keep_all = TRUE)
  
  still_missing <- all_codes |> anti_join(crosswalk, by = "la_code")
  if (nrow(still_missing) > 0) {
    out_path <- file.path(cfg$dir_output, "qa_sop_unmapped_la_codes.csv")
    write_csv(still_missing, out_path)
    message("  UNRESOLVED: ", nrow(still_missing), " authority code(s) have no ",
            "ons_code. Written to ", out_path, " - add them to ",
            cfg$sop_ons_overrides, ":")
    print(still_missing, n = Inf)
  } else {
    message("  All ", nrow(all_codes), " authority code(s) resolved.")
  }
  
  crosswalk
}

# =============================================================================
# 3. JOIN
# =============================================================================
sop_snapshot_to_master_year <- function(snapshot_year) {
  switch(cfg$sop_year_alignment,
         opening = snapshot_year,
         closing = snapshot_year - 1L,
         stop("cfg$sop_year_alignment must be 'opening' or 'closing'.",
              call. = FALSE))
}

join_sop_to_master <- function(master, sop_panel, crosswalk) {
  # Prefer the master's own ons_code where present; fall back to the crosswalk.
  keyed <- master |>
    left_join(crosswalk |> select(la_code, ons_code), by = "la_code",
              suffix = c("", "_cw")) |>
    mutate(join_ons = if_else(!is.na(ons_code) & ons_code != "",
                              ons_code, ons_code_cw)) |>
    select(-ons_code_cw)
  
  sop_keyed <- sop_panel |>
    mutate(year = sop_snapshot_to_master_year(snapshot_year))
  
  joined <- keyed |>
    left_join(sop_keyed |> select(-sop_name, -abolished_year, -snapshot_year),
              by = c("join_ons" = "ons_code", "year")) |>
    mutate(sop_source = cfg$sop_source_label)
  
  coverage <- joined |>
    group_by(year) |>
    summarise(authorities = n(),
              with_key = sum(!is.na(join_ons)),
              with_sop = sum(!is.na(sop_n_total)), .groups = "drop") |>
    mutate(pct_matched = round(100 * with_sop / authorities, 1))
  message("")
  message("  Join coverage by year:")
  print(coverage, n = Inf)
  
  gaps <- joined |>
    filter(!is.na(join_ons), is.na(sop_n_total)) |>
    distinct(year, la_code, la_name, join_ons)
  if (nrow(gaps) > 0) {
    out_path <- file.path(cfg$dir_output, "qa_sop_join_gaps.csv")
    write_csv(gaps, out_path)
    message("  ", nrow(gaps), " authority-year(s) have a key but no SOP row. ",
            "Written to ", out_path)
  }
  
  joined
}

# =============================================================================
# RUNNER
# =============================================================================
run_stock_of_properties <- function() {
  message(strrep("-", 74))
  message("STOCK OF PROPERTIES -> MASTER TABLE (full series)")
  message(strrep("-", 74))
  
  master <- build_master_table()
  
  message("")
  message("[1] Reading sector files (wide panel, 2011-2025)")
  sop_panel <- load_sop_sectors()
  message("  ", nrow(sop_panel), " authority-year row(s), ",
          n_distinct(sop_panel$ons_code), " authorities, ",
          min(sop_panel$snapshot_year), "-", max(sop_panel$snapshot_year), ".")
  
  message("")
  message("[2] Reading change-in-stock file")
  sop_change <- load_sop_change()
  message("  ", nrow(sop_change), " authority row(s), period ",
          first(sop_change$sop_change_period), ".")
  
  message("")
  message("[3] Building la_code -> ons_code crosswalk")
  crosswalk <- build_la_ons_crosswalk(
    master, sop_panel |> distinct(ons_code, sop_name)
  )
  
  message("")
  message("[4] Joining to master (", cfg$sop_year_alignment, "-stock alignment)")
  joined <- join_sop_to_master(master, sop_panel, crosswalk)
  
  write_csv(joined, file.path(cfg$dir_output, "master_with_sop.csv"))
  write_csv(sop_panel, file.path(cfg$dir_output, "sop_panel_by_authority.csv"))
  write_csv(sop_change, file.path(cfg$dir_output, "sop_change_2023_2025.csv"))
  write_csv(crosswalk, file.path(cfg$dir_output, "la_code_ons_crosswalk.csv"))
  
  message("")
  message(strrep("=", 74))
  message("Written: ", cfg$dir_output, "/master_with_sop.csv")
  message("         ", cfg$dir_output, "/sop_panel_by_authority.csv")
  message("         ", cfg$dir_output, "/sop_change_2023_2025.csv")
  message("         ", cfg$dir_output, "/la_code_ons_crosswalk.csv")
  message("Source:  ", cfg$sop_source_label)
  message(strrep("=", 74))
  
  invisible(list(master = master, sop_panel = sop_panel,
                 sop_change = sop_change, crosswalk = crosswalk,
                 joined = joined))
}

res_sop <- run_stock_of_properties()

View(res_sop$joined)      # master + SOP columns, full series
View(res_sop$sop_panel)   # SOP alone, authority-year, 2011-2025
View(res_sop$crosswalk)   # la_code -> ons_code, with source layer
View(res_sop$sop_change)  # the 2023-25 change table


