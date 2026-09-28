# 01_init.R --------------------------------------------------------------------
# Source acquisition, metadata checks, 2025-26 extraction, England validation,
# September CPI preparation and ONS LAD boundary loading.

# File inventory ---------------------------------------------------------------

build_file_map <- function(dir = cfg$dir_data,
                           require_complete = cfg$require_complete_panel) {
  files <- list.files(
    dir,
    pattern = "^nndr3_[0-9]{2}_[0-9]{2}\\.csv$",
    full.names = TRUE
  )
  
  if (length(files) == 0) stop("No NNDR3 values files found in ", dir, ".", call. = FALSE)
  
  years <- 2000L + as.integer(
    str_match(basename(files), "^nndr3_(\\d{2})_")[, 2]
  )
  keep <- years >= cfg$year_min & years <= cfg$year_max
  out <- setNames(as.list(files[keep]), years[keep])
  missing <- setdiff(cfg$year_min:cfg$year_max, as.integer(names(out)))
  
  if (length(missing) > 0) {
    warning(
      "Missing panel years: ", paste(missing, collapse = ", "),
      ". Cross-sectional outputs may run; trend and persistence outputs must remain disabled.",
      call. = FALSE
    )
    if (require_complete) stop("Complete panel required for this run.", call. = FALSE)
  }
  
  message("Years available: ", paste(sort(names(out)), collapse = ", "))
  out
}

required_paths <- function() {
  paths <- c(
    values = file.path(cfg$dir_data, cfg$values_file),
    metadata = file.path(cfg$dir_data, cfg$metadata_file)
  )
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Missing required file(s): ", paste(unname(missing), collapse = ", "), call. = FALSE)
  }
  paths
}

# CPI local source -------------------------------------------------------------
# The ONS D7BT CSV is stored at data/series-260826.csv. The function name
# fetch_cpi_data() is retained so 04_run_metric1.R does not need amendment.

fetch_cpi_data <- function(overwrite = FALSE) {
  path <- file.path(cfg$dir_data, cfg$cpi_file)
  
  if (!file.exists(path)) {
    stop(
      "Local CPI file not found: ", path,
      ". Save series-260826.csv in the data folder.",
      call. = FALSE
    )
  }
  
  if (file.info(path)$size == 0) {
    stop("Local CPI file is empty: ", path, call. = FALSE)
  }
  
  message("Using local CPI file: ", path)
  path
}

read_ons_cpi <- function(path) {
  raw <- read_csv(
    path,
    col_names = c("period", "value"),
    col_types = cols(period = col_character(), value = col_character()),
    show_col_types = FALSE,
    trim_ws = TRUE
  )
  
  monthly <- raw |>
    mutate(period = str_squish(period), value = str_squish(value)) |>
    filter(str_detect(
      period,
      paste0("^\\d{4}\\s+", "(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)$")
    )) |>
    separate(period, into = c("year", "month_name"), sep = "\\s+", remove = FALSE) |>
    mutate(
      year = as.integer(year),
      month = match(month_name, str_to_upper(month.abb)),
      cpi_index = suppressWarnings(as.numeric(value)),
      date = as.Date(sprintf("%04d-%02d-01", year, month))
    ) |>
    filter(!is.na(year), !is.na(month), !is.na(cpi_index)) |>
    select(period, date, year, month, cpi_index)
  
  if (nrow(monthly) == 0) {
    stop(
      "No monthly CPI observations found in ", path,
      ". Expected rows such as '2025 SEP'.",
      call. = FALSE
    )
  }
  
  monthly
}

# QA CALLOUT: how the deflator is constructed ----------------------------------
# cpi_deflator = reference_cpi / september_cpi, i.e. September of the price-basis
# year divided by September of the observation year. Values BEFORE the price
# basis year get a deflator above 1 and are scaled UP; the price-basis year
# itself gets exactly 1.0. If a deflator comes out below 1 for a historic year,
# the reference year has been set wrongly.
#
# The warning below fires when a panel year has no September CPI. It is a
# warning rather than an error so the 2025-26 pilot can run before the full
# 2013-14 onwards panel exists, but any year it names CANNOT carry a real-terms
# figure. Check the console for it before quoting a real series. See caveat C20.
build_september_cpi <- function(cpi, reference_year = cfg$cpi_price_year) {
  september <- cpi |>
    filter(month == cfg$cpi_month) |>
    select(year, september_cpi = cpi_index) |>
    distinct(year, .keep_all = TRUE)
  
  missing_years <- setdiff(cfg$year_min:cfg$year_max, september$year)
  if (length(missing_years) > 0) {
    warning(
      "September CPI is missing for: ",
      paste(missing_years, collapse = ", "),
      call. = FALSE
    )
  }
  
  reference_cpi <- september |>
    filter(year == reference_year) |>
    pull(september_cpi)
  
  if (length(reference_cpi) != 1) {
    stop(
      "Could not identify exactly one September ", reference_year,
      " CPI observation.",
      call. = FALSE
    )
  }
  
  september |>
    mutate(
      reference_year = reference_year,
      reference_cpi = reference_cpi,
      cpi_deflator = reference_cpi / september_cpi,
      price_basis = paste0("September ", reference_year, " prices"),
      cpi_series = cfg$cpi_series_id,
      cpi_source = cfg$cpi_source_label,
      cpi_source_file = cfg$cpi_file
    )
}

# Metadata ---------------------------------------------------------------------

read_nndr3_metadata <- function(path) {
  metadata <- read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  if (ncol(metadata) < 2) {
    stop("Metadata must contain at least a variable and description column.", call. = FALSE)
  }
  names(metadata)[1:2] <- c("variable", "description")
  metadata |>
    mutate(variable = str_trim(as.character(variable)),
           description = as.character(description))
}

validate_variable_map <- function(metadata, variable_map) {
  mapped <- unname(variable_map)
  absent <- setdiff(mapped, metadata$variable)
  if (length(absent) > 0) {
    stop("Mapped variables absent from metadata: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  metadata |>
    filter(variable %in% mapped) |>
    mutate(harmonised_name = names(variable_map)[match(variable, mapped)]) |>
    select(harmonised_name, variable, description)
}

check_appeals_exclusion <- function(variable_map) {
  overlap <- intersect(unname(variable_map), appeals_variables)
  if (length(overlap) > 0) {
    stop("Appeals variables entered Metric 1 mapping: ",
         paste(overlap, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

discover_variables <- function(metadata_path, patterns = search_patterns) {
  meta <- read_nndr3_metadata(metadata_path)
  imap(patterns, \(pattern, label) {
    meta |>
      filter(str_detect(str_to_lower(coalesce(description, "")), pattern)) |>
      transmute(pattern = label, variable, description)
  }) |>
    list_rbind()
}

# Values and extraction --------------------------------------------------------

read_nndr3_values <- function(path) {
  read_csv(path, show_col_types = FALSE, name_repair = "minimal",
           na = c("", "NA", "N/A", ".."))
}

check_required_columns <- function(data, variable_map) {
  absent <- setdiff(unname(variable_map), names(data))
  if (length(absent) > 0) {
    stop("Required columns absent from values file: ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

check_total_suffix <- function(raw, year = cfg$analysis_year,
                               tolerance = cfg$suffix_validation_tolerance) {
  mapping <- integrity_var_map[[as.character(year)]]
  if (is.null(mapping) || !all(unname(mapping) %in% names(raw))) {
    warning("Year ", year, ": BA plus designated-area integrity check not run.",
            call. = FALSE)
    return(tibble())
  }
  
  out <- raw |>
    transmute(
      la_code = str_trim(as.character(.data[[var_map[[as.character(year)]][["la_code"]]]])),
      ba = suppressWarnings(as.numeric(.data[[mapping[["wo_allowance_ba"]]]])),
      designated_area = suppressWarnings(as.numeric(.data[[mapping[["wo_allowance_da"]]]])),
      reported_total = suppressWarnings(as.numeric(.data[[mapping[["wo_allowance_total"]]]])),
      calculated_total = ba + designated_area,
      difference = calculated_total - reported_total
    ) |>
    filter(!is.na(difference), abs(difference) > tolerance)
  
  if (nrow(out) > 0) {
    write_csv(out, file.path(cfg$dir_output, paste0("qa_total_suffix_", year, ".csv")))
    stop("Year ", year, ": component write-offs do not reconcile to total.", call. = FALSE)
  }
  out
}

harmonise_nndr3_year <- function(raw, year) {
  key <- as.character(year)
  required <- var_map[[key]]
  if (is.null(required)) {
    stop("No variable map for ", year,
         ". Validate metadata and add var_map[[\"", year, "\"]].", call. = FALSE)
  }
  
  optional <- optional_var_map[[key]]
  if (is.null(optional)) optional <- character()
  optional <- optional[unname(optional) %in% names(raw)]
  selected <- c(required, optional)
  
  raw |>
    select(all_of(unname(selected))) |>
    rename(!!!setNames(unname(selected), names(selected))) |>
    mutate(
      la_code = str_trim(as.character(la_code)),
      la_name = str_squish(as.character(la_name)),
      across(c(net_rates_payable, wo_allowance, wo_excess),
             \(x) suppressWarnings(as.numeric(x))),
      year = as.integer(year),
      financial_year = label_fy(year),
      .before = 1
    ) |>
    filter(!is.na(la_code), la_code != "")
}

# Compatibility wrapper for current pilot code.
harmonise_2025 <- function(raw) harmonise_nndr3_year(raw, 2025L)

split_england_and_las <- function(data) {
  england <- data |> filter(la_code %in% cfg$aggregate_codes)
  las <- data |> filter(!la_code %in% cfg$aggregate_codes)
  if (nrow(england) != 1) {
    stop("Expected one England row; found ", nrow(england), ".", call. = FALSE)
  }
  if (nrow(las) == 0) stop("No billing-authority rows remain.", call. = FALSE)
  list(england = england, las = las)
}

# QA CALLOUT: sign convention, applied exactly once -----------------------------
# NNDR3 files lines 3 and 4 as deductions, so a plain sum of the raw columns
# returns a negative England total. cfg$sign_flip = TRUE multiplies both by -1
# so that downstream a POSITIVE value means a write-off and a NEGATIVE value
# means a write-back. This is the ONLY place the sign is touched - do not flip
# again in 02_metric1.R or in the charts. If the England total comes out
# negative, check whether this ran, not whether the data is wrong.
apply_writeoff_sign <- function(data) {
  multiplier <- if (isTRUE(cfg$sign_flip)) -1 else 1
  data |>
    mutate(wo_allowance = wo_allowance * multiplier,
           wo_excess = wo_excess * multiplier)
}

check_required_values <- function(las) {
  failures <- las |>
    filter(is.na(wo_allowance) | is.na(wo_excess) | is.na(net_rates_payable)) |>
    select(year, la_code, la_name, wo_allowance, wo_excess,
           net_rates_payable, any_of("form_status"))
  if (nrow(failures) > 0) {
    write_csv(failures, file.path(cfg$dir_output, "qa_missing_required_values.csv"))
    stop(nrow(failures), " authority row(s) have missing required values.", call. = FALSE)
  }
  invisible(TRUE)
}

validate_against_england <- function(
    las,
    england,
    tolerance = cfg$england_validation_tolerance
) {
  comparison <- tibble(
    measure = c("write-offs charged to allowance",
                "write-offs/write-backs in excess",
                "net write-offs", "net rates payable"),
    la_sum = c(sum(las$wo_allowance), sum(las$wo_excess),
               sum(las$wo_allowance + las$wo_excess),
               sum(las$net_rates_payable)),
    england_value = c(england$wo_allowance, england$wo_excess,
                      england$wo_allowance + england$wo_excess,
                      england$net_rates_payable)
  ) |>
    mutate(difference = la_sum - england_value,
           passes = abs(difference) <= tolerance)
  
  write_csv(comparison, file.path(cfg$dir_output, "qa_england_validation.csv"))
  if (any(!comparison$passes)) {
    print(comparison)
    stop("England validation gate failed.", call. = FALSE)
  }
  message("England validation gate passed.")
  comparison
}

load_metric1_source <- function() {
  paths <- required_paths()
  metadata <- read_nndr3_metadata(paths[["metadata"]])
  inventory <- validate_variable_map(metadata, var_map_2025)
  check_appeals_exclusion(var_map_2025)
  write_csv(inventory, file.path(cfg$dir_output, "metric1_variable_inventory.csv"))
  
  raw <- read_nndr3_values(paths[["values"]])
  check_required_columns(raw, var_map_2025)
  check_total_suffix(raw, cfg$analysis_year)
  
  before <- nrow(raw)
  harmonised <- harmonise_2025(raw)
  removed <- before - nrow(harmonised)
  if (removed > 0) message(removed, " blank-code row(s) excluded.")
  
  split <- split_england_and_las(harmonised)
  las <- apply_writeoff_sign(split$las)
  england <- apply_writeoff_sign(split$england)
  check_required_values(las)
  validation <- validate_against_england(las, england)
  
  list(las = las, england = england, metadata = metadata,
       variable_inventory = inventory, validation = validation)
}

# ONS LAD boundaries -----------------------------------------------------------

read_england_boundaries <- function(boundary_url = cfg$boundary_url) {
  if (str_detect(boundary_url, fixed("<a href")) ||
      str_detect(boundary_url, fixed("</a>"))) {
    stop("Boundary URL contains HTML markup. Check 00_config.R.", call. = FALSE)
  }
  
  boundaries <- read_sf(boundary_url, quiet = TRUE)
  required <- c(cfg$boundary_code, cfg$boundary_name)
  missing <- setdiff(required, names(boundaries))
  if (length(missing) > 0) {
    stop("ONS boundary data missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  boundaries |>
    rename(
      ons_code = all_of(cfg$boundary_code),
      authority_name = all_of(cfg$boundary_name)
    ) |>
    select(ons_code, authority_name, geometry) |>
    filter(str_starts(ons_code, "E")) |>
    st_make_valid()
}


# Panel ingestion ==============================================================
# Added for the multi-year build. The single-year path (load_metric1_source)
# is untouched and still works.

# QA CALLOUT: resolution is explicit and loud ----------------------------------
# Returns a named character vector mapping canonical field -> actual column name
# in this file. Halts on any unresolved required field, and halts if more than
# one candidate matches, because an ambiguous match means the candidate list is
# too loose, not that either column will do.
resolve_var_map <- function(raw, year, candidates, required = TRUE) {
  cols <- names(raw)
  resolved <- character()
  unresolved <- character()
  ambiguous <- character()
  
  for (field in names(candidates)) {
    hits <- intersect(candidates[[field]], cols)
    if (length(hits) == 1) {
      resolved[[field]] <- hits
    } else if (length(hits) > 1) {
      ambiguous[[field]] <- paste(hits, collapse = " / ")
    } else {
      unresolved <- c(unresolved, field)
    }
  }
  
  if (length(ambiguous) > 0) {
    stop("Year ", year, ": ambiguous column match for ",
         paste(names(ambiguous), "->", unlist(ambiguous), collapse = "; "),
         ". Tighten cfg$var_candidates.", call. = FALSE)
  }
  
  if (required && length(unresolved) > 0) {
    stop("Year ", year, ": could not resolve required field(s): ",
         paste(unresolved, collapse = ", "),
         ". Run probe_nndr3_columns() and extend cfg$var_candidates.",
         call. = FALSE)
  }
  
  if (!required && length(unresolved) > 0) {
    message("  Year ", year, ": optional field(s) absent - ",
            paste(unresolved, collapse = ", "))
  }
  
  resolved
}

# Diagnostic. Reports which candidate matched in each file WITHOUT loading the
# data. Run this first whenever a new year is added.
probe_nndr3_columns <- function(file_map = build_file_map()) {
  imap(file_map, \(path, year) {
    hdr <- names(read_csv(path, n_max = 0, show_col_types = FALSE,
                          name_repair = "minimal"))
    all_fields <- c(cfg$var_candidates, cfg$optional_candidates)
    tibble(
      year = as.integer(year),
      field = names(all_fields),
      resolved = map_chr(all_fields, \(cands) {
        hit <- intersect(cands, hdr)
        if (length(hit) == 0) NA_character_ else paste(hit, collapse = " / ")
      }),
      required = names(all_fields) %in% names(cfg$var_candidates)
    )
  }) |>
    list_rbind() |>
    arrange(year, desc(required), field)
}

# Load one year end to end, returning LA rows, the England row and the
# validation comparison for that year.
# QA CALLOUT: duplicate column names -------------------------------------------
# read_nndr3_values() uses name_repair = "minimal", which PRESERVES duplicate
# column names rather than renaming them. That is deliberate - a silent rename
# to netrates_tot...2 would let the wrong column be selected without anyone
# noticing. The cost is that .data[[x]] cannot index a frame with duplicates,
# which is the "Can't transform a data frame with duplicate names" error.
#
# This function resolves the situation explicitly:
#   - if every copy of a duplicated name holds identical values, keep the first
#     and say so;
#   - if the copies DIFFER and the name is one Metric 1 needs, HALT - picking a
#     copy would be a guess about which series is the real one;
#   - if the copies differ but the name is unused, drop them all and name them,
#     so nothing ambiguous survives into the panel.
deduplicate_columns <- function(raw, year, needed = character()) {
  nms <- names(raw)
  dup_names <- unique(nms[duplicated(nms)])
  if (length(dup_names) == 0) return(list(data = raw, duplicates = character()))
  
  message("  Year ", year, ": ", length(dup_names),
          " duplicated column name(s) found.")
  
  keep <- rep(TRUE, length(nms))
  conflicting_needed <- character()
  
  for (nm in dup_names) {
    pos <- which(nms == nm)
    cols <- map(pos, \(i) raw[[i]])
    identical_copies <- all(map_lgl(cols[-1], \(x) identical(x, cols[[1]])))
    
    if (identical_copies) {
      message("    '", nm, "' x", length(pos), " - copies identical, keeping first.")
      keep[pos[-1]] <- FALSE
    } else if (nm %in% needed) {
      conflicting_needed <- c(conflicting_needed, nm)
    } else {
      message("    '", nm, "' x", length(pos),
              " - copies DIFFER but column is unused; all copies dropped.")
      keep[pos] <- FALSE
    }
  }
  
  if (length(conflicting_needed) > 0) {
    stop("Year ", year, ": required column(s) appear more than once with ",
         "different values: ", paste(conflicting_needed, collapse = ", "),
         ". Inspect the source file before proceeding - do not guess which ",
         "copy is correct.", call. = FALSE)
  }
  
  list(data = raw[, keep, drop = FALSE], duplicates = dup_names)
}

# Load one year end to end, returning LA rows, the England row and the
# validation comparison for that year.
load_nndr3_year <- function(path, year) {
  year <- as.integer(year)
  message("Year ", year, ": reading ", basename(path))
  
  raw <- read_nndr3_values(path)
  
  # QA CALLOUT: a metadata dictionary is not a values file -----------------------
  # The *_meta.csv and the values CSV sit next to each other on the same GOV.UK
  # page with similar names. Saving the dictionary under the values filename
  # otherwise surfaces as five unresolved fields rather than the real cause.
  if (identical(tolower(names(raw)[1:2]), c("variable", "description"))) {
    stop("Year ", year, ": ", basename(path),
         " is a metadata dictionary (Variable/Description), not a values file. ",
         "Rename it to *_meta.csv and download the consolidated data.",
         call. = FALSE)
  }
  
  needed_names <- unlist(c(cfg$var_candidates, cfg$integrity_candidates))
  dedup <- deduplicate_columns(raw, year, needed = needed_names)
  raw <- dedup$data
  
  required <- resolve_var_map(raw, year, cfg$var_candidates, required = TRUE)
  optional <- resolve_var_map(raw, year, cfg$optional_candidates, required = FALSE)
  integrity <- resolve_var_map(raw, year, cfg$integrity_candidates, required = FALSE)
  
  # QA CALLOUT: metadata validation only runs where a *_meta.csv exists.
  # Disabled features announce themselves rather than silently passing.
  meta_path <- file.path(cfg$dir_data,
                         sub("\\.csv$", "_meta.csv", basename(path)))
  if (year %in% cfg$metadata_years && file.exists(meta_path)) {
    validate_variable_map(read_nndr3_metadata(meta_path), required)
    check_appeals_exclusion(required)
    metadata_checked <- TRUE
  } else {
    message("  Year ", year,
            ": no metadata file - variable map NOT validated against metadata.")
    metadata_checked <- FALSE
  }
  
  # Component-to-total integrity check, where the component columns exist.
  suffix_failures <- 0L
  if (length(integrity) == 3) {
    chk <- raw |>
      transmute(
        la_code = str_trim(as.character(.data[[required[["la_code"]]]])),
        ba = suppressWarnings(as.numeric(.data[[integrity[["wo_allowance_ba"]]]])),
        da = suppressWarnings(as.numeric(.data[[integrity[["wo_allowance_da"]]]])),
        reported = suppressWarnings(as.numeric(.data[[integrity[["wo_allowance_total"]]]])),
        difference = (ba + da) - reported
      ) |>
      filter(!is.na(difference),
             abs(difference) > cfg$suffix_validation_tolerance)
    suffix_failures <- nrow(chk)
    if (suffix_failures > 0) {
      write_csv(chk, file.path(cfg$dir_output,
                               paste0("qa_total_suffix_", year, ".csv")))
      stop("Year ", year, ": component write-offs do not reconcile to total ",
           "for ", suffix_failures, " authority row(s).", call. = FALSE)
    }
  } else {
    message("  Year ", year, ": BA plus designated-area integrity check not run.")
  }
  
  selected <- c(required, optional)
  before <- nrow(raw)
  
  harmonised <- raw |>
    select(all_of(unname(selected))) |>
    rename(!!!setNames(unname(selected), names(selected))) |>
    mutate(
      la_code = str_trim(as.character(la_code)),
      la_name = str_squish(as.character(la_name)),
      across(c(net_rates_payable, wo_allowance, wo_excess),
             \(x) suppressWarnings(as.numeric(x))),
      year = year,
      financial_year = label_fy(year),
      source_file = basename(path),
      .before = 1
    ) |>
    filter(!is.na(la_code), la_code != "")
  
  blank_rows <- before - nrow(harmonised)
  if (blank_rows > 0) message("  ", blank_rows, " blank-code row(s) excluded.")
  
  split <- split_england_and_las(harmonised)
  las <- apply_writeoff_sign(split$las)
  england <- apply_writeoff_sign(split$england)
  check_required_values(las)
  
  validation <- validate_against_england(las, england) |>
    mutate(year = year, .before = 1)
  
  list(
    las = las,
    england = england,
    validation = validation,
    resolved = tibble(year = year,
                      field = names(selected),
                      column = unname(selected)),
    diagnostics = tibble(
      year = year,
      source_file = basename(path),
      n_authorities = nrow(las),
      blank_rows_excluded = blank_rows,
      metadata_checked = metadata_checked,
      suffix_failures = suffix_failures,
      duplicate_columns = if (length(dedup$duplicates) == 0) {
        NA_character_
      } else {
        paste(dedup$duplicates, collapse = "; ")
      },
      optional_absent = if (length(optional) == length(cfg$optional_candidates)) {
        NA_character_
      } else {
        paste(setdiff(names(cfg$optional_candidates), names(optional)),
              collapse = "; ")
      }
    )
  )
}

# Stack every available year into one panel.
build_panel <- function(file_map = build_file_map()) {
  years <- sort(as.integer(names(file_map)))
  years <- years[years >= cfg$panel_year_min & years <= cfg$panel_year_max]
  
  if (length(years) == 0) {
    stop("No files fall inside the panel window ",
         cfg$panel_year_min, " to ", cfg$panel_year_max, ".", call. = FALSE)
  }
  
  loaded <- map(years, \(y) load_nndr3_year(file_map[[as.character(y)]], y))
  names(loaded) <- as.character(years)
  
  list(
    las = map(loaded, "las") |> list_rbind(),
    england = map(loaded, "england") |> list_rbind(),
    validation = map(loaded, "validation") |> list_rbind(),
    resolved = map(loaded, "resolved") |> list_rbind(),
    diagnostics = map(loaded, "diagnostics") |> list_rbind(),
    years = years
  )
}