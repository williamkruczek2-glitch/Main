# 17_demography.R ---------------------------------------------------------------
# ONS Business Demography as NATIONAL CONTEXT for Metrics 1 and 2.
#
# Depends on: 00_config.R, 01_init.R, 10_metric2.R, 12_metric3.R.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("10_metric2.R")
source("12_metric3.R")

# --- Config -------------------------------------------------------------------

cfg$dir_demography <- file.path(cfg$dir_data, "demography")

# No "Source: " prefix and no trailing full stop - mhclg_caption() adds both.
cfg$demog_source_quarterly <- paste(
  "ONS, Business demography, quarterly, UK.",
  "Official statistics in development"
)

demog_caption <- function(extra = NULL, source = cfg$demog_source_quarterly) {
  mhclg_caption(
    extra = extra,
    source = source,
    scope = paste("Counts VAT and/or PAYE-registered enterprises, not rateable",
                  "hereditaments. Contextual only; does not reconcile to NNDR3.")
  )
}

# =============================================================================
# 1. LOADING
# =============================================================================
# CONFIRMED FILE STRUCTURE (from the actual ONS export, not assumed) -----------


cfg$demog_files <- c(
  births = "births_demography.csv",  # creations
  deaths = "deaths_demography.csv"   # closures
)

# [z] = not applicable, [c] = suppressed - same convention as parse_sop_count()
# in 14_stock_of_properties.R, though ONS demography has not shown either
# marker in practice; kept for safety since counts are disclosure-controlled.
parse_count <- function(x) {
  x <- str_trim(as.character(x))
  x <- str_replace_all(x, ",", "")
  x[str_detect(x, "^\\[.*\\]$")] <- NA_character_
  suppressWarnings(as.numeric(dplyr::na_if(x, "")))
}

read_demog_quarterly <- function(path, value_name) {
  if (!file.exists(path)) {
    stop("Demography file not found: ", path, "\nExpected in ",
         cfg$dir_demography, "/ - see cfg$demog_files.", call. = FALSE)
  }
  raw <- read_csv(path, skip = 3, show_col_types = FALSE,
                  name_repair = "minimal",
                  locale = locale(encoding = detect_file_encoding(path)),
                  col_types = cols(.default = col_character()))
  names(raw) <- str_trim(names(raw))
  # Trailing comma at the end of each CSV row (present in births_demography.csv,
  # not in deaths_demography.csv) creates a blank-named 19th column. transmute()
  # refuses to run on any dataframe with an NA or "" name, even on an untouched
  # column - same issue read_sop_raw() already guards against in file 14.
  raw <- raw[, names(raw) != "" & !is.na(names(raw)), drop = FALSE]
  
  required <- c("Quarter", "United Kingdom")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols) > 0) {
    message("  Columns present: ", paste(names(raw), collapse = " | "))
    stop("Demography file is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  out <- raw |>
    transmute(quarter_label = str_trim(Quarter),
              value = parse_count(`United Kingdom`)) |>
    filter(str_detect(quarter_label, "^Q[1-4] \\d{4}$")) |>
    mutate(cal_quarter = as.integer(str_sub(quarter_label, 2, 2)),
           cal_year = as.integer(str_sub(quarter_label, 4)))
  
  names(out)[names(out) == "value"] <- value_name
  out |> select(cal_year, cal_quarter, all_of(value_name))
}

# QA CALLOUT: creations and closures are completed INDEPENDENTLY below --------
# Checked directly against the actual uploaded files (set comparison of every
# (year, quarter) pair): both cover exactly Q1 2017-Q2 2026 with no gaps
# against each other. A full_join with independent per-series completeness is
# still the right design regardless - it costs nothing when the files agree,
# and if a future re-export of either file drops a quarter, this reports it by
# name instead of either halting every chart over one missing cell or silently
# producing a wrong total.
load_demography_quarterly <- function() {
  births <- read_demog_quarterly(
    file.path(cfg$dir_demography, cfg$demog_files[["births"]]), "creations")
  deaths <- read_demog_quarterly(
    file.path(cfg$dir_demography, cfg$demog_files[["deaths"]]), "closures")
  
  combined <- full_join(births, deaths, by = c("cal_year", "cal_quarter")) |>
    arrange(cal_year, cal_quarter)
  
  gaps <- combined |>
    filter(is.na(creations) | is.na(closures)) |>
    mutate(missing_from = case_when(
      is.na(creations) & is.na(closures) ~ "both",
      is.na(creations) ~ "births",
      TRUE ~ "deaths"
    ))
  if (nrow(gaps) > 0) {
    message("  NOTE: ", nrow(gaps), " quarter(s) missing from one file:")
    walk(seq_len(nrow(gaps)), \(i) message(
      "    Q", gaps$cal_quarter[i], " ", gaps$cal_year[i],
      " - missing from ", gaps$missing_from[i]
    ))
  } else {
    message("  Both files cover the same quarters - no gaps.")
  }
  
  combined
}

# QA CALLOUT: financial-year alignment, not calendar year ----------------------
# Every other chart in this pipeline runs on financial year (Apr Y to Mar Y+1,
# labelled year Y - see label_fy() in 00_config.R), so quarterly demography is
# aggregated the same way: FY year Y = Q2(Y) + Q3(Y) + Q4(Y) + Q1(Y+1). Data
# spans Q1 2017 to Q2 2026, so the earliest COMPLETE financial year is FY2017
# (Q2-Q4 2017 + Q1 2018) and the latest is FY2025 (Q2-Q4 2025 + Q1 2026) -
# FY2013-16 do not exist (no calendar-2016 data) and FY2026 is not yet complete
# (only Q2 2026 published). Verified against the actual files: creations and
# closures are both complete for FY2017-FY2025 (9 years each). Completed
# independently below regardless, so a gap in one series in a future data
# refresh cannot silently truncate the other.
aggregate_demog_to_fy <- function(quarterly) {
  fy_tagged <- quarterly |>
    mutate(year = if_else(cal_quarter == 1L, cal_year - 1L, cal_year))
  
  creations_fy <- fy_tagged |>
    filter(!is.na(creations)) |>
    group_by(year) |>
    summarise(n_quarters = n(), creations = sum(creations), .groups = "drop") |>
    filter(n_quarters == 4L) |>
    select(year, creations)
  
  closures_fy <- fy_tagged |>
    filter(!is.na(closures)) |>
    group_by(year) |>
    summarise(n_quarters = n(), closures = sum(closures), .groups = "drop") |>
    filter(n_quarters == 4L) |>
    select(year, closures)
  
  full_join(creations_fy, closures_fy, by = "year") |>
    mutate(financial_year = label_fy(year)) |>
    arrange(year)
}

# 2. CHARTS
# =============================================================================

# 41. National closures as backdrop to the M1 write-off series. Two panels
# rather than dual axes: the series are different units (enterprise counts vs
# GBP) and a shared axis would imply a comparability that C37 explicitly denies.
chart_demog_closures <- function(demog) {
  plot_data <- demog |> filter(!is.na(closures))
  if (nrow(plot_data) == 0) {
    stop("No closures data to plot.", call. = FALSE)
  }
  ggplot(plot_data, aes(year, closures)) +
    annotate_covid(cfg$covid_years) +
    geom_line(linewidth = 0.9, colour = mhclg_col$metric1) +
    geom_point(size = 1.8, colour = mhclg_col$metric1) +
    scale_x_continuous(breaks = sort(unique(plot_data$year)),
                       labels = label_fy(sort(unique(plot_data$year)))) +
    scale_y_continuous(labels = comma_format(),
                       expand = expansion(c(0.08, 0.12))) +
    labs(
      title = "Business closures, UK",
      subtitle = paste("The mechanism behind a write-off: a business closing",
                       "mid-liability. National context, not joined to the",
                       "authority panel."),
      x = NULL, y = NULL,
      caption = demog_caption()
    ) +
    theme_mhclg(grid = "y")
}

# 42. Creations against closures. Net churn is the forward-looking signal the
# allowance is conceptually trying to anticipate (M2).
chart_demog_creations_closures <- function(demog) {
  plot_data <- demog |>
    select(year, creations, closures) |>
    pivot_longer(c(creations, closures), names_to = "series",
                 values_to = "n") |>
    filter(!is.na(n)) |>
    mutate(series = factor(series, levels = c("creations", "closures"),
                           labels = c("Creations", "Closures")))
  if (nrow(plot_data) == 0) {
    stop("No creations/closures data to plot.", call. = FALSE)
  }
  ggplot(plot_data, aes(year, n, colour = series)) +
    annotate_covid(cfg$covid_years) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = c(Creations = unname(mhclg_pal[["teal"]]),
                                   Closures = unname(mhclg_pal[["plum"]]))) +
    scale_x_continuous(breaks = sort(unique(plot_data$year)),
                       labels = label_fy(sort(unique(plot_data$year)))) +
    scale_y_continuous(labels = comma_format(),
                       expand = expansion(c(0.08, 0.12))) +
    labs(
      title = "Business creations and closures, UK",
      subtitle = paste("Closures are forward-looking in the same way the",
                       "allowance is - both concern loss expected, not",
                       "realised."),
      x = NULL, y = NULL, colour = NULL,
      caption = demog_caption()
    ) +
    theme_mhclg(grid = "y") +
    theme(legend.position = "top")
}

# 43. Closures alongside the national allowance movement (M2). Indexed to the
# first common year so two different units can share one panel legitimately -
# this shows co-movement in shape only, and makes no claim about levels.
chart_demog_vs_allowance <- function(demog, national) {
  allowance <- national |>
    filter(!is.na(allowance_cb)) |>
    select(year, value = allowance_cb) |>
    mutate(series = "Allowance for non-collection")
  closures <- demog |>
    filter(!is.na(closures)) |>
    select(year, value = closures) |>
    mutate(series = "Business closures (UK)")
  
  common <- intersect(allowance$year, closures$year)
  if (length(common) < 3) {
    stop("Fewer than 3 overlapping years between the allowance series and ",
         "demography - cannot index a comparison.", call. = FALSE)
  }
  base_year <- min(common)
  
  plot_data <- bind_rows(allowance, closures) |>
    filter(year %in% common) |>
    group_by(series) |>
    mutate(index = 100 * value / value[year == base_year]) |>
    ungroup()
  
  ggplot(plot_data, aes(year, index, colour = series)) +
    annotate_covid(cfg$covid_years) +
    geom_hline(yintercept = 100, linewidth = 0.4,
               colour = mhclg_col$deemphasis) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_colour_manual(values = setNames(
      c(mhclg_col$metric2, unname(mhclg_pal[["lilac"]])),
      c("Allowance for non-collection", "Business closures (UK)")
    )) +
    scale_x_continuous(breaks = sort(unique(plot_data$year)),
                       labels = label_fy(sort(unique(plot_data$year)))) +
    labs(
      title = "Is provisioning tracking observable business failure?",
      subtitle = paste0("Indexed to ", label_fy(base_year),
                        " = 100. Shape comparison only - the two series are ",
                        "different units and different populations."),
      x = NULL, y = NULL, colour = NULL,
      caption = demog_caption(
        paste("Indexed because enterprise counts and GBP provision cannot",
              "share an axis. Co-movement is suggestive, not evidence of a",
              "causal link (C37).")
      )
    ) +
    theme_mhclg(grid = "y") +
    theme(legend.position = "top")
}

# =============================================================================
# RUNNER
# =============================================================================
run_demography <- function() {
  message(strrep("-", 74))
  message("BUSINESS DEMOGRAPHY (national context for M1 and M2)")
  message(strrep("-", 74))
  
  message("")
  message("[1] Reading quarterly births and deaths files")
  quarterly <- load_demography_quarterly()
  message("  ", nrow(quarterly), " quarter(s) read.")
  
  message("")
  message("[2] Aggregating to financial year (creations and closures ",
          "completed independently - see the QA callout above)")
  demog <- aggregate_demog_to_fy(quarterly)
  creations_years <- demog |> filter(!is.na(creations)) |> pull(year)
  closures_years  <- demog |> filter(!is.na(closures)) |> pull(year)
  message("  Creations: ", length(creations_years), " complete FY(s), ",
          min(creations_years), "-", max(creations_years), ".")
  message("  Closures:  ", length(closures_years), " complete FY(s), ",
          min(closures_years), "-", max(closures_years), ".")
  
  message("")
  message("[3] Building national M2 series for the comparison chart")
  base <- build_metrics23_base()
  national <- metrics23_national(base)
  
  message("")
  message("[4] Charts")
  charts <- list(
    demog_closures  = chart_demog_closures(demog),
    demog_both      = chart_demog_creations_closures(demog),
    demog_allowance = chart_demog_vs_allowance(demog, national)
  )
  
  files <- c(
    demog_closures  = "41_demography_closures.png",
    demog_both      = "42_demography_creations_closures.png",
    demog_allowance = "43_demography_vs_allowance.png"
  )
  walk(names(files), \(nm) {
    ggsave(file.path(cfg$dir_charts, files[[nm]]), charts[[nm]],
           width = 9, height = 5.5, dpi = 300, bg = "white")
  })
  
  write_csv(demog, file.path(cfg$dir_output, "demography_national.csv"))
  
  message("")
  message(strrep("=", 74))
  message("Charts written to ", cfg$dir_charts, "/: ",
          paste(files, collapse = ", "))
  message("Data:    ", cfg$dir_output, "/demography_national.csv")
  message("NATIONAL CONTEXT ONLY - not joined to the authority panel (C37).")
  message(strrep("=", 74))
  
  invisible(list(demog = demog, national = national, charts = charts))
}

res_demography <- run_demography()