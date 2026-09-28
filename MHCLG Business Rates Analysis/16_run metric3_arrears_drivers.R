# 16_metric3_arrears_drivers.R ---------------------------------------------------
# Sharper tests of what sector data can say about arrears, built after 32-37
# showed a same-year sector-share-vs-arrears cross-section is too blunt an
# instrument: NNDR3 arrears has several documented drivers (insolvency, misuse
# of relief, appeals, misreporting), of which sector composition proxies only
# one, and Stock of Properties counts are structurally sticky year to year.
#
# Two designs:
#   - LAGGED: does stock shrinkage in year t predict arrears deteriorating in
#     t+1, rather than comparing authorities to each other in one year.
#   - DIFFERENCED: does a CHANGE in sector share associate with a CHANGE in
#     arrears rate, rather than comparing LEVELS across authorities. Nets out
#     whatever is structurally different between authorities (urban/rural mix,
#     tax base size) and tests what actually moved.
#
# Continues the chart numbering from 15_metric3_sector.R (last used: 37) -
# starts at 38 by instruction. Nothing already produced (32-37) is touched.
#
# QA CALLOUT: "stock change" is a proxy for deletions, not deletions itself ---
# SOP8.1 (the true insertions/deletions breakdown) is a single cumulative
# period, 1 April 2023 to 31 March 2025 (see load_sop_change() in file 14), not
# an annual series, so it cannot be lagged year by year. Year-on-year change in
# TOTAL STOCK COUNT is used instead - it nets insertions against deletions
# rather than isolating deletions, so it understates the true churn signal, but
# it is the only measure available at annual frequency across the full series.
#
# QA CALLOUT: COVID-affected transitions excluded from both designs -----------
# Net rates payable cratered in 2020-21 as retail/hospitality/leisure relief
# expanded, which mechanically moves the arrears-rate denominator independent
# of anything behavioural. Any year-pair touching a COVID-affected year
# (cfg$covid_years) is dropped rather than left in to distort the correlation.
#
# QA CALLOUT: SOP join here does NOT filter to live authorities ---------------
# 15_metric3_sector.R's sector_national() filters to abolished_year == "[z]"
# because it sums ACROSS authorities nationally, and an abolished authority and
# its back-cast successor both carry data for the same year (double-counting -
# see the QA callout in that file). This file joins PER AUTHORITY, one la_code
# to one ons_code via the crosswalk, so there is no double-counting risk, and
# filtering to live-only would silently drop the 12 pre-2019 Dorset/Bucks
# authorities from both designs instead of correctly joining them to their own
# (abolished) ons_code's historical series.
#
# Correlation: Pearson throughout, per the earlier instruction to standardise
# on it across this body of work. Distributions here are as skewed as anywhere
# else in this file (City of London, Westminster) - read the scatter, not just
# the coefficient.
#
# Depends on: 00_config.R, 01_init.R, 02_master_table.R, 10_metric2.R,
# 12_metric3.R, 14_stock_of_properties.R, 15_metric3_sector.R (reuses
# sector_caption(), cfg$sector_levels, cfg$sector_labels).

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("10_metric2.R")
source("12_metric3.R")
source("14_stock_of_properties.R")
source("15_run_sector_plots.R")

# =============================================================================
# DATA
# =============================================================================
# Joins the canonical M2/M3 base (correct field names, covid_affected already
# computed) to SOP stock and sector counts, reusing the crosswalk file 14
# already built and validated rather than rebuilding it.
build_arrears_drivers_base <- function() {
  if (!exists("res_sop")) {
    stop("res_sop not found - source 14_stock_of_properties.R first.",
         call. = FALSE)
  }
  base <- build_metrics23_base()
  crosswalk <- res_sop$crosswalk
  
  keyed <- base |>
    left_join(crosswalk |> select(la_code, ons_code), by = "la_code",
              suffix = c("", "_cw")) |>
    mutate(join_ons = if_else(!is.na(ons_code) & ons_code != "",
                              ons_code, ons_code_cw)) |>
    select(-ons_code_cw)
  
  sop_cols <- c("sop_n_total", paste0("sop_n_", cfg$sector_levels))
  sop_vals <- res_sop$sop_panel |>
    select(ons_code, year = snapshot_year, all_of(sop_cols))
  
  keyed |>
    left_join(sop_vals, by = c("join_ons" = "ons_code", "year")) |>
    mutate(arrears_share = if_else(!is.na(net_rates_payable) &
                                     net_rates_payable > 0,
                                   arrears / net_rates_payable, NA_real_))
}

# --- A. Lagged: stock shrinkage in year t -> arrears change t to t+1 --------

build_lagged_data <- function(drivers_base) {
  drivers_base |>
    arrange(la_code, year) |>
    group_by(la_code) |>
    mutate(
      stock_growth = (sop_n_total - lag(sop_n_total)) / lag(sop_n_total),
      arrears_share_next = lead(arrears_share),
      covid_pair = covid_affected | lead(covid_affected)
    ) |>
    ungroup() |>
    filter(!is.na(stock_growth), !is.na(arrears_share_next),
           !is.na(arrears_share), !covid_pair) |>
    mutate(arrears_change_next = arrears_share_next - arrears_share)
}

# 38. Lagged design - see the file header for why total stock change stands
# in for true deletions here.
chart_m3_lag_stock_arrears <- function(lagged) {
  r <- suppressWarnings(cor(lagged$stock_growth, lagged$arrears_change_next,
                            method = "pearson", use = "complete.obs"))
  
  ggplot(lagged, aes(stock_growth, arrears_change_next)) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    geom_vline(xintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    geom_point(alpha = 0.3, size = 1.6, colour = mhclg_col$metric3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linewidth = 0.7, colour = mhclg_col$highlight) +
    scale_x_continuous(labels = label_prop(1)) +
    scale_y_continuous(labels = label_prop(1)) +
    labs(
      title = "Does stock shrinkage predict arrears deteriorating the following year?",
      subtitle = paste0("Every authority-year transition, ",
                        label_fy(min(lagged$year)), " to ",
                        label_fy(max(lagged$year)),
                        ". Pearson r = ", sprintf("%.2f", r),
                        ". COVID-affected transitions excluded."),
      x = "Year-on-year change in total property stock (year t)",
      y = "Change in arrears rate, year t to t+1",
      caption = sector_caption(
        paste("Stock change is a proxy for net churn, not true deletions -",
              "see the QA callout at the top of this file. Association only.")
      )
    ) +
    theme_mhclg(grid = "both")
}

# --- B. Differenced: Δ sector share vs Δ arrears rate, same period ----------

build_diff_data <- function(drivers_base, sector) {
  col <- paste0("sop_n_", sector)
  drivers_base |>
    filter(!is.na(.data[[col]]), !is.na(sop_n_total), sop_n_total > 0) |>
    arrange(la_code, year) |>
    group_by(la_code) |>
    mutate(
      sector_share = .data[[col]] / sop_n_total,
      d_sector_share = sector_share - lag(sector_share),
      d_arrears_share = arrears_share - lag(arrears_share),
      covid_pair = covid_affected | lag(covid_affected)
    ) |>
    ungroup() |>
    filter(!is.na(d_sector_share), !is.na(d_arrears_share), !covid_pair)
}

# 39-40. Differenced design, one call per sector (retail -> 39, other -> 40).
chart_m3_diff_sector_arrears <- function(diff_data, sector) {
  r <- suppressWarnings(cor(diff_data$d_sector_share, diff_data$d_arrears_share,
                            method = "pearson", use = "complete.obs"))
  
  ggplot(diff_data, aes(d_sector_share, d_arrears_share)) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    geom_vline(xintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    geom_point(alpha = 0.3, size = 1.6, colour = mhclg_col$metric3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linewidth = 0.7, colour = mhclg_col$highlight) +
    scale_x_continuous(labels = label_prop(1)) +
    scale_y_continuous(labels = label_prop(1)) +
    labs(
      title = paste0("Change in ", cfg$sector_labels[[sector]],
                     " share against change in arrears rate"),
      subtitle = paste0("Year-on-year differences, every authority-year, ",
                        label_fy(min(diff_data$year)), " to ",
                        label_fy(max(diff_data$year)),
                        ". Pearson r = ", sprintf("%.2f", r),
                        ". COVID-affected transitions excluded."),
      x = paste0("Change in ", cfg$sector_labels[[sector]], " share of stock"),
      y = "Change in arrears rate",
      caption = sector_caption(
        paste("Differenced to net out fixed authority characteristics",
              "(urban/rural mix, tax base size) that a levels comparison",
              "cannot separate from what actually moved.")
      )
    ) +
    theme_mhclg(grid = "both")
}

# =============================================================================
# RUNNER
# =============================================================================
run_metric3_arrears_drivers <- function() {
  message(strrep("-", 74))
  message("METRIC 3 DRIVERS: LAGGED AND DIFFERENCED DESIGNS")
  message(strrep("-", 74))
  
  drivers_base <- build_arrears_drivers_base()
  
  message("")
  message("[1] Lagged: stock growth (t) vs arrears change (t -> t+1)")
  lagged <- build_lagged_data(drivers_base)
  message("  ", nrow(lagged), " authority-year transition(s), COVID excluded.")
  chart_lag <- chart_m3_lag_stock_arrears(lagged)
  
  message("")
  message("[2] Differenced: change in sector share vs change in arrears rate")
  diff_retail <- build_diff_data(drivers_base, "retail")
  diff_other  <- build_diff_data(drivers_base, "other")
  message("  retail: ", nrow(diff_retail), " transition(s) | other: ",
          nrow(diff_other), " transition(s).")
  chart_diff_retail <- chart_m3_diff_sector_arrears(diff_retail, "retail")
  chart_diff_other  <- chart_m3_diff_sector_arrears(diff_other, "other")
  
  charts <- list(
    m3_lag_stock_arrears   = chart_lag,
    m3_diff_retail_arrears = chart_diff_retail,
    m3_diff_other_arrears  = chart_diff_other
  )
  
  files <- c(
    m3_lag_stock_arrears   = "38_metric3_lag_stock_arrears.png",
    m3_diff_retail_arrears = "39_metric3_diff_retail_arrears.png",
    m3_diff_other_arrears  = "40_metric3_diff_other_arrears.png"
  )
  walk(names(files), \(nm) {
    ggsave(file.path(cfg$dir_charts, files[[nm]]), charts[[nm]],
           width = 9, height = 5.5, dpi = 300, bg = "white")
  })
  
  message("")
  message(strrep("=", 74))
  message("Charts written to ", cfg$dir_charts, "/: ",
          paste(files, collapse = ", "))
  message("Numbering continues from 15_metric3_sector.R (last used: 37).")
  message("Nothing in 32-37 was modified or overwritten.")
  message(strrep("=", 74))
  
  invisible(list(drivers_base = drivers_base, lagged = lagged,
                 diff_retail = diff_retail, diff_other = diff_other,
                 charts = charts))
}

res_m3_drivers <- run_metric3_arrears_drivers()