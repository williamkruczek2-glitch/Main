# 10_metric2.R ------------------------------------------------------------------
# Metric 2 chart DEFINITIONS: change in allowance for non-collection, the
# forward-looking risk indicator. No runner here - see 08_run_metric2.R.
#
# Built from the master table (2013-14 to 2025-26) so Metric 2 shares one source with the
# other metrics. House style, annotation layer and captions reuse 00_config.R
# exactly as the Metric 1 charts do. No trend is fitted on any chart.
#
# Chart numbering: 20-2x.

# --- Shared base: derive the metric fields from the master table --------------
# QA CALLOUT: single source ----------------------------------------------------
# build_metrics23_base() and metrics23_national() are defined ONCE here and
# reused by Metric 3 (09_metric3.R sources this file). Both metrics draw from
# build_master_table(); there is no separate read.
build_metrics23_base <- function() {
  master <- build_master_table()
  
  cpi <- read_ons_cpi(fetch_cpi_data())
  september_cpi <- build_september_cpi(cpi) |>
    select(year, cpi_deflator, price_basis)
  
  master |>
    left_join(september_cpi, by = "year") |>
    mutate(
      net_writeoffs_nominal = wo_allowance + coalesce(wo_excess, 0),
      # Allowance fields are filed as negative liabilities; negate so a larger
      # provision reads as a larger positive number.
      allowance_cb = -noncoll_cb,
      allowance_ob = -noncoll_ob,
      allowance_change = allowance_cb - allowance_ob,
      arrears = sums_outstanding,
      covid_affected = year %in% cfg$covid_years,
      immature = year > (cfg$analysis_year - cfg$immature_years)
    )
}

# QA CALLOUT: sum that preserves "not available" -------------------------------
# sum(x, na.rm = TRUE) over an all-NA column returns 0, not NA. If a source field is genuinely unavailable, a
# plain na.rm sum turned "unavailable" into a hard zero that then plotted as a
# real flat line at £0. sum_or_na() returns NA when there is nothing to sum, so
# ggplot drops those years instead of drawing a false value.
sum_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

metrics23_national <- function(base) {
  base |>
    group_by(year, financial_year) |>
    summarise(
      n_authorities = n(),
      net_writeoffs = sum(net_writeoffs_nominal, na.rm = TRUE),
      allowance_cb = sum_or_na(allowance_cb),
      allowance_ob = sum_or_na(allowance_ob),
      allowance_change = sum_or_na(allowance_change),
      net_rates_payable = sum(net_rates_payable, na.rm = TRUE),
      arrears = sum_or_na(arrears),
      covid_affected = first(covid_affected),
      immature = first(immature),
      .groups = "drop"
    ) |>
    arrange(year)
}

# --- Shared annotation layer (identical to the Metric 1 panel charts) ---------
metric_annotations <- function(national) {
  layers <- list()
  
  if (any(national$covid_affected)) {
    covid <- sort(unique(national$year[national$covid_affected]))
    layers <- c(layers, annotate_covid(covid))
  }
  
  c(layers, list(
    scale_x_continuous(breaks = national$year, labels = label_fy(national$year))
  ))
}

metric2_caption <- function(extra = NULL) {
  mhclg_caption(
    extra = extra,
    scope = paste("Allowance for non-collection is the provision authorities",
                  "hold against rates they expect not to collect.")
  )
}

# --- Charts -------------------------------------------------------------------

# 20. Headline: closing allowance balance with realised write-offs overlaid.
# QA CALLOUT: what this chart is for -------------------------------------------
# The allowance is risk PROVIDED FOR; net write-offs are loss REALISED. Showing
# them together answers whether authorities are building provision ahead of the
# losses actually being taken - the forward-looking reading Toby asked for. The
# two are NOT the same quantity: noncoll_chrgd equals Line 3 by identity, but
# the closing BALANCE is a stock that accumulates, a different series from the
# annual write-off flow. Hence they are not comparable in level, only in shape.
chart_m2_balance_vs_writeoffs <- function(national) {
  long <- national |>
    select(year, covid_affected, immature,
           `Allowance closing balance` = allowance_cb,
           `Net write-offs (realised)` = net_writeoffs) |>
    pivot_longer(-c(year, covid_affected, immature),
                 names_to = "series", values_to = "value")
  
  ggplot(long, aes(year, value, colour = series)) +
    metric_annotations(national) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2.2) +
    scale_y_continuous(labels = label_gbp_m(0),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    scale_colour_manual(values = c(`Allowance closing balance` = mhclg_col$metric2,
                                   `Net write-offs (realised)` = mhclg_col$metric1)) +
    labs(
      title = "Provision held against realised write-offs, England",
      subtitle = paste0("Allowance is risk provided for; write-offs are loss ",
                        "realised. Series covers 2013-14 to 2025-26."),
      x = NULL, y = NULL, colour = NULL,
      caption = metric2_caption(
        paste("Closing balance is a stock; write-offs are the annual flow.",
              "The two are not directly comparable in level.")
      )
    ) +
    theme_mhclg(grid = "y")
}

# 21. Supporting: year-on-year change in the allowance (the flow).
chart_m2_change <- function(national) {
  covid <- sort(unique(national$year[national$covid_affected]))
  
  ggplot(national, aes(factor(year), allowance_change)) +
    annotate_covid_discrete(national$year, covid) +
    geom_hline(yintercept = 0, colour = mhclg_grey[["axis"]], linewidth = 0.4) +
    geom_col(fill = mhclg_col$metric2, width = 0.7) +
    scale_x_discrete(labels = \(x) label_fy(as.integer(x))) +
    scale_y_continuous(labels = label_gbp_m(0)) +
    labs(
      title = "Year-on-year change in the non-collection allowance, England",
      subtitle = paste0("Positive means authorities increased the provision. ",
                        "Series covers 2013-14 to 2025-26."),
      x = NULL, y = NULL,
      caption = metric2_caption(
        "The flow behind the closing-balance stock in the headline chart."
      )
    ) +
    theme_mhclg(grid = "y")
}

# 22. Supporting: allowance as a share of net rates payable (normalised).
chart_m2_share <- function(national) {
  plot_data <- national |>
    mutate(allowance_share = allowance_cb / net_rates_payable)
  
  ggplot(plot_data, aes(year, allowance_share)) +
    metric_annotations(national) +
    geom_line(linewidth = 0.9, colour = mhclg_col$metric2) +
    geom_point(size = 2.2, colour = mhclg_col$metric2) +
    scale_y_continuous(labels = label_prop(2),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    labs(
      title = "Non-collection allowance as a share of net rates payable, England",
      subtitle = paste0("Normalised against the tax base so years are comparable. ",
                        "Series covers 2013-14 to 2025-26."),
      x = NULL, y = NULL,
      caption = metric2_caption(
        "Denominator is NNDR3 net rates payable, not QRC4 (C16)."
      )
    ) +
    theme_mhclg(grid = "y")
}

build_metric2_charts <- function(national) {
  list(
    m2_balance_vs_writeoffs = chart_m2_balance_vs_writeoffs(national),
    m2_change = chart_m2_change(national),
    m2_share = chart_m2_share(national)
  )
}

# QA CALLOUT: missing from this file, present in 12_metric3.R as
# print_metric3_charts() - 11_run_metric2.R calls this and had no matching
# definition anywhere. Same pattern as the M3 equivalent, copied exactly.
print_metric2_charts <- function(charts) {
  iwalk(charts, \(plot, name) {
    message("Displaying chart: ", name)
    print(plot)
  })
  invisible(charts)
}

save_metric2_charts <- function(charts, dir = file.path(cfg$dir_charts, "m2")) {
  # QA CALLOUT: Metric 2 charts go in charts/m2/. save_mhclg() creates the
  # folder if absent, so no separate dir.create is needed.
  filenames <- c(
    m2_balance_vs_writeoffs = "20_metric2_balance_vs_writeoffs.png",
    m2_change = "21_metric2_change.png",
    m2_share = "22_metric2_share.png"
  )
  imap_chr(charts, \(plot, name) save_mhclg(plot, filenames[[name]], dir = dir))
}
