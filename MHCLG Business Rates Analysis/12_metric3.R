# 09_metric3.R ------------------------------------------------------------------
# Metric 3 chart DEFINITIONS: arrears. No runner here - see 10_run_metric3.R.
#
# Sources 07_metric2.R for the shared base (build_metrics23_base,
# metrics23_national, metric_annotations) so both metrics use one source and one
# annotation layer. House style reuses 00_config.R. No trend fitted.
#
# SCOPE NOTE: the commission asks for arrears AND collection rates. NNDR3 gives
# the arrears stock but NOT an in-year collection rate, which is a QRC4 concept
# (open question C16). Only arrears is built here; the collection-rate half is
# deferred pending the QRC4 decision with Toby.
#
# Chart numbering: 30-3x.

metric3_caption <- function(extra = NULL) {
  mhclg_caption(
    extra = extra,
    scope = paste("Arrears are business rates billed but still outstanding",
                  "from ratepayers at year end.")
  )
}

# 30. Headline: arrears stock over time.
# QA CALLOUT: arrears and C13 --------------------------------------------------
# Recent-year arrears are still being collected against, so the latest years'
# stock is inflated relative to where it will settle. The C13 immaturity point
# bites arrears HARDER than write-offs, because an arrears balance is a snapshot
# of debt not yet resolved either way. Shaded accordingly, no trend fitted.
chart_m3_arrears <- function(national) {
  ggplot(national, aes(year, arrears)) +
    metric_annotations(national) +
    geom_line(linewidth = 0.9, colour = mhclg_col$metric3) +
    geom_point(size = 2.2, colour = mhclg_col$metric3) +
    scale_y_continuous(labels = label_gbp_m(0),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    labs(
      title = "Business rates arrears outstanding, England",
      subtitle = paste0("Rates billed but not yet collected at year end. ",
                        "No trend fitted."),
      x = NULL, y = NULL,
      caption = metric3_caption(
        paste("NNDR3 gives the arrears stock, not an in-year collection rate",
              "(a QRC4 concept, C16). Recent years are immature.")
      )
    ) +
    theme_mhclg(grid = "y")
}

# 31. Supporting: arrears as a share of net rates payable (normalised).
chart_m3_share <- function(national) {
  plot_data <- national |>
    mutate(arrears_share = arrears / net_rates_payable)
  
  ggplot(plot_data, aes(year, arrears_share)) +
    metric_annotations(national) +
    geom_line(linewidth = 0.9, colour = mhclg_col$metric3) +
    geom_point(size = 2.2, colour = mhclg_col$metric3) +
    scale_y_continuous(labels = label_prop(1),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    labs(
      title = "Arrears as a share of net rates payable, England",
      subtitle = "Normalised against the tax base so years are comparable.",
      x = NULL, y = NULL,
      caption = metric3_caption(
        "Denominator is NNDR3 net rates payable, not QRC4 (C16)."
      )
    ) +
    theme_mhclg(grid = "y")
}

build_metric3_charts <- function(national) {
  list(
    m3_arrears = chart_m3_arrears(national),
    m3_share = chart_m3_share(national)
  )
}

print_metric3_charts <- function(charts) {
  iwalk(charts, \(plot, name) {
    message("Displaying chart: ", name)
    print(plot)
  })
  invisible(charts)
}

save_metric3_charts <- function(charts, dir = cfg$dir_charts) {
  filenames <- c(
    m3_arrears = "30_metric3_arrears.png",
    m3_share = "31_metric3_share.png"
  )
  imap_chr(charts, \(plot, name) save_mhclg(plot, filenames[[name]], dir = dir))
}