# 18_metric_outliers.R -----------------------------------------------------------

#

#

#
# QA CALLOUT: eligibility threshold ----------------------------------------------
# Authorities do not all appear in all thirteen years (reorganisation: 326 rows
# in 2013-14, 296 by 2023-24). An authority present for only 2 years that was
# top-quartile in both would score 100% on a naive share, which is meaningless.
# cfg$outlier_min_years requires a minimum number of observed years before an
# authority is eligible to be called recurring. Ineligible authorities are
# reported separately, not silently dropped.
#
# QA CALLOUT: COVID years are INCLUDED, with a sensitivity check -----------------
# Unlike the correlation work in 16_metric3_arrears_drivers.R, COVID years are
# not excluded here. The question is whether an authority is PERSISTENTLY a
# high writer-off, and dropping two years from a thirteen-year count weakens
# exactly the persistence signal being measured. The runner reports the same
# counts with COVID years excluded so the ranking can be checked for
# sensitivity; if a "recurring" authority only qualifies on COVID years, that
# shows up there.
#
# Proposed caveat C39: quartile membership is computed within each year, so it
#   is a relative measure. A year in which all authorities wrote off little
#   still has a top quartile.
#
# Depends on: 00_config.R, 01_init.R, 02_master_table.R, 10_metric2.R,
# 04_metric1_maps.R (boundaries and map theme), 14_stock_of_properties.R
# (la_code -> ons_code crosswalk, needed to map pre-2020 years).

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("04_metric1_maps.R")
source("10_metric2.R")
source("14_stock_of_properties.R")
# QA CALLOUT: ggrepel dependency, owned here rather than via file 15 ----------
# This file previously sourced 15_metric3_sector.R purely to reuse
# label_outliers() and library(ggrepel). That made a general cross-metric
# outliers file depend on an M3-sector-specific one for two small, generic
# things. Defined directly below instead, so this file is self-contained.
# Install with install.packages("ggrepel") if not already present.
suppressPackageStartupMessages(library(ggrepel))

# Highest and lowest N raw values of value_col, one label per authority (its
# own most extreme observation survives distinct(), so a pooled multi-year
# chart cannot show the same name twice). Identical to the version in
# 15_metric3_sector.R - kept in sync by hand since it is a small, stable
# utility; if it changes there, mirror the change here too.
label_outliers <- function(data, value_col, n = 5) {
  highest <- data |>
    filter(!is.na(.data[[value_col]])) |>
    arrange(desc(.data[[value_col]])) |>
    distinct(la_code, .keep_all = TRUE) |>
    slice_head(n = n)
  lowest <- data |>
    filter(!is.na(.data[[value_col]])) |>
    arrange(.data[[value_col]]) |>
    distinct(la_code, .keep_all = TRUE) |>
    slice_head(n = n)
  bind_rows(highest, lowest)
}

cfg$outlier_min_years <- 8L
cfg$outlier_top_n     <- 12L

outlier_caption <- function(extra = NULL) {
  mhclg_caption(
    extra = extra,
    scope = paste("Quartiles are computed within each year, so membership is",
                  "relative to other authorities in that year, not an absolute",
                  "threshold (C39).")
  )
}

# =============================================================================
# 1. QUARTILE MEMBERSHIP
# =============================================================================
build_outlier_base <- function() {
  base <- build_metrics23_base()
  
  base |>
    filter(!is.na(net_rates_payable), net_rates_payable > 0,
           !is.na(net_writeoffs_nominal)) |>
    mutate(writeoff_rate = net_writeoffs_nominal / net_rates_payable) |>
    group_by(year) |>
    # ntile() splits into 4 equal-count groups within the year. 4 = highest
    # write-off rate because ntile ranks ascending.
    mutate(quartile = ntile(writeoff_rate, 4)) |>
    ungroup() |>
    select(la_code, la_name, year, financial_year, writeoff_rate, quartile,
           net_writeoffs_nominal, net_rates_payable, covid_affected)
}

# Counts per authority. exclude_covid is the sensitivity variant, not the
# headline - see the QA callout above.
summarise_outliers <- function(outlier_base, exclude_covid = FALSE) {
  dat <- outlier_base
  if (exclude_covid) dat <- dat |> filter(!covid_affected)
  
  dat |>
    group_by(la_code) |>
    summarise(
      la_name = last(na.omit(la_name)),
      years_observed = n(),
      years_top = sum(quartile == 4L),
      years_bottom = sum(quartile == 1L),
      mean_rate = mean(writeoff_rate, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      eligible = years_observed >= cfg$outlier_min_years,
      share_top = years_top / years_observed,
      share_bottom = years_bottom / years_observed
    ) |>
    arrange(desc(years_top), desc(share_top))
}

# =============================================================================
# 2. CHARTS
# =============================================================================

# 44. The slide 13 chart. Replaces a single-year top/bottom 10: a diverging
# bar of the most persistent high and low write-off authorities, labelled with
# how many of their observed years they spent in that quartile.
chart_m1_recurring_outliers <- function(outlier_summary,
                                        n = cfg$outlier_top_n) {
  eligible <- outlier_summary |> filter(eligible)
  
  top <- eligible |>
    arrange(desc(years_top), desc(mean_rate)) |>
    slice_head(n = n) |>
    mutate(group = "Persistently high", value = years_top)
  bottom <- eligible |>
    arrange(desc(years_bottom), mean_rate) |>
    slice_head(n = n) |>
    mutate(group = "Persistently low", value = -years_bottom)
  
  plot_data <- bind_rows(top, bottom) |>
    mutate(label = paste0(la_name, " (", abs(value), "/", years_observed, ")"),
           group = factor(group,
                          levels = c("Persistently high", "Persistently low")))
  
  ggplot(plot_data, aes(value, fct_reorder(label, value), fill = group)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    scale_fill_manual(values = c("Persistently high" = mhclg_col$metric1,
                                 "Persistently low" = mhclg_col$metric3)) +
    scale_x_continuous(labels = \(x) abs(x)) +
    labs(
      title = "Which authorities are repeatedly at the extremes?",
      subtitle = paste0("Years spent in the top or bottom quartile of the ",
                        "write-off rate, ", label_fy(cfg$year_min), " to ",
                        label_fy(cfg$analysis_year),
                        ". Authorities observed in at least ",
                        cfg$outlier_min_years, " years only."),
      x = "Years in quartile", y = NULL, fill = NULL,
      caption = outlier_caption(
        paste("Counting across the series separates structural outliers from",
              "authorities that had one large insolvency in a single year.")
      )
    ) +
    theme_mhclg(grid = "x") +
    theme(legend.position = "top")
}

# 45. The slide 14 map. Shows persistence, not a single year's rate - so the
# fill is a count of years, not a rate, and it deliberately does NOT reuse
# prepare_metric1_map_data() (which bins one year's writeoff_rate_nrp).
chart_m1_outlier_map <- function(outlier_summary, crosswalk, boundaries) {
  mapped <- outlier_summary |>
    filter(eligible) |>
    left_join(crosswalk |> select(la_code, ons_code), by = "la_code")
  
  unmapped <- sum(is.na(mapped$ons_code))
  if (unmapped > 0) {
    message("  NOTE: ", unmapped, " eligible authority(ies) have no ons_code ",
            "and cannot be mapped.")
  }
  
  map_data <- boundaries |>
    left_join(mapped |> filter(!is.na(ons_code)), by = "ons_code") |>
    mutate(years_top_band = cut(
      years_top,
      breaks = c(-Inf, 0, 2, 4, 6, Inf),
      labels = c("Never", "1-2 years", "3-4 years", "5-6 years", "7+ years")
    ))
  
  ggplot(map_data) +
    geom_sf(aes(fill = years_top_band), colour = "white", linewidth = 0.08) +
    scale_fill_manual(
      values = c("Never" = "#F0F0F0", "1-2 years" = "#C9D9E5",
                 "3-4 years" = "#7FA3BF", "5-6 years" = "#3D6E94",
                 "7+ years" = mhclg_col$metric1),
      na.value = "#FAFAFA", name = "Years in top quartile"
    ) +
    labs(
      title = "Recurring high write-off authorities",
      subtitle = paste0("Years spent in the top quartile of the write-off ",
                        "rate, ", label_fy(cfg$year_min), " to ",
                        label_fy(cfg$analysis_year), "."),
      caption = map_caption()
    ) +
    metric1_map_theme()
}

# =============================================================================
# 3. CROSS-METRIC OUTLIERS
# =============================================================================
# build_metrics23_base() already carries write-offs, allowance and arrears in
# one row per authority-year - the same source charts 44/45 use for M1 alone.
build_cross_metric_base <- function() {
  base <- build_metrics23_base()
  base |>
    filter(!is.na(net_rates_payable), net_rates_payable > 0) |>
    mutate(
      writeoff_rate = if_else(!is.na(net_writeoffs_nominal),
                              net_writeoffs_nominal / net_rates_payable,
                              NA_real_),
      allowance_share = if_else(!is.na(allowance_cb),
                                allowance_cb / net_rates_payable, NA_real_),
      arrears_share = if_else(!is.na(arrears),
                              arrears / net_rates_payable, NA_real_)
    )
}

# 46. Provision against realised loss, authority level - the same "5x" gap as
# slide 18 (national), broken down to who is actually driving it. A 45-degree
# line marks parity: above it, an authority wrote off MORE than it had
# provisioned (a surprise, worth asking why); below it, more was set aside
# than was ever lost (the conservative-provisioning norm this deck has already
# established nationally).
chart_cross_allowance_vs_writeoff <- function(cross_base, year = cfg$analysis_year) {
  plot_data <- cross_base |>
    filter(year == !!year, !is.na(allowance_share), !is.na(writeoff_rate)) |>
    mutate(gap = writeoff_rate - allowance_share)
  
  max_val <- max(c(plot_data$allowance_share, plot_data$writeoff_rate),
                 na.rm = TRUE)
  
  ggplot(plot_data, aes(allowance_share, writeoff_rate)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.5,
                colour = mhclg_col$deemphasis, linetype = "dashed") +
    geom_point(alpha = 0.45, size = 1.8, colour = mhclg_col$metric2) +
    geom_text_repel(
      data = label_outliers(plot_data, "gap", n = 5),
      aes(label = la_name), size = 2.8, colour = mhclg_col$text,
      segment.size = 0.3, max.overlaps = Inf, seed = 1
    ) +
    scale_x_continuous(labels = label_prop(0), limits = c(0, max_val)) +
    scale_y_continuous(labels = label_prop(0), limits = c(0, max_val)) +
    labs(
      title = "Provision against realised loss, by authority",
      subtitle = paste0(label_fy(year), ". Above the dashed line: wrote off ",
                        "more than provisioned. Below: provisioned more than ",
                        "was ever lost. Labelled: 5 largest gaps each way."),
      x = "Allowance for non-collection, share of net rates payable",
      y = "Net write-offs, share of net rates payable",
      caption = mhclg_caption(scope = paste(
        "Authority-level version of the national gap (~5x, slide 18). Two",
        "different NNDR3 return lines - not a claim that one predicts the",
        "other, only that they can be compared directly.")
      )
    ) +
    theme_mhclg(grid = "both")
}

# 47. Arrears against write-offs - the same debt-lifecycle question from slide
# 5 (bill -> arrears -> write-off), now at authority level instead of the
# national total. High arrears with low write-offs: debt not yet given up on -
# could be collection discipline, could be delay; this cannot distinguish
# between the two. Low arrears with high write-offs: writing off faster
# relative to what currently sits outstanding.
chart_cross_arrears_vs_writeoff <- function(cross_base, year = cfg$analysis_year) {
  plot_data <- cross_base |>
    filter(year == !!year, !is.na(arrears_share), !is.na(writeoff_rate))
  
  ggplot(plot_data, aes(arrears_share, writeoff_rate)) +
    geom_point(alpha = 0.45, size = 1.8, colour = mhclg_col$metric3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linewidth = 0.7, colour = mhclg_col$highlight) +
    geom_text_repel(
      data = label_outliers(plot_data, "arrears_share", n = 5),
      aes(label = la_name), size = 2.8, colour = mhclg_col$text,
      segment.size = 0.3, max.overlaps = Inf, seed = 1
    ) +
    scale_x_continuous(labels = label_prop(0)) +
    scale_y_continuous(labels = label_prop(0)) +
    labs(
      title = "Arrears against write-offs, by authority",
      subtitle = paste0(label_fy(year), ". High arrears with low write-offs: ",
                        "debt not yet given up on. Labelled: 5 highest arrears."),
      x = "Arrears, share of net rates payable",
      y = "Net write-offs, share of net rates payable",
      caption = mhclg_caption(scope = paste(
        "Two stages of the same debt lifecycle (slide 5). Pearson association",
        "only, per the standardised method used across this pack.")
      )
    ) +
    theme_mhclg(grid = "both")
}

# --- Multi-metric persistence --------------------------------------------------
# Extends the SAME quartile-persistence logic as chart 44, computed for all
# three metrics rather than write-offs alone. What 44/45 cannot answer: is it
# the same authorities struggling across every metric, or different ones for
# different reasons? Persistently top-quartile on all three is a stronger,
# more specific finding than being flagged on any single measure once.
build_multi_metric_quartiles <- function(cross_base) {
  cross_base |>
    filter(!is.na(writeoff_rate), !is.na(allowance_share),
           !is.na(arrears_share)) |>
    group_by(year) |>
    mutate(
      q_writeoff = ntile(writeoff_rate, 4),
      q_allowance = ntile(allowance_share, 4),
      q_arrears = ntile(arrears_share, 4)
    ) |>
    ungroup()
}

# "Persistent" on a metric = top quartile in more than half of an authority's
# OWN observed years - the same idea as the majority-share threshold implicit
# in charts 44/45, just made explicit here since three metrics need comparing
# on equal footing.
summarise_multi_metric_outliers <- function(quartiles,
                                            min_years = cfg$outlier_min_years) {
  quartiles |>
    group_by(la_code) |>
    summarise(
      la_name = last(na.omit(la_name)),
      years_observed = n(),
      years_top_writeoff = sum(q_writeoff == 4L),
      years_top_allowance = sum(q_allowance == 4L),
      years_top_arrears = sum(q_arrears == 4L),
      .groups = "drop"
    ) |>
    mutate(
      eligible = years_observed >= min_years,
      persistent_writeoff = years_top_writeoff / years_observed > 0.5,
      persistent_allowance = years_top_allowance / years_observed > 0.5,
      persistent_arrears = years_top_arrears / years_observed > 0.5,
      metrics_flagged = persistent_writeoff + persistent_allowance +
        persistent_arrears
    ) |>
    arrange(desc(metrics_flagged), desc(years_top_writeoff))
}

# 48. The cross-metric callout. Same text-table style as
# chart_panel_distribution_callout() in file 08, for visual consistency across
# the pack's two "named authorities" table slides. Only authorities flagged on
# 2 or more metrics are shown - flagged-on-1 is just charts 44/45 again.
chart_multi_metric_callout <- function(summary) {
  standout <- summary |>
    filter(eligible, metrics_flagged >= 2) |>
    arrange(desc(metrics_flagged), desc(years_top_writeoff))
  
  if (nrow(standout) == 0) {
    stop("No eligible authority is persistently top-quartile on 2 or more ",
         "metrics - nothing to show in this callout.", call. = FALSE)
  }
  
  label_data <- standout |>
    mutate(wo_label = paste0(years_top_writeoff, "/", years_observed),
           al_label = paste0(years_top_allowance, "/", years_observed),
           ar_label = paste0(years_top_arrears, "/", years_observed),
           row = row_number())
  n <- nrow(label_data)
  
  ggplot(label_data, aes(y = -row)) +
    geom_text(aes(x = 0, label = la_name), hjust = 0, size = 3.3,
              colour = mhclg_col$text) +
    geom_text(aes(x = 0.42, label = wo_label), hjust = 0, size = 3.3,
              colour = mhclg_col$metric1) +
    geom_text(aes(x = 0.58, label = al_label), hjust = 0, size = 3.3,
              colour = mhclg_col$metric2) +
    geom_text(aes(x = 0.74, label = ar_label), hjust = 0, size = 3.3,
              colour = mhclg_col$metric3) +
    geom_text(aes(x = 0.90, label = metrics_flagged), hjust = 0, size = 3.3,
              fontface = "bold") +
    annotate("text", x = 0, y = 0.8, label = "Authority", hjust = 0,
             fontface = "bold", size = 3.1, colour = mhclg_col$secondary) +
    annotate("text", x = 0.42, y = 0.8, label = "Write-off yrs", hjust = 0,
             fontface = "bold", size = 3.1, colour = mhclg_col$secondary) +
    annotate("text", x = 0.58, y = 0.8, label = "Allowance yrs", hjust = 0,
             fontface = "bold", size = 3.1, colour = mhclg_col$secondary) +
    annotate("text", x = 0.74, y = 0.8, label = "Arrears yrs", hjust = 0,
             fontface = "bold", size = 3.1, colour = mhclg_col$secondary) +
    annotate("text", x = 0.90, y = 0.8, label = "#", hjust = 0,
             fontface = "bold", size = 3.1, colour = mhclg_col$secondary) +
    xlim(0, 1) +
    ylim(-n - 0.5, 1.4) +
    labs(
      title = paste0(n, " authority(ies) persistently top-quartile on 2 or ",
                     "more metrics"),
      subtitle = paste("\"Persistent\" = top quartile in more than half of",
                       "observed years. Years shown out of years observed."),
      caption = mhclg_caption(scope = paste(
        "Metrics: write-off rate (M1), allowance share (M2), arrears share",
        "(M3). Answers whether the same authorities recur across all three,",
        "or different ones for different reasons.")
      )
    ) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 13, hjust = 0,
                                    margin = margin(b = 2)),
          plot.subtitle = element_text(size = 9.5, colour = mhclg_col$secondary,
                                       margin = margin(b = 10)),
          plot.margin = margin(10, 10, 10, 10))
}

# =============================================================================
# RUNNER
# =============================================================================
run_metric_outliers <- function() {
  message(strrep("-", 74))
  message("OUTLIERS ACROSS THE SERIES: M1 RECURRING (44/45) + CROSS-METRIC (46-48)")
  message(strrep("-", 74))
  
  if (!exists("res_sop")) {
    stop("res_sop not found - source 14_stock_of_properties.R first ",
         "(needed for the la_code -> ons_code crosswalk).", call. = FALSE)
  }
  
  message("")
  message("[1] Assigning within-year quartiles")
  outlier_base <- build_outlier_base()
  message("  ", nrow(outlier_base), " authority-year row(s), ",
          n_distinct(outlier_base$la_code), " authorities.")
  
  message("")
  message("[2] Counting quartile membership")
  summary_all <- summarise_outliers(outlier_base)
  summary_excl <- summarise_outliers(outlier_base, exclude_covid = TRUE)
  
  n_eligible <- sum(summary_all$eligible)
  message("  ", n_eligible, " authority(ies) observed in at least ",
          cfg$outlier_min_years, " years (eligible).")
  message("  ", sum(!summary_all$eligible), " ineligible (too few years).")
  
  message("")
  message("[3] Most persistent high write-off authorities")
  print(summary_all |> filter(eligible) |>
          slice_head(n = 10) |>
          select(la_name, years_top, years_observed, mean_rate) |>
          mutate(mean_rate = scales::percent(mean_rate, accuracy = 0.01)))
  
  message("")
  message("[4] Sensitivity: same ranking with COVID years excluded")
  print(summary_excl |> filter(eligible) |>
          slice_head(n = 10) |>
          select(la_name, years_top, years_observed))
  
  message("")
  message("[5] Charts")
  chart_bars <- chart_m1_recurring_outliers(summary_all)
  
  boundaries <- read_england_boundaries()
  chart_map <- chart_m1_outlier_map(summary_all, res_sop$crosswalk, boundaries)
  
  ggsave(file.path(cfg$dir_charts, "44_metric1_recurring_outliers.png"),
         chart_bars, width = 9, height = 6.5, dpi = 300, bg = "white")
  ggsave(file.path(cfg$dir_charts, "45_metric1_outlier_map.png"),
         chart_map, width = 7, height = 8, dpi = 300, bg = "white")
  
  write_csv(summary_all,
            file.path(cfg$dir_output, "metric1_outlier_summary.csv"))
  write_csv(outlier_base,
            file.path(cfg$dir_output, "metric1_quartile_membership.csv"))
  
  message("")
  message("[6] Cross-metric base (write-off rate, allowance share, arrears share)")
  cross_base <- build_cross_metric_base()
  message("  ", nrow(cross_base), " authority-year row(s).")
  
  message("")
  message("[7] Cross-metric scatters, ", label_fy(cfg$analysis_year))
  chart_allowance_vs_writeoff <- chart_cross_allowance_vs_writeoff(cross_base)
  chart_arrears_vs_writeoff <- chart_cross_arrears_vs_writeoff(cross_base)
  
  message("")
  message("[8] Multi-metric persistence")
  multi_quartiles <- build_multi_metric_quartiles(cross_base)
  multi_summary <- summarise_multi_metric_outliers(multi_quartiles)
  by_count <- multi_summary |> filter(eligible) |> count(metrics_flagged)
  message("  Eligible authorities by number of metrics persistently ",
          "top-quartile on:")
  print(by_count)
  chart_multi <- chart_multi_metric_callout(multi_summary)
  
  ggsave(file.path(cfg$dir_charts, "46_cross_allowance_vs_writeoff.png"),
         chart_allowance_vs_writeoff, width = 9, height = 6.5, dpi = 300,
         bg = "white")
  ggsave(file.path(cfg$dir_charts, "47_cross_arrears_vs_writeoff.png"),
         chart_arrears_vs_writeoff, width = 9, height = 6.5, dpi = 300,
         bg = "white")
  ggsave(file.path(cfg$dir_charts, "48_cross_multi_metric_outliers.png"),
         chart_multi, width = 12, height = 9, dpi = 300, bg = "white")
  
  write_csv(multi_summary,
            file.path(cfg$dir_output, "cross_metric_outlier_summary.csv"))
  
  message("")
  message(strrep("=", 74))
  message("Charts: ", cfg$dir_charts,
          "/44_metric1_recurring_outliers.png, 45_metric1_outlier_map.png,")
  message("        46_cross_allowance_vs_writeoff.png, ",
          "47_cross_arrears_vs_writeoff.png,")
  message("        48_cross_multi_metric_outliers.png")
  message("Data:   ", cfg$dir_output, "/metric1_outlier_summary.csv, ",
          "cross_metric_outlier_summary.csv")
  message(strrep("=", 74))
  
  invisible(list(
    outlier_base = outlier_base, summary_all = summary_all,
    summary_excl = summary_excl, cross_base = cross_base,
    multi_summary = multi_summary,
    charts = list(bars = chart_bars, map = chart_map,
                  allowance_vs_writeoff = chart_allowance_vs_writeoff,
                  arrears_vs_writeoff = chart_arrears_vs_writeoff,
                  multi_metric = chart_multi)
  ))
}

res_metric_outliers <- run_metric_outliers()