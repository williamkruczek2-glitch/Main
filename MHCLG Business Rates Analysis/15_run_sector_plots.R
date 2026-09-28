# 15_metric3_sector.R -----------------------------------------------------------
# Metric 3 supporting charts: what the sector composition of the tax base can
# say about outstanding liabilities. Definitions and runner in one file.
#
# SCOPE NOTE - READ BEFORE USING THESE CHARTS.
# NNDR3 does not split arrears by property sector. There is no sector dimension
# on sums_outstanding, so arrears CANNOT be decomposed into "retail arrears",
# "office arrears" and so on, and nothing here attempts it. What these charts
# show instead:
#   - how the SECTOR COMPOSITION OF THE TAX BASE has moved over the series
#     (SOP alone), which is context for why arrears might move; and
#   - whether authorities with more of a given sector run higher arrears
#     (cross-sectional association at authority level).
# The second is an association between two authority characteristics. It is not
# attribution, and sector mix covaries with urban/rural character, tax base size
# and deprivation, none of which are controlled for here. Proposed caveat C36.
#
# QA CALLOUT: national sector series use LIVE authorities only ----------------
# SOP carries both an abolished authority AND its back-cast successor for the
# same year: at 31 March 2013 Bournemouth (abolished 2019) shows 7,290 and BCP
# shows 14,500. Summing every LAUA row overcounts England by ~136,000 in 2013.
# Filtering to Abolished year == "[z]" reconciles to the published England
# figure within rounding (1,779,560 against 1,779,370) AND gives a constant
# 296-authority basis across all fifteen years, so the trend is not contaminated
# by reorganisation. The authority-level scatter joins per authority-year and is
# unaffected.
#
# Depends on: 00_config.R, 01_init.R, 02_master_table.R, 10_metric2.R,
# 12_metric3.R, 14_stock_of_properties.R.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("10_metric2.R")
source("12_metric3.R")
source("14_stock_of_properties.R")

cfg$sector_levels <- c("retail", "office", "industrial", "other")
cfg$sector_labels <- c(retail = "Retail", office = "Office",
                       industrial = "Industrial", other = "Other")
cfg$sector_index_base <- 2013L

sector_caption <- function(extra = NULL) {
  mhclg_caption(
    extra = extra,
    scope = paste("Property counts are VOA Stock of Properties at 31 March,",
                  "rounded to the nearest 10. Arrears are not published by",
                  "sector; sector figures describe the tax base, not arrears.")
  )
}

# --- Data ---------------------------------------------------------------------

# Constant-boundary national series: live authorities only (see QA callout).
# min_year defaults to the existing cutoff so every current call site is
# unaffected; sector_yoy() below passes one year earlier to get a prior-year
# denominator for the first plotted point.
sector_national <- function(sop_panel, min_year = cfg$sector_index_base) {
  sop_panel |>
    filter(abolished_year == "[z]", str_starts(ons_code, "E")) |>
    group_by(year = snapshot_year) |>
    summarise(across(all_of(paste0("sop_n_", cfg$sector_levels)),
                     \(x) sum(x, na.rm = TRUE)),
              total = sum(sop_n_total, na.rm = TRUE), .groups = "drop") |>
    filter(year >= min_year) |>
    pivot_longer(starts_with("sop_n_"), names_to = "sector", values_to = "n") |>
    mutate(sector = str_remove(sector, "^sop_n_"),
           sector = factor(sector, levels = cfg$sector_levels,
                           labels = cfg$sector_labels[cfg$sector_levels]))
}

# Index to the base year so sectors of very different size are comparable.
sector_indexed <- function(national_sectors) {
  national_sectors |>
    group_by(sector) |>
    mutate(index = 100 * n / n[year == cfg$sector_index_base]) |>
    ungroup()
}

sector_change <- function(national_sectors) {
  yr_max <- max(national_sectors$year)
  national_sectors |>
    filter(year %in% c(cfg$sector_index_base, yr_max)) |>
    select(sector, year, n) |>
    pivot_wider(names_from = year, values_from = n,
                names_prefix = "y") |>
    rename(start = !!paste0("y", cfg$sector_index_base),
           end = !!paste0("y", yr_max)) |>
    mutate(pct_change = (end - start) / start,
           direction = if_else(pct_change >= 0, "up", "down"))
}

# Needs one extra prior year purely as the denominator for the first plotted
# year's change, then drops it - full 2013-25 coverage, matching the other
# sector charts, rather than starting one year short at 2014.
sector_yoy <- function(sop_panel) {
  sector_national(sop_panel, min_year = cfg$sector_index_base - 1L) |>
    arrange(sector, year) |>
    group_by(sector) |>
    mutate(yoy = (n - lag(n)) / lag(n)) |>
    ungroup() |>
    filter(year >= cfg$sector_index_base)
}

# --- Charts -------------------------------------------------------------------

# 32. Sector trajectories, indexed. The primary chart for "has any sector moved
# significantly": absolute counts differ by an order of magnitude between
# sectors, so an unindexed line chart would show only that Other is largest.
chart_m3_sector_index <- function(indexed) {
  ggplot(indexed, aes(year, index, colour = sector)) +
    annotate_covid(cfg$covid_years) +
    geom_hline(yintercept = 100, linewidth = 0.4,
               colour = mhclg_col$deemphasis) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_colour_mhclg() +
    scale_x_continuous(breaks = sort(unique(indexed$year)),
                       labels = label_fy(sort(unique(indexed$year)))) +
    scale_y_continuous(expand = expansion(c(0.05, 0.12))) +
    labs(
      title = paste0("Rateable property counts by sector, indexed (",
                     label_fy(cfg$sector_index_base), " = 100)"),
      subtitle = paste("Constant set of 296 live billing authorities, so",
                       "reorganisation does not affect the trend."),
      x = NULL, y = NULL, colour = NULL,
      caption = sector_caption(
        paste("Indexed because absolute counts differ by an order of magnitude",
              "between sectors.")
      )
    ) +
    theme_mhclg(grid = "y") +
    theme(legend.position = "top")
}

# 33. Headline summary of the same data for a slide.
chart_m3_sector_change <- function(change, year_from, year_to) {
  ggplot(change, aes(pct_change, fct_reorder(sector, pct_change),
                     fill = direction)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    scale_fill_manual(values = c(up = mhclg_col$metric3,
                                 down = mhclg_col$highlight),
                      guide = "none") +
    scale_x_continuous(labels = label_prop(0),
                       expand = expansion(c(0.12, 0.12))) +
    labs(
      title = "Change in rateable property count by sector",
      subtitle = paste0(label_fy(year_from), " to ", label_fy(year_to),
                        ", constant set of live billing authorities."),
      x = NULL, y = NULL,
      caption = sector_caption()
    ) +
    theme_mhclg(grid = "x")
}

# 36-37. Authority-level association, one call per sector (retail -> 36,
# other -> 37). Deliberately the only charts that put a
# sector measure and arrears on the same axes, and it is a scatter rather than
# a decomposition for the reason in the scope note.
chart_m3_arrears_vs_sector <- function(authority, sector = "retail") {
  col <- paste0("sop_n_", sector)
  plot_data <- authority |>
    filter(!is.na(.data[[col]]), !is.na(sop_n_total), sop_n_total > 0,
           !is.na(arrears), !is.na(net_rates_payable), net_rates_payable > 0) |>
    mutate(sector_share = .data[[col]] / sop_n_total,
           arrears_share = arrears / net_rates_payable)
  
  rho <- suppressWarnings(
    cor(plot_data$sector_share, plot_data$arrears_share, method = "pearson",
        use = "complete.obs")
  )
  
  ggplot(plot_data, aes(sector_share, arrears_share)) +
    geom_point(alpha = 0.45, size = 1.8, colour = mhclg_col$metric3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                linewidth = 0.7, colour = mhclg_col$highlight) +
    scale_x_continuous(labels = label_prop(0)) +
    scale_y_continuous(labels = label_prop(0)) +
    labs(
      title = paste0("Arrears against ", cfg$sector_labels[[sector]],
                     " share of the property stock"),
      subtitle = paste0("Billing authorities, ", label_fy(cfg$analysis_year),
                        ". Pearson r = ", sprintf("%.2f", rho),
                        ". Association only, not attribution."),
      x = paste0(cfg$sector_labels[[sector]], " share of rateable properties"),
      y = "Arrears as a share of net rates payable",
      caption = sector_caption(
        paste("Pearson correlation. Authority-level distributions are skewed",
              "(City of London, Westminster), which Pearson does not account",
              "for - read alongside the scatter, not in place of it. Sector",
              "mix also covaries with urban/rural character and tax base",
              "size, not controlled for (C36).")
      )
    ) +
    theme_mhclg(grid = "both")
}

# 34. Sector share of stock, clustered by year. This is the rate-vs-time view
# as a bar chart rather than a line: each year gets a cluster of four bars, one
# per sector, height = that sector's SHARE of total stock that year (a rate,
# not a raw count - counts differ by an order of magnitude between sectors and
# would make the smallest bars unreadable in the same cluster).
# QA CALLOUT: colour is the key, not per-bar labels ----------------------------
# 13 years x 4 sectors is 52 bars. A number on every bar is illegible at any
# usable chart width and repeats the same four sector names 13 times over.
# Sector is identified by fill colour and the legend instead.
chart_m3_sector_bars <- function(national_sectors) {
  plot_data <- national_sectors |> mutate(share = n / total)
  
  ggplot(plot_data, aes(factor(year), share, fill = sector)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.7) +
    scale_fill_mhclg() +
    scale_x_discrete(labels = label_fy(sort(unique(plot_data$year)))) +
    scale_y_continuous(labels = label_prop(0), expand = expansion(c(0, 0.08))) +
    labs(
      title = "Sector share of rateable property stock, by year",
      subtitle = paste("Rate, not count - each bar is that sector's share of",
                       "the 296-authority live stock that year."),
      x = NULL, y = NULL, fill = NULL,
      caption = sector_caption()
    ) +
    theme_mhclg(grid = "y") +
    theme(legend.position = "top")
}

# 35. Year-on-year change, same colour key as chart 34. Same sector factor
# (levels = cfg$sector_levels, same order every time it's built in
# sector_national()), so scale_colour_mhclg() here maps to the identical
# colours scale_fill_mhclg() used for the fill in chart 34 - no separate
# palette to keep in sync by hand.
# Complements the cumulative index in chart 32: that shows how far a sector
# has moved from 2013; this shows WHEN it moved, which a cumulative index can
# hide inside a longer trend (e.g. a single volatile COVID year).
chart_m3_sector_yoy <- function(yoy_data) {
  ggplot(yoy_data, aes(year, yoy, colour = sector)) +
    annotate_covid(cfg$covid_years) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = mhclg_col$text) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_colour_mhclg() +
    scale_x_continuous(breaks = sort(unique(yoy_data$year)),
                       labels = label_fy(sort(unique(yoy_data$year)))) +
    scale_y_continuous(labels = label_prop(1), expand = expansion(c(0.12, 0.12))) +
    labs(
      title = "Year-on-year change in rateable property count, by sector",
      subtitle = paste("Same sector colours as the other stock charts.",
                       "Constant set of 296 live billing authorities."),
      x = NULL, y = NULL, colour = NULL,
      caption = sector_caption()
    ) +
    theme_mhclg(grid = "y") +
    theme(legend.position = "top")
}


build_m3_sector_charts <- function(sop_panel, joined) {
  national_sectors <- sector_national(sop_panel)
  indexed <- sector_indexed(national_sectors)
  change <- sector_change(national_sectors)
  yoy <- sector_yoy(sop_panel)
  year_to <- max(national_sectors$year)
  
  # res_sop$joined is built from master (file 14), not build_metrics23_base()
  # (file 10) - so it carries sums_outstanding, not the renamed arrears used
  # everywhere else in Metric 2/3. Same rename as build_metrics23_base(),
  # applied here rather than restructuring file 14's join.
  authority <- joined |>
    filter(year == cfg$analysis_year) |>
    mutate(arrears = sums_outstanding)
  
  list(
    m3_sector_index  = chart_m3_sector_index(indexed),
    m3_sector_change = chart_m3_sector_change(change, cfg$sector_index_base,
                                              year_to),
    m3_sector_bars   = chart_m3_sector_bars(national_sectors),
    m3_sector_yoy    = chart_m3_sector_yoy(yoy),
    m3_arrears_retail = chart_m3_arrears_vs_sector(authority, "retail"),
    m3_arrears_other  = chart_m3_arrears_vs_sector(authority, "other")
  )
}

run_metric3_sector <- function() {
  message(strrep("-", 74))
  message("METRIC 3 SUPPORTING: SECTOR COMPOSITION OF THE TAX BASE")
  message(strrep("-", 74))
  
  if (!exists("res_sop")) {
    stop("res_sop not found - source 14_stock_of_properties.R first.",
         call. = FALSE)
  }
  sop_panel <- res_sop$sop_panel
  joined <- res_sop$joined
  
  national_sectors <- sector_national(sop_panel)
  change <- sector_change(national_sectors)
  
  message("")
  message("[1] National sector counts, live authorities only")
  print(national_sectors |>
          pivot_wider(names_from = sector, values_from = n) |>
          mutate(across(where(is.numeric) & !any_of("year"),
                        \(x) format(x, big.mark = ","))),
        n = Inf)
  
  message("")
  message("[2] Change ", label_fy(cfg$sector_index_base), " to ",
          label_fy(max(national_sectors$year)))
  print(change |> mutate(pct_change = sprintf("%+.1f%%", 100 * pct_change)))
  
  charts <- build_m3_sector_charts(sop_panel, joined)
  
  files <- c(
    m3_sector_index   = "32_metric3_sector_index.png",
    m3_sector_change  = "33_metric3_sector_change.png",
    m3_sector_bars    = "34_metric3_sector_bars.png",
    m3_sector_yoy     = "35_metric3_sector_yoy.png",
    m3_arrears_retail = "36_metric3_arrears_vs_retail.png",
    m3_arrears_other  = "37_metric3_arrears_vs_other.png"
  )
  walk(names(files), \(nm) {
    ggsave(file.path(cfg$dir_charts, files[[nm]]), charts[[nm]],
           width = 9, height = 5.5, dpi = 300, bg = "white")
  })
  
  message("")
  message(strrep("=", 74))
  message("Charts written to ", cfg$dir_charts, "/: ",
          paste(files, collapse = ", "))
  message("Arrears are NOT published by sector - see the scope note at the top ",
          "of this file before using these in the pack.")
  message(strrep("=", 74))
  
  invisible(list(national_sectors = national_sectors, change = change,
                 charts = charts))
}

res_m3_sector <- run_metric3_sector()
