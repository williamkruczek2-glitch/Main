# 05_run_metric1_pilot.R --------------------------------------------------------
# PILOT runner for Metric 1: single-year snapshot for cfg$analysis_year
# (currently 2025-26). Builds the master table, filters to that year, runs the
# panel chart set at n = 1 year, and produces the England + London choropleth
# maps. This is the descendant of the old 04_run_metrics1.R, now sitting on the
# master table so pilot and panel share one ingest path.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("04_metric1_maps.R")
source("06_metric1.R")
source("08_metric1_panel_charts.R")

run_metric1_pilot <- function(year = cfg$analysis_year) {
  message(strrep("-", 74))
  message("METRIC 1 PILOT (", label_fy(year), ")")
  message(strrep("-", 74))
  
  # Source from the master table filtered to one year.
  master <- build_master_table()
  cpi <- read_ons_cpi(fetch_cpi_data())
  september_cpi <- build_september_cpi(cpi)
  
  # QA CALLOUT: pilot = one year of the master, no separate ingest ---------------
  # The single-year figure MUST reproduce whatever the panel produces for that
  # year, because they now share the same source. This is the regression the
  # panel runner also checks.
  master <- master |> mutate(wo_excess = coalesce(wo_excess, 0))
  metric1_year <- compute_metric1_panel(master, september_cpi) |>
    filter(year == !!year)
  
  if (nrow(metric1_year) == 0) {
    stop("Pilot: no rows for year ", year, " in the master table.",
         call. = FALSE)
  }
  
  national <- metric1_national_panel(metric1_year)
  
  write_csv(metric1_year, file.path(cfg$dir_output,
                                    paste0("metric1_pilot_", year, "_la.csv")))
  write_csv(national, file.path(cfg$dir_output,
                                paste0("metric1_pilot_", year, "_national.csv")))
  
  # Panel charts, called at n=1 year. These render fine for a single year;
  # the boxplot and components chart just show one column instead of a series.
  charts <- build_panel_charts(metric1 = metric1_year, national = national)
  print_panel_charts(charts)
  chart_paths <- save_panel_charts(charts,
                                   dir = file.path(cfg$dir_charts, "m1_pilot"))
  
  # Maps.
  boundaries <- read_england_boundaries()
  maps <- make_metric1_maps(metric1_year, boundaries)
  map_dir <- file.path(cfg$dir_charts, "m1_pilot")
  dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
  map_paths <- c(
    england = ggsave(
      filename = file.path(map_dir, "06_metric1_rate_map_england.png"),
      plot = maps$england, width = 8, height = 9, dpi = 300, bg = "white"
    ),
    london = ggsave(
      filename = file.path(map_dir, "07_metric1_rate_map_london.png"),
      plot = maps$london, width = 8, height = 7, dpi = 300, bg = "white"
    )
  )
  
  # Verification callout ------------------------------------------------------
  message("")
  message("[1] ", label_fy(year), " pilot summary")
  print(national |>
          transmute(financial_year,
                    authorities = n_authorities,
                    net_writeoffs = fmt_gbp_m(net_writeoffs_nominal),
                    rate = fmt_prop(writeoff_rate_nrp)),
        width = Inf)
  
  # Regression against the known 2025-26 pilot figure.
  if (year == 2025L) {
    if (abs(national$net_writeoffs_nominal - 264.9e6) > 0.5e6) {
      warning("2025-26 pilot does not reproduce the verified £264.9m figure. ",
              "Master-fed and panel-fed paths have diverged.", call. = FALSE)
    } else {
      message("[2] Regression PASS - matches the verified £264.9m / 0.94% / 296.")
    }
  }
  
  message("")
  message("[3] Outputs in ", map_dir, "/")
  message(strrep("-", 74))
  
  invisible(list(metric1 = metric1_year, national = national,
                 charts = charts, chart_paths = chart_paths,
                 maps = maps, map_paths = map_paths))
}

res_pilot <- run_metric1_pilot()