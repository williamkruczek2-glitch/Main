# 09_run_metric1.R --------------------------------------------------------------
# Metric 1 PANEL runner across the full 13-year series (2013-14 to 2025-26).
# Draws from the master table so it shares one ingest path with M2/M3.
# The pilot (05_run_metric1_pilot.R) is a one-year filter of the same source.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("04_metric1_maps.R")
source("06_metric1.R")
source("08_metric1_panel_charts.R")

run_metric1_panel <- function() {
  
  # Source from the master table -----------------------------------------------
  # QA CALLOUT: single ingest path -----------------------------------------------
  # Metric 1 now reads the same build_master_table() output as M2/M3. The old
  # load_metric1_source() path (single year) is retained only for the 04 pilot.
  master <- build_master_table()
  
  cpi <- read_ons_cpi(fetch_cpi_data())
  september_cpi <- build_september_cpi(cpi)
  
  # QA CALLOUT: missing Line 4 is treated as zero for the metric, not NA --------
  # The positional years (2013-17) have no writeoffs_excnoncoll (Line 4) - it did
  # not exist in that form vintage. wo_allowance + NA would blank the whole year.
  # Coalescing Line 4 to zero lets those years carry a net figure equal to Line 3
  # alone, which is correct: net write-offs for those years ARE just Line 3.
  # A caveat records that the Line 3 / Line 4 split is unavailable pre-2018.
  master <- master |>
    mutate(wo_excess = coalesce(wo_excess, 0))
  
  metric1 <- compute_metric1_panel(master, september_cpi)
  national <- metric1_national_panel(metric1)
  
  write_csv(metric1, file.path(cfg$dir_output, "metric1_panel_master_la.csv"))
  write_csv(national, file.path(cfg$dir_output, "metric1_panel_master_national.csv"))
  
  # Charts ---------------------------------------------------------------------
  # QA CALLOUT: identical plot logic to the original panel charts -----------------
  # These are the SAME five functions Metric 1 has always used (now in
  # 03_metric1_panel_charts.R). The only change is the data: they are fed the
  # master-table series spanning 2018-26 rather than the old 2020-26 panel. No
  # chart code is altered. Charts save to charts/m1/ to match m2/m3.
  charts <- build_panel_charts(metric1 = metric1, national = national)
  print_panel_charts(charts)
  chart_paths <- save_panel_charts(charts)
  
  # Verification callouts ------------------------------------------------------
  message("")
  message(strrep("=", 74))
  message("METRIC 1 PANEL BUILD (from master table)")
  message(strrep("=", 74))
  message("Years: ", paste(national$financial_year, collapse = ", "))
  message("Source: master table (2018-26 currently; positional years pending).")
  
  message("")
  message("[1] National Metric 1 series")
  print(national |>
          transmute(financial_year,
                    n_authorities,
                    net_writeoffs = fmt_gbp_m(net_writeoffs_nominal),
                    net_writeoffs_real = fmt_gbp_m(net_writeoffs_real),
                    rate = fmt_prop(writeoff_rate_nrp)),
        n = Inf, width = Inf)
  
  # QA CALLOUT: regression against the verified 2025-26 pilot --------------------
  # The master-fed panel MUST reproduce the single-year pilot figure for 2025-26.
  # If it does not, the two ingest paths disagree and the master table has drifted
  # from load_metric1_source().
  pilot <- national |> filter(year == cfg$analysis_year)
  if (nrow(pilot) == 1) {
    message("")
    message("[2] Regression check against the 2025-26 pilot")
    message("    net write-offs: ", fmt_gbp_m(pilot$net_writeoffs_nominal),
            "  (pilot: £264.9m)")
    message("    rate:           ", fmt_prop(pilot$writeoff_rate_nrp),
            "  (pilot: 0.94%)")
    message("    authorities:    ", pilot$n_authorities, "  (pilot: 296)")
    if (abs(pilot$net_writeoffs_nominal - 264.9e6) > 0.1e6) {
      warning("2025-26 net write-offs differ from the verified pilot. ",
              "The master and single-year paths disagree.", call. = FALSE)
    } else {
      message("    PASS - master-fed panel matches the pilot.")
    }
  }
  
  message("")
  message("[3] Charts (2018-26, same plot logic, saved to charts/m1/)")
  iwalk(chart_paths, \(p, n) message("    ", n, " -> ", basename(p)))
  
  message("")
  message(strrep("=", 74))
  message("Metric 1 panel built from the master table. 04 pilot unchanged.")
  message("No trend fitted. Denominator is NNDR3 net rates payable (C16 open).")
  message(strrep("=", 74))
  
  invisible(list(panel = metric1, national = national,
                 master = master, september_cpi = september_cpi,
                 charts = charts, chart_paths = chart_paths))
}

res_metric1_panel <- run_metric1_panel()

setdiff(as.character(cfg$master_years), names(cfg$year_formats))
setdiff(names(cfg$year_formats), as.character(cfg$master_years))