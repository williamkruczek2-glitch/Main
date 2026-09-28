# 13_run_metric3.R --------------------------------------------------------------
# Metric 3 (arrears) runner across 2013-14 to 2025-26. Draws from the master
# table via 10_metric2.R (which holds the shared base) and 12_metric3.R.
#
# Collection rate (QRC4, C16) is NOT built here - open decision with Toby.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("06_metric1.R")
source("10_metric2.R")
source("12_metric3.R")

run_metric3 <- function() {
  base <- build_metrics23_base()
  national <- metrics23_national(base)
  
  write_csv(national, file.path(cfg$dir_output, "metric3_national.csv"))
  
  charts <- build_metric3_charts(national)
  print_metric3_charts(charts)
  chart_paths <- save_metric3_charts(charts)
  
  # Verification callouts ------------------------------------------------------
  message("")
  message(strrep("=", 74))
  message("METRIC 3 CHART BUILD")
  message(strrep("=", 74))
  message("Years: ", paste(national$financial_year, collapse = ", "))
  message("Source: master table (2018-26).")
  
  message("")
  message("[1] Arrears outstanding and arrears as a share of net rates payable")
  print(national |>
          transmute(financial_year,
                    arrears_share = fmt_prop(arrears / net_rates_payable),
                    arrears = fmt_gbp_m(arrears)),
        n = Inf, width = Inf)
  
  # QA CALLOUT: sign sanity.
  # QA CALLOUT: na.rm is required. Years where the field is genuinely
  # unavailable now carry NA (see sum_or_na / C27), and any(NA < 0)
  # returns NA, which if() cannot evaluate.
  if (any(national$arrears < 0, na.rm = TRUE)) {
    warning("Arrears is negative for some year - check sign.", call. = FALSE)
  }
  
  message("")
  message("[2] Charts written to ", cfg$dir_charts)
  iwalk(chart_paths, \(p, n) message("    ", n, " -> ", basename(p)))
  
  message("")
  message(strrep("=", 74))
  message("Metric 3 arrears charts built. Collection rate (QRC4, C16) NOT built")
  message("- that half of the metric awaits the QRC4 decision. No trend fitted.")
  message(strrep("=", 74))
  
  invisible(list(base = base, national = national,
                 charts = charts, chart_paths = chart_paths))
}

res_metric3 <- run_metric3()