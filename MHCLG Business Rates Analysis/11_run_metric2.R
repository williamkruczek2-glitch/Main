# 11_run_metric2.R --------------------------------------------------------------
# Metric 2 (change in allowance for non-collection) runner across 2013-14 to
# 2025-26. Draws from the master table via 10_metric2.R.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("06_metric1.R")
source("10_metric2.R")

run_metric2 <- function() {
  base <- build_metrics23_base()
  national <- metrics23_national(base)
  
  write_csv(national, file.path(cfg$dir_output, "metric2_national.csv"))
  
  charts <- build_metric2_charts(national)
  print_metric2_charts(charts)
  chart_paths <- save_metric2_charts(charts)
  
  # Verification callouts ------------------------------------------------------
  message("")
  message(strrep("=", 74))
  message("METRIC 2 CHART BUILD")
  message(strrep("=", 74))
  message("Years: ", paste(national$financial_year, collapse = ", "))
  message("Source: master table (2018-26).")
  
  message("")
  message("[1] Allowance closing balance, realised write-offs, and the change")
  print(national |>
          transmute(financial_year,
                    allowance_cb = fmt_gbp_m(allowance_cb),
                    net_writeoffs = fmt_gbp_m(net_writeoffs),
                    allowance_change = fmt_gbp_m(allowance_change)),
        n = Inf, width = Inf)
  
  # QA CALLOUT: sign sanity. The allowance is filed negative and negated on
  # load; assert the closing balance came out positive.
  # QA CALLOUT: na.rm is required. Years where the field is genuinely
  # unavailable now carry NA (see sum_or_na / C27), and any(NA < 0)
  # returns NA, which if() cannot evaluate.
  if (any(national$allowance_cb < 0, na.rm = TRUE)) {
    warning("Allowance closing balance is negative for some year - check sign.",
            call. = FALSE)
  }
  
  message("")
  message("[2] Charts written to ", cfg$dir_charts)
  iwalk(chart_paths, \(p, n) message("    ", n, " -> ", basename(p)))
  
  message("")
  message(strrep("=", 74))
  message("Metric 2 charts built from the master table. No trend fitted.")
  message(strrep("=", 74))
  
  invisible(list(base = base, national = national,
                 charts = charts, chart_paths = chart_paths))
}


res_metric2 <- run_metric2()

getwd()
any(grepl("col_character", readLines("02_master_table.R")))
