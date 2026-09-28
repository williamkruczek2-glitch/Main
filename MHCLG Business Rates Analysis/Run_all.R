
# --- Definitions (order matters) ---------------------------------------------
source("00_config.R")
source("01_init.R")
source("02_master_table.R")
source("04_metric1_maps.R")
source("06_metric1.R")
source("08_metric1_panel_charts.R")
source("10_metric2.R")
source("12_metric3.R")

# --- Runners (each is idempotent; each writes its own outputs) ----------------
# Sourced individually so we can capture their return objects.
run_all <- function() {
  message("\n", strrep("#", 74))
  message("# FULL PIPELINE RUN: master table -> pilot -> M1 panel -> M2 -> M3")
  message(strrep("#", 74), "\n")
  
  # Import runner definitions without executing their tail `res <- ...` calls
  # by isolating them. Each of these files ends with `res_* <- run_*()`, so
  # sourcing them runs the pipeline exactly once each.
  source("03_run_master.R",         local = FALSE)
  source("05_run_metric1_pilot.R",  local = FALSE)
  source("09_run_metric1.R",        local = FALSE)
  source("11_run_metric2.R",        local = FALSE)
  source("13_run_metric3.R",        local = FALSE)
  source("14_stock_of_properties.R",     local = FALSE)
  source("15_metric3_sector.R",          local = FALSE)
  source("16_metric3_arrears_drivers.R", local = FALSE)
  source("17_demography.R",              local = FALSE)
  source("18_metric_outliers.R",         local = FALSE)
  
  message("\n", strrep("#", 74))
  message("# FULL PIPELINE COMPLETE. Outputs in ", cfg$dir_output, "/")
  message(strrep("#", 74))
}

run_all()



message("[4] Line 3 vs Line 4 split, national")
print(
  res_metric1_panel$national |>
    select(financial_year, wo_allowance, wo_excess,
           net_writeoffs = net_writeoffs_nominal) |>
    mutate(wo_allowance = fmt_gbp_m(wo_allowance),
           wo_excess = fmt_gbp_m(wo_excess),
           net_writeoffs = fmt_gbp_m(net_writeoffs))
)

