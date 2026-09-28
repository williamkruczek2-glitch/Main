# 03_run_master.R ---------------------------------------------------------------
# Runner for the master table build. Sources config -> init -> master_table,
# then builds and verifies the 13-year panel across all metrics' fields.

source("00_config.R")
source("01_init.R")
source("02_master_table.R")

run_master <- function() {
  
  master <- build_master_table(cfg$master_years)
  
  # Attach CPI so real terms are available, using the existing machinery.
  cpi <- read_ons_cpi(fetch_cpi_data())
  september_cpi <- build_september_cpi(cpi) |>
    select(year, september_cpi, cpi_deflator, price_basis)
  
  master <- master |>
    left_join(september_cpi, by = "year") |>
    mutate(
      net_writeoffs_nominal = wo_allowance + coalesce(wo_excess, 0),
      net_writeoffs_real = net_writeoffs_nominal * cpi_deflator,
      writeoff_rate_nrp = if_else(net_rates_payable > 0,
                                  net_writeoffs_nominal / net_rates_payable,
                                  NA_real_)
    )
  
  # Per-year national summary for the checks below.
  national <- master |>
    group_by(year, financial_year) |>
    group_modify(\(d, k) {
      la_net <- d$net_writeoffs_nominal
      tibble(
        n_authorities = nrow(d),
        net_writeoffs_nominal = sum(la_net),
        net_writeoffs_real = sum(d$net_writeoffs_real),
        wo_allowance = sum(d$wo_allowance),
        wo_excess = sum(d$wo_excess, na.rm = TRUE),
        net_rates_payable = sum(d$net_rates_payable),
        writeoff_rate_nrp = sum(la_net) / sum(d$net_rates_payable),
        cpi_deflator = first(d$cpi_deflator)
      )
    }) |>
    ungroup() |>
    arrange(year)
  
  # Outputs
  write_csv(master, file.path(cfg$dir_output, "master_table_m123.csv"))
  write_csv(national, file.path(cfg$dir_output, "master_national_summary.csv"))
  
  # ---- Verification callouts -------------------------------------------------
  
  message("")
  message(strrep("=", 78))
  message("MASTER TABLE VERIFICATION")
  message(strrep("=", 78))
  
  message("")
  message("[1] Years built")
  message("    ", paste(cfg$master_years, collapse = ", "),
          "  (positional years 2013-17 pending)")
  
  message("")
  message("[2] Row count reconciles to sum of years")
  expected <- sum(national$n_authorities)
  actual <- nrow(master)
  message("    sum of per-year authority counts: ", expected)
  message("    rows in master table:             ", actual)
  if (expected != actual) stop("Master row count mismatch.", call. = FALSE)
  message("    PASS")
  
  message("")
  message("[3] No duplicate authority-year keys")
  dupes <- master |> count(year, la_code) |> filter(n > 1)
  if (nrow(dupes) > 0) { print(dupes); stop("Duplicate keys.", call. = FALSE) }
  message("    PASS - ", nrow(master), " unique authority-year rows")
  
  message("")
  message("[4] Authority counts by year (reorganisations expected)")
  walk2(national$year, national$n_authorities,
        \(y, n) message("    ", y, ": ", n))
  
  message("")
  message("[5] Allowance identity (D2 Line 3 vs D5 noncoll_chrgd, magnitudes)")
  check_allowance_identity(master)
  
  message("")
  message("[6] Metric 1 field completeness")
  m1_na <- master |>
    summarise(across(c(net_rates_payable, wo_allowance, net_writeoffs_nominal),
                     \(x) sum(is.na(x))))
  print(m1_na, width = Inf)
  # wo_excess is legitimately NA for positional years; not checked here.
  
  message("")
  message("[7] National Metric 1 series (nominal)")
  print(national |>
          transmute(financial_year, n_authorities,
                    net_writeoffs = fmt_gbp_m(net_writeoffs_nominal),
                    rate = fmt_prop(writeoff_rate_nrp)),
        n = Inf, width = Inf)
  
  message("")
  message("[8] Coverage of Metric 2 / Metric 3 fields by year")
  cov <- master |>
    group_by(year) |>
    summarise(
      m2_change = sum(!is.na(change_noncoll)),
      m2_allowance = sum(!is.na(noncoll_cb)),
      m3_arrears = sum(!is.na(sums_outstanding)),
      .groups = "drop"
    )
  print(cov, n = Inf, width = Inf)
  message("    Zeros here flag years where M2/M3 cannot yet be built.")
  
  message("")
  message(strrep("=", 78))
  message("Master table built for machine-name years. Denominator is NNDR3 net")
  message("rates payable throughout (C16 open). No trend claim is made.")
  message(strrep("=", 78))
  
  invisible(list(master = master, national = national))
}

res_master <- run_master()
master <- build_master_table()
View(master)
any(grepl("id_row_idx", readLines("01_init.R")))          # must be TRUE
any(grepl("implausibly few", readLines("01_init.R")))     # must be TRUE


