# 02_metric1.R -----------------------------------------------------------------
# Compute and summarise Metric 1 for the 2025-26 pilot.

compute_metric1 <- function(las, september_cpi) {
  cpi_row <- september_cpi |>
    filter(year == cfg$analysis_year)
  
  if (nrow(cpi_row) != 1) {
    stop(
      "No unique September CPI observation for ",
      cfg$analysis_year,
      ".",
      call. = FALSE
    )
  }
  
  out <- las |>
    mutate(
      year = cfg$analysis_year,
      financial_year = cfg$financial_year,
      
      # QA CALLOUT: sign convention ---------------------------------------------
      # Lines 3 and 4 are filed on NNDR3 as deductions, so apply_writeoff_sign()
      # in 01_init.R has already flipped them. After that flip a POSITIVE value
      # is a write-off and a NEGATIVE value is a write-back. Do not flip again.
      net_writeoffs_nominal = wo_allowance + wo_excess,
      
      # QA CALLOUT: real-terms deflation ----------------------------------------
      # cpi_deflator is September-CPI-of-analysis-year rebased to the September
      # of cfg$cpi_price_year (see build_september_cpi() in 01_init.R). One
      # deflator applies to every authority, so real values preserve the
      # cross-sectional shape exactly; deflation only matters across years.
      # See caveats C18 to C23 for the basis, and note that changing
      # cfg$cpi_price_year restates every real figure already circulated.
      september_cpi = cpi_row$september_cpi,
      cpi_deflator = cpi_row$cpi_deflator,
      price_basis = cpi_row$price_basis,
      
      wo_allowance_real = wo_allowance * cpi_deflator,
      wo_excess_real = wo_excess * cpi_deflator,
      net_writeoffs_real = net_writeoffs_nominal * cpi_deflator,
      
      # Temporary compatibility alias for chart code.
      net_writeoffs = net_writeoffs_nominal,
      
      # QA CALLOUT: there is deliberately no "real" version of this rate ---------
      # Interim contextual rate, not the preferred NCD-based rate. Numerator and
      # denominator are both nominal and same-year, so applying the deflator to
      # both cancels: a real rate would be identical to this one. Do not create a
      # writeoff_rate_nrp_real, and never divide net_writeoffs_real by nominal
      # net_rates_payable - that mixes price bases and inflates the rate.
      writeoff_rate_nrp = if_else(
        net_rates_payable > 0,
        net_writeoffs_nominal / net_rates_payable,
        NA_real_
      ),
      
      negative_net_writeoff = net_writeoffs_nominal < 0,
      
      interpretation = case_when(
        net_writeoffs_nominal < 0 ~ "Net write-back",
        net_writeoffs_nominal == 0 ~ "No net write-off recorded",
        TRUE ~ "Positive net write-off"
      ),
      
      maturity_status = "Recent-period write-off observation"
    )
  
  england_total <- sum(out$net_writeoffs_nominal)
  
  # QA CALLOUT: scalar guard, not a row-wise test --------------------------------
  # england_total is one number for the whole of England (length 1), while
  # net_writeoffs_nominal is one value per billing authority (length 296).
  # dplyr::if_else() requires the condition, `true` and `false` to be the same
  # length, so a length-1 condition with a length-296 `true` branch fails with
  # "Can't recycle `true` (size 296) to size 1".
  #
  # Base `if` is the correct construct: the guard is evaluated once for the whole
  # column rather than row by row. Rule of thumb for this codebase - if the
  # condition does not vary by authority, do not use if_else().
  #
  # Contrast with writeoff_rate_nrp above, where net_rates_payable > 0 IS
  # evaluated per authority and if_else() is correct.
  share_denominator_valid <- england_total != 0
  
  # Disabled features announce themselves rather than returning silent NA.
  if (!share_denominator_valid) {
    warning(
      "England net write-offs total zero; share_of_england_net_writeoffs ",
      "set to NA for all authorities.",
      call. = FALSE
    )
  }
  
  out |>
    mutate(
      share_of_england_net_writeoffs = if (share_denominator_valid) {
        net_writeoffs_nominal / england_total
      } else {
        NA_real_
      }
    ) |>
    arrange(desc(net_writeoffs_nominal))
}

qa_metric1 <- function(metric1) {
  list(
    coverage = metric1 |>
      summarise(
        financial_year = first(financial_year),
        n_authorities = n(),
        n_positive_net_writeoffs = sum(net_writeoffs_nominal > 0),
        n_negative_net_writeoffs = sum(net_writeoffs_nominal < 0),
        n_zero_net_writeoffs = sum(net_writeoffs_nominal == 0),
        n_zero_or_negative_denominators = sum(net_rates_payable <= 0)
      ),
    
    duplicate_codes = metric1 |>
      count(la_code, name = "n") |>
      filter(n > 1),
    
    negative_net_writeoffs = metric1 |>
      filter(negative_net_writeoff) |>
      select(
        la_code,
        la_name,
        wo_allowance,
        wo_excess,
        net_writeoffs_nominal,
        interpretation
      ),
    
    bad_denominators = metric1 |>
      filter(net_rates_payable <= 0) |>
      select(
        la_code,
        la_name,
        net_rates_payable,
        net_writeoffs_nominal
      ),
    
    largest_absolute_entries = metric1 |>
      mutate(absolute_net_writeoffs = abs(net_writeoffs_nominal)) |>
      arrange(desc(absolute_net_writeoffs)) |>
      slice_head(n = 20) |>
      select(
        la_code,
        la_name,
        net_writeoffs_nominal,
        net_writeoffs_real,
        wo_allowance,
        wo_excess,
        net_rates_payable,
        writeoff_rate_nrp
      )
  )
}

metric1_national <- function(metric1) {
  # QA CALLOUT: capture row-level vectors BEFORE summarise() ---------------------
  # summarise() evaluates its arguments in order, and each one can see the ones
  # defined before it. The first argument below rebinds net_writeoffs_nominal to
  # a single England total, so every later reference to that NAME gets the
  # scalar, not the 296-value column. That silently produced
  # n_negative_net_writeoffs = 0, n_positive_net_writeoffs = 1, net_writebacks =
  # 0 and positive_writeoffs = the net total.
  #
  # Any statistic that must be computed ACROSS authorities is therefore read from
  # la_net below,
  #
  # Cross-check: these counts must equal the equivalent fields in
  # qa_metric1(metric1)$coverage, which computes them without any rebinding.
  la_net <- metric1$net_writeoffs_nominal
  
  metric1 |>
    summarise(
      financial_year = first(financial_year),
      net_writeoffs_nominal = sum(net_writeoffs_nominal),
      net_writeoffs_real = sum(net_writeoffs_real),
      net_writeoffs = net_writeoffs_nominal,
      wo_allowance = sum(wo_allowance),
      wo_excess = sum(wo_excess),
      wo_allowance_real = sum(wo_allowance_real),
      wo_excess_real = sum(wo_excess_real),
      positive_writeoffs = sum(pmax(la_net, 0)),
      net_writebacks = sum(pmin(la_net, 0)),
      n_writeback_authorities = sum(la_net < 0),
      net_rates_payable = sum(net_rates_payable),
      writeoff_rate_nrp = net_writeoffs_nominal / net_rates_payable,
      september_cpi = first(september_cpi),
      cpi_deflator = first(cpi_deflator),
      price_basis = first(price_basis),
      # QA CALLOUT: this previously read cfg$cpi_source_url, which 00_config.R
      # did not define. Assigning NULL inside summarise() DROPS the column
      # If a provenance column goes missing from an output, check the cfg name
      # first. 
      cpi_series = cfg$cpi_series_id,
      cpi_source = cfg$cpi_source_label,
      cpi_source_url = cfg$cpi_source_url,
      n_authorities = n(),
      n_positive_net_writeoffs = sum(la_net > 0),
      n_negative_net_writeoffs = sum(la_net < 0),
      numerator_source = "NNDR3",
      contextual_denominator_source = "NNDR3 net rates payable",
      agreed_rate_available = FALSE,
      agreed_denominator_required = "QRC4 net collectable debit",
      scope = paste(
        "Billed business rates debt written off as irrecoverable;",
        "not all debt remaining unpaid at year end"
      )
    )
}

metric1_concentration_screen <- function(metric1, top_share = 0.10) {
  if (
    !is.numeric(top_share) ||
    length(top_share) != 1 ||
    top_share <= 0 ||
    top_share >= 1
  ) {
    stop("top_share must be between 0 and 1.", call. = FALSE)
  }
  
  n_top <- max(1L, ceiling(nrow(metric1) * top_share))
  
  ranked <- metric1 |>
    mutate(
      rank_cash = min_rank(desc(net_writeoffs_nominal)),
      rank_contextual_rate = min_rank(desc(writeoff_rate_nrp)),
      top_decile_cash = rank_cash <= n_top,
      top_decile_contextual_rate = rank_contextual_rate <= n_top
    )
  
  total_positive <- sum(pmax(ranked$net_writeoffs_nominal, 0))
  
  safe_share <- function(filter_vector) {
    if (total_positive == 0) return(NA_real_)
    
    sum(
      pmax(ranked$net_writeoffs_nominal[filter_vector], 0)
    ) / total_positive
  }
  
  summary <- tibble(
    screen = c(
      "Top decile by net write-off cash amount",
      "Top decile by NNDR3 contextual rate"
    ),
    n_authorities = c(
      sum(ranked$top_decile_cash),
      sum(ranked$top_decile_contextual_rate)
    ),
    share_of_positive_writeoffs = c(
      safe_share(ranked$top_decile_cash),
      safe_share(ranked$top_decile_contextual_rate)
    ),
    interpretation = c(
      "Single-year cash concentration; affected by authority size",
      "Single-year contextual-rate screen; not the agreed NCD rate"
    )
  )
  
  list(ranked = ranked, summary = summary)
}


# Panel computation ============================================================
# Multi-year equivalent of compute_metric1(). 

compute_metric1_panel <- function(panel_las, september_cpi) {
  panel_years <- sort(unique(panel_las$year))
  cpi <- september_cpi |>
    select(year, september_cpi, cpi_deflator, price_basis)
  
  # QA CALLOUT: every panel year must carry a deflator ---------------------------
  # A left join would leave NA deflators and produce NA real figures that look
  # like genuine zeros in a chart. Halt instead. See caveat C20.
  missing_cpi <- setdiff(panel_years, cpi$year)
  if (length(missing_cpi) > 0) {
    stop("No September CPI observation for panel year(s): ",
         paste(missing_cpi, collapse = ", "),
         ". Real-terms series cannot be produced.", call. = FALSE)
  }
  
  out <- panel_las |>
    left_join(cpi, by = "year") |>
    mutate(
      # Sign already applied once in apply_writeoff_sign(). Positive = write-off.
      net_writeoffs_nominal = wo_allowance + wo_excess,
      wo_allowance_real = wo_allowance * cpi_deflator,
      wo_excess_real = wo_excess * cpi_deflator,
      net_writeoffs_real = net_writeoffs_nominal * cpi_deflator,
      net_writeoffs = net_writeoffs_nominal,
      writeoff_rate_nrp = if_else(
        net_rates_payable > 0,
        net_writeoffs_nominal / net_rates_payable,
        NA_real_
      ),
      # C13: the most recent years are structurally immature because write-offs
      # lag the non-payment they represent. Flag, do not drop.
      immature = year > (cfg$analysis_year - cfg$immature_years),
      # Ben's steer: include COVID years so the effect is visible, flagged, and
      # excluded from trend rather than deleted from the descriptive series.
      covid_affected = year %in% cfg$covid_years
    ) |>
    arrange(year, desc(net_writeoffs_nominal))
  
  stopifnot(!any(is.na(out$cpi_deflator)))
  out
}

# National series, one row per year. Uses the same la_net guard as
# metric1_national() to avoid the summarise() shadowing bug.
metric1_national_panel <- function(metric1_panel) {
  metric1_panel |>
    group_by(year, financial_year) |>
    group_modify(\(d, k) {
      la_net <- d$net_writeoffs_nominal
      tibble(
        n_authorities = nrow(d),
        net_writeoffs_nominal = sum(la_net),
        net_writeoffs_real = sum(d$net_writeoffs_real),
        wo_allowance = sum(d$wo_allowance),
        wo_excess = sum(d$wo_excess),
        positive_writeoffs = sum(pmax(la_net, 0)),
        net_writebacks = sum(pmin(la_net, 0)),
        n_writeback_authorities = sum(la_net < 0),
        net_rates_payable = sum(d$net_rates_payable),
        writeoff_rate_nrp = sum(la_net) / sum(d$net_rates_payable),
        cpi_deflator = first(d$cpi_deflator),
        price_basis = first(d$price_basis),
        covid_affected = first(d$covid_affected),
        immature = first(d$immature)
      )
    }) |>
    ungroup() |>
    arrange(year)
}