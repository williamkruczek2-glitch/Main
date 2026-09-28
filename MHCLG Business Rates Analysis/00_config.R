# 00_config.R ------------------------------------------------------------------
# Configuration and MHCLG styling for Metric 1: billed business rates debt
# ultimately written off. Full-series by design (2013-14 to 2025-26), while
# allowing partial runs as historic files and year-specific mappings are added.

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(grid)
  library(sf)
  library(lubridate)
})

# Configuration ----------------------------------------------------------------

cfg <- list(
  dir_data = "data",
  dir_lookup = "lookup",
  dir_output = "_outputs_",
  dir_charts = file.path("_outputs_", "charts"),
  
  year_min = 2013L,
  year_max = 2025L,
  analysis_year = 2025L,
  financial_year = "2025-26",
  require_complete_panel = FALSE,
  min_years_for_trend = 2L,
  rolling_years = 3L,
  immature_years = 3L,
  
  structural_breaks = c(
    retention_start = 2013L,
    revaluation_2017 = 2017L,
    covid_start = 2020L,
    covid_recovery = 2022L,
    revaluation_2023 = 2023L
  ),
  covid_years = c(2020L, 2021L),
  include_covid_in_descriptive = TRUE,
  exclude_covid_from_trend = TRUE,
  
  england_code = "E0000",
  aggregate_codes = c("E0000"),
  sign_flip = TRUE,
  
  values_file = "nndr3_25_26.csv",
  metadata_file = "nndr3_25_26_meta.csv",
  
  agreed_denominator = "qrc4_net_collectable_debit",
  contextual_denominator = "nndr3_net_rates_payable",
  source_label = "MHCLG, NNDR3 outturn returns",
  
  # ONS CPI All Items Index, 2015=100, series D7BT.
  # Local file used for reproducibility rather than a live ONS pull, so a rerun
  # months later reproduces the same real series.
  #
  # QA CALLOUT: why September, and why one month rather than a financial-year
  # average. September CPI is the statutory reference point for business rates:
  # the multiplier for year t+1 is uprated by the September of year t. Using the
  # same month keeps the deflator conceptually aligned with how the tax base
  # itself is uprated. September also sits close to the midpoint of the April to
  # March financial year. It is NOT the start of the tax year. See caveat C18.
  #
  # cpi_price_year sets the price basis for every real figure the pipeline
  # produces. Changing it restates all published real numbers - see caveat C21.
  cpi_series_id = "D7BT",
  cpi_file = "series-260826.csv",
  cpi_month = 9L,
  cpi_price_year = 2025L,
  cpi_source_label = paste(
    "ONS CPI All Items Index, 2015=100, series D7BT;",
    "local file series-260826.csv"
  ),
  cpi_source_url = paste0(
    "https://www.ons.gov.uk/economy/inflationandpriceindices/",
    "timeseries/d7bt/mm23"
  ),
  
  # ONS December 2024 LAD boundaries, BGC generalised product.
  boundary_url = paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
    "Local_Authority_Districts_December_2024_Boundaries_UK_BGC/",
    "FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
  ),
  boundary_code = "LAD24CD",
  boundary_name = "LAD24NM",
  london_code_prefix = "E09",
  map_bins = 5L,
  
  backcast_lookup = file.path("lookup", "la_backcast_lookup.csv"),
  apply_backcast = TRUE,
  
  england_validation_tolerance = 1,
  suffix_validation_tolerance = 1
)

walk(
  c(cfg$dir_data, cfg$dir_lookup, cfg$dir_output, cfg$dir_charts),
  \(path) dir.create(path, recursive = TRUE, showWarnings = FALSE)
)

# Publication pages -------------------------------------------------------------

cfg$landing_urls <- setNames(
  paste0(
    "https://www.gov.uk/government/statistics/",
    c(
      "national-non-domestic-rates-collected-by-councils-in-england-2013-to-2014",
      "national-non-domestic-rates-collected-by-councils-in-england-2014-to-2015",
      "national-non-domestic-rates-collected-by-councils-in-england-2015-to-2016",
      "national-non-domestic-rates-collected-by-councils-in-england-2016-to-2017",
      "national-non-domestic-rates-collected-by-councils-in-england-2017-to-2018",
      "national-non-domestic-rates-collected-by-councils-in-england-2018-to-2019",
      "national-non-domestic-rates-collected-by-councils-in-england-2019-to-2020-provisional-data",
      "national-non-domestic-rates-collected-by-councils-in-england-2020-to-2021",
      "national-non-domestic-rates-collected-by-councils-in-england-2021-to-2022",
      "national-non-domestic-rates-collected-by-councils-in-england-2022-to-2023",
      "national-non-domestic-rates-collected-by-councils-in-england-2023-to-2024",
      "national-non-domestic-rates-collected-by-councils-in-england-2024-to-2025",
      "national-non-domestic-rates-collected-by-councils-in-england-2025-to-2026"
    )
  ),
  as.character(2013:2025)
)

cfg$format_era <- list(
  consolidated_csv = 2021:2025,
  dropdown_xlsx = 2013:2020
)

# MHCLG palette ----------------------------------------------------------------

mhclg_pal <- c(
  navy = "#12436D", teal = "#28A197", plum = "#801650",
  orange = "#F46A25", grey = "#3D3D3D", lilac = "#A285D1"
)

mhclg_grey <- c(
  text = "#0B0C0C", secondary = "#505A5F", gridline = "#E5E5E5",
  axis = "#B1B4B6", deemphasis = "#BFBFBF"
)

mhclg_col <- list(
  metric1 = unname(mhclg_pal[["navy"]]),
  metric2 = unname(mhclg_pal[["plum"]]),
  metric3 = unname(mhclg_pal[["teal"]]),
  provisional = "#8FA8BF",
  covid = "#EDEDED",
  highlight = unname(mhclg_pal[["orange"]]),
  deemphasis = unname(mhclg_grey[["deemphasis"]]),
  text = unname(mhclg_grey[["text"]]),
  secondary = unname(mhclg_grey[["secondary"]])
)

mhclg_map_seq <- c("#DCE6EE", "#A9C0D4", "#6E93B4", "#3C6C95", "#12436D")

scale_colour_mhclg <- function(...) scale_colour_manual(values = unname(mhclg_pal), ...)
scale_fill_mhclg <- function(...) scale_fill_manual(values = unname(mhclg_pal), ...)
scale_fill_mhclg_seq <- function(...) scale_fill_gradientn(colours = mhclg_map_seq, ...)

mhclg_focus <- function(levels, focus, aes_name = c("colour", "fill"),
                        focus_colour = mhclg_col$metric1, ...) {
  aes_name <- match.arg(aes_name)
  values <- setNames(ifelse(levels %in% focus, focus_colour,
                            mhclg_col$deemphasis), levels)
  if (aes_name == "fill") scale_fill_manual(values = values, ...)
  else scale_colour_manual(values = values, ...)
}

# Theme ------------------------------------------------------------------------

theme_mhclg <- function(base_size = 12,
                        base_family = getOption("mhclg.font", ""),
                        grid = c("y", "x", "both", "none"),
                        axis_line = TRUE) {
  grid <- match.arg(grid)
  gl <- element_line(colour = mhclg_grey[["gridline"]], linewidth = 0.4)
  
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = mhclg_col$text, family = base_family),
      plot.title = element_text(face = "bold", size = base_size * 1.3,
                                hjust = 0, colour = mhclg_col$text,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = base_size, hjust = 0,
                                   colour = mhclg_col$secondary,
                                   margin = margin(b = 14)),
      plot.caption = element_text(size = base_size * 0.75, hjust = 0,
                                  colour = mhclg_col$secondary,
                                  margin = margin(t = 14)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = margin(t = 8, r = 12, b = 8, l = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "both")) gl else element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "both")) gl else element_blank(),
      panel.border = element_blank(),
      axis.title = element_text(size = base_size * 0.9,
                                colour = mhclg_col$secondary),
      axis.text = element_text(size = base_size * 0.9,
                               colour = mhclg_col$secondary),
      axis.line.x = if (axis_line) element_line(
        colour = mhclg_grey[["axis"]], linewidth = 0.5
      ) else element_blank(),
      axis.line.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      legend.justification = "left",
      legend.key = element_blank(),
      legend.margin = margin(b = 6),
      legend.text = element_text(size = base_size * 0.9),
      strip.text = element_text(face = "bold", hjust = 0,
                                size = base_size * 0.95,
                                colour = mhclg_col$text)
    )
}

theme_mhclg_bar <- function(...) theme_mhclg(grid = "x", axis_line = FALSE, ...)

use_theme_mhclg <- function(base_size = 12) {
  theme_set(theme_mhclg(base_size))
  update_geom_defaults("line", list(colour = mhclg_col$metric1, linewidth = 0.9))
  update_geom_defaults("point", list(colour = mhclg_col$metric1, size = 2.4))
  update_geom_defaults("col", list(fill = mhclg_col$metric1))
  update_geom_defaults("bar", list(fill = mhclg_col$metric1))
  update_geom_defaults("text", list(colour = mhclg_col$text, size = 3.4))
  invisible(TRUE)
}

use_theme_mhclg()

# Captions and formatters -------------------------------------------------------

mhclg_caption <- function(extra = NULL, source = cfg$source_label,
                          scope = paste("Measures non-payment of billed liability only;",
                                        "excludes unbilled liability and relief misuse.")) {
  parts <- c(paste0("Source: ", source, "."), scope, extra)
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = "\n")
}

metric1_caption <- function(extra = NULL) {
  mhclg_caption(
    extra = extra,
    scope = paste("Measures billed business rates debt written off as irrecoverable;",
                  "excludes unbilled liability and relief misuse.")
  )
}

fmt_gbp_m <- function(x, dp = 1) paste0("£", comma(x / 1e6, accuracy = 10^-dp), "m")
fmt_gbp_bn <- function(x, dp = 1) paste0("£", comma(x / 1e9, accuracy = 10^-dp), "bn")
fmt_prop <- function(x, dp = 2) percent(x, accuracy = 10^-dp)
fmt_pct <- function(x, dp = 2) paste0(formatC(x, format = "f", digits = dp), "%")
label_gbp_m <- function(dp = 0) \(x) paste0("£", comma(x / 1e6, accuracy = 10^-dp), "m")
label_gbp_bn <- function(dp = 1) \(x) paste0("£", comma(x / 1e9, accuracy = 10^-dp), "bn")
label_prop <- function(dp = 1) percent_format(accuracy = 10^-dp)
label_pct <- function(dp = 2) \(x) paste0(formatC(x, format = "f", digits = dp), "%")
label_fy <- function(x) paste0(x, "-", substr(x + 1L, 3, 4))

annotate_covid <- function(years = cfg$covid_years, label = "COVID-affected") {
  list(
    annotate("rect", xmin = min(years) - 0.5, xmax = max(years) + 0.5,
             ymin = -Inf, ymax = Inf, fill = mhclg_col$covid),
    annotate("text", x = mean(years), y = Inf, label = label,
             vjust = 1.6, size = 3, colour = mhclg_col$secondary)
  )
}

# QA CALLOUT: discrete-axis variant ---------------------------------------------
# annotate_covid() assumes a CONTINUOUS x-axis (raw year values as coordinates).
# The bar and box-plot charts use aes(factor(year), ...) - a DISCRETE axis where
# each year is a category mapped to an integer POSITION (1, 2, 3, ...) in level
# order, not to its literal year value. Passing raw years to annotate() on a
# discrete axis places the shading at the wrong x-position. This variant takes
# the axis's own set of years and looks up each COVID year's position within it,
# so the shading lines up with the bars/boxes it is meant to cover. Same fill
# and text styling as annotate_covid() so the band reads identically wherever it
# appears. Returns an empty layer list (a no-op when added to a plot) if none of
# the covid years are present on this axis.
annotate_covid_discrete <- function(x_levels, covid_years,
                                    label = "COVID-affected") {
  levels_sorted <- sort(unique(x_levels))
  pos <- match(covid_years, levels_sorted)
  pos <- pos[!is.na(pos)]
  if (length(pos) == 0) return(list())
  list(
    annotate("rect", xmin = min(pos) - 0.5, xmax = max(pos) + 0.5,
             ymin = -Inf, ymax = Inf, fill = mhclg_col$covid),
    annotate("text", x = mean(pos), y = Inf, label = label,
             vjust = 1.6, size = 3, colour = mhclg_col$secondary)
  )
}

save_mhclg <- function(plot, filename,
                       size = c("slide", "note", "wide", "square"),
                       dir = cfg$dir_charts, dpi = 300) {
  size <- match.arg(size)
  dims <- switch(size, slide = c(10, 5.6), note = c(6.3, 3.8),
                 wide = c(12, 5), square = c(6, 6))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, filename)
  ggsave(path, plot, width = dims[1], height = dims[2], dpi = dpi, bg = "white")
  message("Written: ", path)
  invisible(path)
}

# Variable mappings ------------------------------------------------------------
# Add a mapping only after checking that year's values file and metadata.

vars_2025 <- c(
  la_code = "Ecode",
  la_name = "Local_Authority",
  net_rates_payable = "netrates_tot",
  wo_allowance = "writeoffs_noncoll_tot",
  wo_excess = "writeoffs_excnoncoll_tot"
)

optional_vars_2025 <- c(
  ons_code = "ONSCode",
  org_type = "OrgType",
  org_region = "OrgRegion",
  form_status = "FormStatus"
)

integrity_vars_2025 <- c(
  wo_allowance_ba = "writeoffs_noncoll_baa",
  wo_allowance_da = "writeoffs_noncoll_da",
  wo_allowance_total = "writeoffs_noncoll_tot"
)

var_map <- list("2025" = vars_2025)
optional_var_map <- list("2025" = optional_vars_2025)
integrity_var_map <- list("2025" = integrity_vars_2025)

# Panel column resolution ------------------------------------------------------
# QA CALLOUT: why candidates rather than a hardcoded map per year.
# NNDR3 consolidated CSVs keep the same variable names for the financial fields
# (netrates_tot, writeoffs_noncoll_tot, ...) but the IDENTIFIER columns have
# changed spelling across releases - the 2019-20 workbook uses "Ecode" and
# "LA name", the 2025-26 CSV uses "Ecode" and "Local_Authority". Rather than
# guess, resolve_var_map() in 01_init.R matches each canonical field against the
# ordered candidates below, PRINTS what it resolved for every year, and HALTS if
# any required field is unresolved. Nothing is silently dropped or guessed.
#
# If a year halts, run probe_nndr3_columns() and add the real column name to the
# front of the relevant vector. Do not widen a candidate list to something
# generic - a loose pattern that matches two columns is how the wrong series
# gets into a published chart.
cfg$var_candidates <- list(
  la_code           = c("Ecode", "ecode", "E-code", "LA Code", "la_code"),
  la_name           = c("Local_Authority", "LA name", "LA Name", "Local Authority",
                        "la_name", "Authority"),
  net_rates_payable = c("netrates_tot"),
  wo_allowance      = c("writeoffs_noncoll_tot"),
  wo_excess         = c("writeoffs_excnoncoll_tot")
)

cfg$optional_candidates <- list(
  ons_code    = c("ONSCode", "ONS_Code", "ons_code"),
  org_type    = c("OrgType", "Org_Type", "org_type"),
  org_region  = c("OrgRegion", "Org_Region", "org_region"),
  form_status = c("FormStatus", "Form_Status", "Status", "form_status")
)

cfg$integrity_candidates <- list(
  wo_allowance_ba    = c("writeoffs_noncoll_baa"),
  wo_allowance_da    = c("writeoffs_noncoll_da"),
  wo_allowance_total = c("writeoffs_noncoll_tot")
)

# Years for which a *_meta.csv exists. Metadata validation is skipped, loudly,
# for any other year - see load_nndr3_year().
cfg$metadata_years <- c(2025L)

# Panel window actually attempted. Distinct from year_min/year_max, which
# describe the target 12-year series. Widen as historic years are ingested.
cfg$panel_year_min <- 2020L
cfg$panel_year_max <- 2025L

# Master table: explicit per-year source format registry -----------------------
# QA CALLOUT: format is declared, never inferred. Each year names its type and
# its file(s). Adding a year means adding a line here, so the mapping is
# auditable in one place rather than guessed from filenames at runtime.
#   machine_flat  - one consolidated CSV, variable names on row 1
#   machine_split - separate D2 + D5 CSVs, variable names on row 3, join on Ecode
#   positional    - old dropdown CSV, no names, description-anchored (D2 only)
# QA CALLOUT: the five positional years (2013-14 to 2017-18) are now live. Their
# D2 reader (Metric 1, change_noncoll) and D5 reader (allowance stock for Metric
# 2, balance-sheet arrears for Metric 3) are verified against the supplied files:
# D5 England closing balances resolve to sensible magnitudes (-£674m for 14-15
# through -£1,409m for 17-18) at ~326 authorities per year, and 2013-14's
# positive-filed stock fields are sign-normalised in read_nndr3_positional_d5().
# The layouts are genuinely distinct (five description sets on D2; four D5 header
# schemes) so each year keeps its own spec - do not collapse them.
cfg$master_years <- c(2013L, 2014L, 2015L, 2016L, 2017L, 2018L, 2019L, 2020L, 2021L, 2022L, 2023L, 2024L, 2025L)

cfg$year_formats <- list(
  "2013" = list(type = "positional", spec = "13_14", d2 = "nndr3_13_14_D2.csv", d5 = "nndr3_13_14_D5.csv"),
  "2014" = list(type = "positional", spec = "14_15", d2 = "nndr3_14_15_D2.csv", d5 = "nndr3_14_15_D5.csv"),
  "2015" = list(type = "positional", spec = "15_16", d2 = "nndr3_15_16_D2.csv", d5 = "nndr3_15_16_D5.csv"),
  "2016" = list(type = "positional", spec = "16_17", d2 = "nndr3_16_17_D2.csv", d5 = "nndr3_16_17_D5.csv"),
  "2017" = list(type = "positional", spec = "17_18", d2 = "nndr3_17_18_D2.csv", d5 = "nndr3_17_18_D5.csv"),
  "2018" = list(type = "machine_split", d2 = "nndr3_18_19_D2.csv", d5 = "nndr3_18_19_D5.csv"),
  "2019" = list(type = "machine_split", d2 = "nndr3_19_20_D2.csv", d5 = "nndr3_19_20_D5.csv"),
  "2020" = list(type = "machine_flat",  file = "nndr3_20_21.csv"),
  "2021" = list(type = "machine_flat",  file = "nndr3_21_22.csv"),
  "2022" = list(type = "machine_flat",  file = "nndr3_22_23.csv"),
  "2023" = list(type = "machine_flat",  file = "nndr3_23_24.csv"),
  "2024" = list(type = "machine_flat",  file = "nndr3_24_25.csv"),
  "2025" = list(type = "machine_flat",  file = "nndr3_25_26.csv")
)

# Compatibility aliases used by current pilot scripts.
var_map_2025 <- vars_2025
optional_map_2025 <- optional_vars_2025
integrity_map_2025 <- integrity_vars_2025

appeals_variables <- c(
  "appeals_ob_cg", "appeals_ob_ba", "appeals_ob_mpa", "appeals_ob_fra",
  "appeals_ob_tot", "appeals_ob_adj_cg", "appeals_ob_adj_ba",
  "appeals_ob_adj_mpa", "appeals_ob_adj_fra", "appeals_ob_adj_tot"
)

search_patterns <- list(
  writeoffs = "written.?off|write.?off|writeoff",
  allowance = "allowance for non.?collection|non.?collection",
  appeals = "appeal|alteration of lists",
  denominator = "net rates payable|net amount receivable|collectable"
)

baseline_2016 <- tibble(
  financial_year = "2015-16",
  year = 2015L,
  write_offs = 214e6,
  denominator = 24.4e9,
  write_off_rate = 0.009,
  numerator_source = "QRC4",
  denominator_source = "QRC4 net collectable debit",
  comparable_to_nndr3 = FALSE
)