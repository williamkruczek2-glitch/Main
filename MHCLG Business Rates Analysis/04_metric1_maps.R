# 04_metric1_maps.R -------------------------------------------------------------
# Metric 1 CHOROPLETH map functions.
#
# A map shows one year at a time. It needs ons_code (LAD24CD) for the boundary
# join, which the recent consolidated files carry and the older files do not.
# The functions here build England and London-inset maps of the write-off rate,
# using England-wide quintiles so the two maps share a legend.
#
# Depends on: 00_config.R (cfg$boundary_code, cfg$london_code_prefix,
# cfg$map_bins, mhclg palette, save_mhclg helper), 01_init.R
# (read_england_boundaries). Sourced by 05_run_metric1_pilot.R.

# --- Data prep ----------------------------------------------------------------
# QA CALLOUT: join key is ons_code (9-digit GSS), not la_code (MHCLG E-code) ---
# la_code (Ecode) and ons_code (LAD24CD, e.g. "E07000223") are different
# identifiers. Boundary layers use the GSS code. Rows without ons_code cannot be
# mapped and become NA in the quintile scale, showing grey on the map.
prepare_metric1_map_data <- function(metric1, boundaries, bins = cfg$map_bins) {
  # metric1 here is a SINGLE YEAR filtered from the master. Uses writeoff_rate_nrp.
  if (!"ons_code" %in% names(metric1)) {
    stop("prepare_metric1_map_data: metric1 has no ons_code column.",
         call. = FALSE)
  }
  # Compute England-wide quintile breaks from the non-NA rate.
  vals <- metric1$writeoff_rate_nrp
  vals <- vals[!is.na(vals) & is.finite(vals)]
  breaks <- quantile(vals, probs = seq(0, 1, length.out = bins + 1L), na.rm = TRUE)
  breaks <- unique(breaks)   # guard against ties collapsing bins
  
  labels <- paste0(
    "Q", seq_along(breaks[-1]), ": ",
    scales::percent(breaks[-length(breaks)], accuracy = 0.01), " to ",
    scales::percent(breaks[-1], accuracy = 0.01)
  )
  
  binned <- metric1 |>
    mutate(
      rate_quintile = cut(writeoff_rate_nrp, breaks = breaks,
                          labels = labels, include.lowest = TRUE)
    )
  
  # Join to boundaries. Boundaries have ons_code + geometry from
  # read_england_boundaries().
  boundaries |>
    left_join(binned, by = "ons_code")
}

# --- Themes ------------------------------------------------------------------
metric1_map_theme <- function() {
  theme_void(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 14,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(colour = mhclg_col$secondary, size = 11,
                                   margin = margin(b = 8)),
      plot.caption = element_text(colour = mhclg_col$secondary, size = 8,
                                  hjust = 0, lineheight = 1.15,
                                  margin = margin(t = 8)),
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.height = grid::unit(0.5, "cm"),
      legend.key.width = grid::unit(0.4, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# --- Map builders ------------------------------------------------------------
map_caption <- function() {
  paste(
    "Source: MHCLG, NNDR3 outturn returns.",
    "Measures billed business rates debt written off as irrecoverable; excludes unbilled liability and relief misuse.",
    "The rate uses NNDR3 net rates payable as an interim denominator. Grey areas represent unmatched or unavailable values.",
    "Contains National Statistics and OS data",
    "\u00a9 Crown copyright and database right 2024.",
    sep = "\n"
  )
}

chart_metric1_map_england <- function(joined, year_label) {
  ggplot(joined) +
    geom_sf(aes(fill = rate_quintile), colour = "white", linewidth = 0.1) +
    scale_fill_brewer(
      palette = "Blues", na.value = "grey85",
      name = "Net write-offs as a share\nof net rates payable",
      drop = FALSE
    ) +
    labs(
      title = "Net write-off rates vary across billing authorities",
      subtitle = paste0("England, ", year_label, ", England-wide quintiles"),
      caption = map_caption()
    ) +
    metric1_map_theme() +
    coord_sf(datum = NA)
}

chart_metric1_map_london <- function(joined, year_label) {
  london <- joined |>
    filter(!is.na(ons_code), str_starts(ons_code, cfg$london_code_prefix))
  ggplot(london) +
    geom_sf(aes(fill = rate_quintile), colour = "white", linewidth = 0.2) +
    scale_fill_brewer(
      palette = "Blues", na.value = "grey85",
      name = "Net write-offs as a share\nof net rates payable",
      drop = FALSE
    ) +
    labs(
      title = "Net write-off rates across London boroughs",
      subtitle = paste0("London inset, ", year_label,
                        ", using England-wide quintiles"),
      caption = map_caption()
    ) +
    metric1_map_theme() +
    coord_sf(datum = NA)
}

# QA CALLOUT: single entry point --------------------------------------------
# make_metric1_maps() takes a SINGLE YEAR of metric1 rows plus the boundaries
# and returns the two maps. Callers filter the master table before calling.
make_metric1_maps <- function(metric1, boundaries) {
  if (length(unique(metric1$year)) != 1L) {
    stop("make_metric1_maps: expected a single year of rows; got ",
         length(unique(metric1$year)), ".", call. = FALSE)
  }
  year_label <- label_fy(unique(metric1$year))
  joined <- prepare_metric1_map_data(metric1, boundaries)
  
  # QA: how many boundaries did NOT match a metric1 row?
  unmatched <- sum(is.na(joined$rate_quintile))
  if (unmatched > 0) {
    message("Maps: ", unmatched,
            " boundary polygon(s) unmatched to a metric1 row (shown grey).")
  }
  
  list(
    england = chart_metric1_map_england(joined, year_label),
    london = chart_metric1_map_london(joined, year_label)
  )
}