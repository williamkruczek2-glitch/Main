# 08_metric1_panel_charts.R -----------------------------------------------------
# Metric 1 PANEL chart definitions, extracted verbatim from the original panel
# runner so the plot logic is unchanged. Sourced by both the panel runner and
# 11_run_metric1_panel.R (which feeds them the master-table series, 2018-26).
#
# No side effects: this file defines functions only and never calls run_*().
# The charts here, their shared annotation layer, and the print/save helpers all
# take `national` / `metric1` objects and do not care which ingest path built
# them.

panel_annotations <- function(national, show_covid = TRUE) {
  layers <- list()
  
  if (show_covid && any(national$covid_affected)) {
    covid <- sort(unique(national$year[national$covid_affected]))
    layers <- c(layers, annotate_covid(covid))
  }
  
  c(layers, list(
    scale_x_continuous(breaks = national$year, labels = label_fy(national$year))
  ))
}

# 1. Nominal and real net write-offs -------------------------------------------
# Ben's steer: a real version alongside the nominal one. Both on one axis, same
# units, so the divergence is the point of the chart.
chart_panel_writeoffs <- function(national) {
  long <- national |>
    select(year, covid_affected, immature,
           Nominal = net_writeoffs_nominal, Real = net_writeoffs_real) |>
    pivot_longer(c(Nominal, Real), names_to = "basis", values_to = "value")
  
  ggplot(long, aes(year, value, colour = basis)) +
    panel_annotations(national) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_y_continuous(labels = label_gbp_m(0),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    scale_colour_manual(values = c(Nominal = mhclg_col$metric1,
                                   Real = mhclg_col$highlight)) +
    labs(
      title = "Net write-offs of business rates, England",
      subtitle = paste0("Nominal and real terms, ",
                        first(national$price_basis), ". No trend fitted."),
      x = NULL, y = NULL, colour = NULL,
      caption = metric1_caption(
        "Real series deflated by ONS September CPI (D7BT). See caveats C18-C23."
      )
    ) +
    theme_mhclg(grid = "y")
}

# 2. Write-off rate ------------------------------------------------------------
# Separate panel rather than a second axis: different units, and dual axes
# invite readers to infer a relationship that has not been tested.
chart_panel_rate <- function(national) {
  ggplot(national, aes(year, writeoff_rate_nrp)) +
    panel_annotations(national) +
    geom_line(linewidth = 0.9, colour = mhclg_col$metric1) +
    geom_point(size = 2, colour = mhclg_col$metric1) +
    scale_y_continuous(labels = label_prop(2),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    labs(
      title = "Net write-offs as a share of net rates payable, England",
      subtitle = paste0("Denominator is NNDR3 net rates payable, ",
                        "not QRC4 net collectable debit (C16)."),
      x = NULL, y = NULL,
      caption = metric1_caption(
        "Rate is same-year nominal over nominal; there is no real-terms rate (C22)."
      )
    ) +
    theme_mhclg(grid = "y")
}

# 3. The denominator -----------------------------------------------------------
# QA CALLOUT: this chart is not optional context, it is what makes chart 2
# honest. Relief policy moves the collectable base independently of payment
# behaviour (C15), and during COVID it moved a great deal. Without the
# denominator on the page, a movement in the rate reads as behavioural when it
# may be compositional.
chart_panel_denominator <- function(national) {
  long <- national |>
    mutate(net_rates_payable_real = net_rates_payable * cpi_deflator) |>
    select(year, covid_affected, immature,
           Nominal = net_rates_payable, Real = net_rates_payable_real) |>
    pivot_longer(c(Nominal, Real), names_to = "basis", values_to = "value")
  
  ggplot(long, aes(year, value, colour = basis)) +
    panel_annotations(national) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_y_continuous(labels = label_gbp_bn(1),
                       limits = c(0, NA), expand = expansion(c(0, 0.12))) +
    scale_colour_manual(values = c(Nominal = mhclg_col$metric3,
                                   Real = mhclg_col$highlight)) +
    labs(
      title = "Net rates payable, England",
      subtitle = paste0("The rate denominator. Moves with relief policy ",
                        "independently of payment behaviour (C15)."),
      x = NULL, y = NULL, colour = NULL,
      caption = metric1_caption(
        "Shown so movements in the write-off rate can be read against the base."
      )
    ) +
    theme_mhclg(grid = "y")
}

# 4. Line 3 against line 4 -----------------------------------------------------
# Grouped, never stacked: line 4 can be negative and a stacked bar would hide it.
chart_panel_components <- function(national) {
  long <- national |>
    select(year, covid_affected, immature,
           `Line 3: charged to allowance` = wo_allowance,
           `Line 4: in excess / write-backs` = wo_excess) |>
    pivot_longer(-c(year, covid_affected, immature),
                 names_to = "component", values_to = "value")
  
  covid <- sort(unique(national$year[national$covid_affected]))
  
  ggplot(long, aes(factor(year), value, fill = component)) +
    annotate_covid_discrete(national$year, covid) +
    geom_hline(yintercept = 0, colour = mhclg_grey[["axis"]], linewidth = 0.4) +
    geom_col(position = position_dodge(width = 0.75), width = 0.7) +
    scale_x_discrete(labels = \(x) label_fy(as.integer(x))) +
    scale_y_continuous(labels = label_gbp_m(0)) +
    scale_fill_manual(values = c(mhclg_col$metric1, mhclg_col$metric2)) +
    labs(
      title = "Write-offs by NNDR3 return line, England",
      subtitle = paste0("Sign convention: positive is a write-off, ",
                        "negative a write-back."),
      x = NULL, y = NULL, fill = NULL,
      caption = metric1_caption(
        paste("This split is by return line. The gross/write-back split is by",
              "authority-level sign - the two do not reconcile (C26).")
      )
    ) +
    theme_mhclg(grid = "y")
}

# 5. Distribution across authorities -------------------------------------------
# The systemic-drift versus concentration question: does the whole distribution
# move, or does England move because a handful of authorities move?
#
# QA CALLOUT: cfg$panel_distribution_cap is a VIEW cap, not a data cap --------
# A handful of authority-years (City-of-London-type denominator effects) sit
# far above the rest, which compressed every year's box into an unreadable
# sliver near zero. coord_cartesian() zooms the axis without dropping rows
# before ggplot computes the boxplot statistics, so the median/quartiles/
# whiskers stay correct even though the view is capped - unlike
# scale_y_continuous(limits = ...), which would drop data first and distort
# them. Authorities above the cap are named in
# chart_panel_distribution_callout() below, not silently hidden.
cfg$panel_distribution_cap <- 0.15

chart_panel_distribution <- function(metric1, cap = cfg$panel_distribution_cap) {
  plot_data <- metric1 |> filter(!is.na(writeoff_rate_nrp))
  covid <- sort(unique(plot_data$year[plot_data$covid_affected]))
  
  ggplot(plot_data, aes(factor(year), writeoff_rate_nrp)) +
    annotate_covid_discrete(plot_data$year, covid) +
    geom_boxplot(outlier.alpha = 0.35, outlier.size = 1,
                 fill = mhclg_col$provisional, alpha = 0.5,
                 colour = mhclg_col$metric1, linewidth = 0.4) +
    coord_cartesian(ylim = c(0, cap)) +
    scale_x_discrete(labels = \(x) label_fy(as.integer(x))) +
    scale_y_continuous(labels = label_prop(1)) +
    labs(
      title = "Distribution of write-off rates across billing authorities",
      subtitle = paste0("One box per year, view capped at ", label_prop(0)(cap),
                        " - authorities above this are listed separately, ",
                        "not dropped from the underlying statistics."),
      x = NULL, y = NULL,
      caption = metric1_caption(
        paste("Cross-year comparison of the distribution is affected by",
              "changing authority composition until back-casting is applied.",
              "Boxplot quartiles and whiskers are computed on the full data;",
              "only the axis view is capped.")
      )
    ) +
    theme_mhclg(grid = "y")
}

# Companion to the chart above: every authority that exceeds the cap in at
# least one year, grouped rather than listed authority-year, because the
# question worth answering is the same persistence question as charts 44/45 -
# is this a repeat name or a one-off.
panel_distribution_outliers <- function(metric1, cap = cfg$panel_distribution_cap) {
  above_cap <- metric1 |>
    filter(!is.na(writeoff_rate_nrp), writeoff_rate_nrp > cap)
  
  # QA CALLOUT: checked BEFORE group_by/summarise, not after -----------------
  # dplyr evaluates a summarise() expression once on an empty vector to infer
  # its output type even when there are zero groups to actually summarise -
  # calling max() on that empty trial throws "no non-missing arguments to
  # max" as a warning before this function gets a chance to report anything
  # useful. Checking here instead avoids that noise entirely, and reports the
  # actual observed maximum so the message says WHY nothing cleared the cap,
  # not just THAT nothing did - the number needed to tell "cap is too high
  # for this run's data" apart from "something upstream is actually wrong."
  if (nrow(above_cap) == 0) {
    observed_max <- max(metric1$writeoff_rate_nrp, na.rm = TRUE)
    message(
      "No authority-year exceeds ", label_prop(0)(cap),
      " - highest observed rate is ", label_prop(1)(observed_max),
      ". Distribution callout skipped."
    )
    return(tibble(
      la_code = character(),
      la_name = character(),
      years_exceeded = integer(),
      years_list = character(),
      max_rate = double()
    ))
  }
  
  out <- above_cap |>
    group_by(la_code) |>
    summarise(la_name = last(na.omit(la_name)),
              years_exceeded = n(),
              years_list = paste(sort(financial_year), collapse = ", "),
              max_rate = max(writeoff_rate_nrp),
              .groups = "drop") |>
    arrange(desc(max_rate))
  
  # QA CALLOUT: la_name can resolve to NA -----------------------------------
  # last(na.omit(la_name)) returns NA if la_name is NA/blank for every single
  # row that la_code appears in, not just some. That means master itself has
  # no name on record for that authority in any year it exceeded the cap -
  # worth investigating upstream, not just papering over here. Falls back to
  # la_code so the callout still shows something identifiable, and reports
  # exactly which code(s) hit this rather than silently rendering a blank row.
  # Known code/name repair for the Plot 15 callout. Apply only when the
  # master-table name is actually missing, so a valid populated name is never
  # overwritten.
  liverpool_missing <- out$la_code == "E4302" &
    (is.na(out$la_name) | out$la_name == "")
  if (any(liverpool_missing)) {
    out$la_name[liverpool_missing] <- "Liverpool City Council"
  }
  
  # Generic fallback for any other unresolved authority names.
  missing_name <- is.na(out$la_name) | out$la_name == ""
  if (any(missing_name)) {
    message("  NOTE: la_name unresolved for ", sum(missing_name),
            " la_code(s) - falling back to la_code as the display label: ",
            paste(out$la_code[missing_name], collapse = ", "),
            ". Check master table coverage for these codes.")
    out$la_name[missing_name] <- out$la_code[missing_name]
  }
  
  out
}

# Text callout for the slide - three columns positioned by x-anchor rather
# than relying on a monospace font being installed, since font availability
# cannot be guaranteed on whatever machine renders this.
chart_panel_distribution_callout <- function(outliers,
                                             cap = cfg$panel_distribution_cap) {
  if (nrow(outliers) == 0) {
    return(NULL)
  }
  if (nrow(outliers) > 20) {
    message("  NOTE: ", nrow(outliers), " authority(ies) exceed the cap - ",
            "long for a slide callout. Consider raising cfg$panel_distribution_cap.")
  }
  
  label_data <- outliers |>
    mutate(max_rate_label = label_prop(1)(max_rate),
           years_label = paste(years_exceeded, "yr(s)"),
           row = row_number())
  n <- nrow(label_data)
  
  ggplot(label_data, aes(y = -row)) +
    geom_text(aes(x = 0, label = la_name), hjust = 0, size = 3.4,
              colour = mhclg_col$text) +
    geom_text(aes(x = 0.62, label = years_label), hjust = 0, size = 3.4,
              colour = mhclg_col$secondary) +
    geom_text(aes(x = 0.86, label = max_rate_label), hjust = 0, size = 3.4,
              fontface = "bold", colour = mhclg_col$metric1) +
    annotate("text", x = 0, y = 0.8, label = "Authority", hjust = 0,
             fontface = "bold", size = 3.2, colour = mhclg_col$secondary) +
    annotate("text", x = 0.62, y = 0.8, label = "Years above cap", hjust = 0,
             fontface = "bold", size = 3.2, colour = mhclg_col$secondary) +
    annotate("text", x = 0.86, y = 0.8, label = "Peak rate", hjust = 0,
             fontface = "bold", size = 3.2, colour = mhclg_col$secondary) +
    xlim(0, 1.03) +
    ylim(-n - 0.5, 1.4) +
    labs(
      title = paste0(n, " authority(ies) exceeded ", label_prop(0)(cap),
                     " in at least one year"),
      subtitle = "Excluded from the boxplot view above, not from its statistics.",
      caption = metric1_caption()
    ) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 13, hjust = 0,
                                    margin = margin(b = 2)),
          plot.subtitle = element_text(size = 10, colour = mhclg_col$secondary,
                                       margin = margin(b = 10)),
          plot.margin = margin(10, 10, 10, 10),
          # QA CALLOUT: aspect.ratio, not just ggsave dimensions -------------
          # ggsave() only fixes the aspect ratio of a SAVED file. Viewed
          # interactively (e.g. RStudio's Plots pane), a ggplot has no
          # inherent size - it stretches to whatever the pane's current
          # dimensions happen to be, which is what produced the huge gaps
          # between 2 rows of text in a tall pane. aspect.ratio is baked into
          # the plot's own theme, so the panel keeps a sensible height:width
          # proportion for its actual row count no matter what device or pane
          # renders it. Floor/cap match the save-height formula below in
          # spirit, not value - they govern different things (panel-only vs
          # whole-image-including-margins) so do not need to match exactly.
          aspect.ratio = min(2.2, max(0.35, 0.12 * n + 0.16))) -> p
  
  # QA CALLOUT: row count carried as an attribute, read by save_panel_charts()
  # ---------------------------------------------------------------------------
  # Every other chart in this file is saved at a fixed "slide" size (10x5.6in)
  # via save_mhclg() - fine for a data-dense scatter, absurd for a short text
  # list, which just stretches the same handful of rows across a canvas far
  # taller than the content, producing the huge dead whitespace this callout
  # was showing. n is attached here so save_panel_charts() can size this one
  # chart's canvas to its actual row count instead of using the shared preset.
  attr(p, "callout_n_rows") <- n
  p
}

build_panel_charts <- function(metric1, national) {
  outliers <- panel_distribution_outliers(metric1)
  list(
    writeoffs = chart_panel_writeoffs(national),
    rate = chart_panel_rate(national),
    denominator = chart_panel_denominator(national),
    components = chart_panel_components(national),
    distribution = chart_panel_distribution(metric1),
    distribution_callout = chart_panel_distribution_callout(outliers)
  )
}

# QA CALLOUT: charts are PRINTED as well as saved -------------------------------
# print() sends each plot to the RStudio Plots pane so a reviewer can page
# through them in the session rather than opening PNGs from disk. Saving alone
# is not enough for QA.
print_panel_charts <- function(charts) {
  valid <- charts[!map_lgl(charts, is.null)]
  iwalk(valid, \(plot, name) {
    message("Displaying chart: ", name)
    print(plot)
  })
  invisible(valid)
}

save_panel_charts <- function(charts, dir = file.path(cfg$dir_charts, "m1")) {
  filenames <- c(
    writeoffs = "10_panel_writeoffs_nominal_real.png",
    rate = "11_panel_writeoff_rate.png",
    denominator = "12_panel_net_rates_payable.png",
    components = "13_panel_components_line3_line4.png",
    distribution = "14_panel_rate_distribution.png",
    distribution_callout = "15_panel_rate_distribution_outliers.png"
  )
  valid <- charts[!map_lgl(charts, is.null)]
  
  imap_chr(valid, \(plot, name) {
    if (name == "distribution_callout") {
      # QA CALLOUT: proportional height, not the shared "slide" preset --------
      # save_mhclg() only offers four fixed sizes, all sized for data-dense
      # charts. This one is a short text list - n rows read from the
      # attribute chart_panel_distribution_callout() attaches - so height
      # scales with actual content instead of stretching 2-3 rows across the
      # same canvas a 20-point scatter gets. Floor/cap keep it sane at the
      # extremes (a single row, or the >20-row case that function already
      # warns about). Mirrors save_mhclg()'s own dir-creation, message and
      # return-value behaviour so this stays a drop-in for the other charts.
      n_rows <- attr(plot, "callout_n_rows")
      if (is.null(n_rows)) n_rows <- 5  # fallback if ever called without it
      height <- max(2.5, min(7, 1.8 + 0.55 * n_rows))
      
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      path <- file.path(dir, filenames[[name]])
      ggsave(path, plot, width = 15, height = height, dpi = 300, bg = "white")
      message("Written: ", path)
      return(invisible(path))
    }
    save_mhclg(plot, filenames[[name]], dir = dir)
  })
}

