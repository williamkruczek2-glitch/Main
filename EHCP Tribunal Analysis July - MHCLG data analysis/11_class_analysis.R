
#----- run file 09 for idaci_clean P sen_raw var

library(tidyverse)




# ------ parameters ------

CLASS_DIR  <- "outputs/class_analysis"
CLASS_PATH <- "data/core_authorities_by_class.csv"

dir.create(CLASS_DIR, showWarnings = FALSE, recursive = TRUE)

MIN_SEN_TOTAL     <- 100
EXCLUDED_LA_CODES <- c("E09000001","E06000046")
MIN_CLASS_N       <- 5

# ONS statistical definition. The 1963 Act definition adds Greenwich
# (E09000011) and drops Haringey and Newham.
INNER_LONDON_CODES <- c(
  "E09000001", "E09000007", "E09000012", "E09000013", "E09000014",
  "E09000019", "E09000020", "E09000022", "E09000023", "E09000025",
  "E09000028", "E09000030", "E09000032", "E09000033"
)

SUPERSEDED_CODES <- c(
  "E10000002", "E10000009", "E06000028", "E06000029",
  "E10000021", "E10000006", "E10000023", "E10000027"
)

CLASS_COLOURS <- c(
  "Unitary"      = mhclg_teal,
  "County"       = mhclg_plum,
  "Metropolitan" = mhclg_grey,
  "Inner London" = mhclg_navy,
  "Outer London" = colorRampPalette(c(mhclg_navy, "white"))(4)[2]
)

IDACI_LAB <- "IDACI average score (higher = more deprived)"

cor_label <- function(x, y) {
  test <- cor.test(x, y, method = "pearson")
  sprintf("Pearson r = %+.2f (p = %.3f, n = %d)",
          unname(test$estimate), test$p.value, length(x))
}


# ------ school-census SEN decomposition ------

sen_master <- sen_raw %>%
  filter(geographic_level    == "Local authority",
         phase_type_grouping == "Total",
         hospital_school     == "Total",
         establishment_type  == "Total") %>%
  transmute(
    year        = paste0(str_sub(time_period, 1, 4), "/", str_sub(time_period, 5, 6)),
    year_order  = as.integer(str_sub(time_period, 1, 4)),
    la_code     = normalise_geog(new_la_code),
    la_name,
    region_name,
    provision   = unname(provision_map[sen_provision]),
    pupil_count = to_num(pupil_count)
  ) %>%
  group_by(year, year_order, la_code, provision) %>%
  summarise(
    pupil_count = if (any(is.na(pupil_count))) NA_real_ else sum(pupil_count),
    la_name     = first(la_name),
    region_name = first(region_name),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    id_cols     = c(year, year_order, la_code, la_name, region_name),
    names_from  = provision,
    values_from = pupil_count
  ) %>%
  mutate(
    sen_total       = sen_support + ehcp_census,
    ehcp_pct_of_sen = 100 * ehcp_census / sen_total,
    sen_pct_of_all  = 100 * sen_total / total_pupils,
    ehcp_pct_of_all = 100 * ehcp_census / total_pupils
  )

current_year <- sen_master %>%
  slice_max(year_order, n = 1, with_ties = FALSE) %>%
  pull(year)


# ------ IDACI and authority classes ------

idaci_la <- idaci_clean %>%
  transmute(la_code = normalise_geog(str_trim(ons_code)), idaci = idaci_score) %>%
  filter(str_starts(la_code, "E")) %>%
  group_by(la_code) %>%
  summarise(idaci = mean(idaci), .groups = "drop")

class_lookup <- read_csv(CLASS_PATH, show_col_types = FALSE) %>%
  transmute(la_code = as.character(ons_code), class = as.character(class)) %>%
  filter(!la_code %in% SUPERSEDED_CODES) %>%
  mutate(class = case_when(
    class == "LB" & la_code %in% INNER_LONDON_CODES ~ "Inner London",
    class == "LB"                                   ~ "Outer London",
    class == "UA"                                   ~ "Unitary",
    class == "SC"                                   ~ "County",
    class == "MD"                                   ~ "Metropolitan"
  ))


# ------ analysis frame ------

# Growth uses SEN2 plan counts (all plans 0-25) so it matches the rest of the
# pipeline. Prevalence, identification and conversion stay school-census.

sen2_growth <- master_la_table %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR)) %>%
  group_by(la_code = normalise_geog(new_la_code)) %>%
  summarise(
    base   = sum(ehcplans[academic_year == BASE_YEAR],   na.rm = TRUE),
    latest = sum(ehcplans[academic_year == LATEST_YEAR], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(base > 0) %>%
  transmute(la_code, count_growth = 100 * (latest / base - 1))

sen2_rate <- master_la_table %>%
  filter(academic_year == LATEST_YEAR) %>%
  group_by(la_code = normalise_geog(new_la_code)) %>%
  summarise(plans = sum(ehcplans, na.rm = TRUE),
            pop   = sum(pop_0_25, na.rm = TRUE), .groups = "drop") %>%
  filter(pop > 0) %>%
  transmute(la_code, prevalence = 100 * plans / pop)

dat_all <- sen_master %>%
  filter(year == current_year) %>%
  transmute(la_code, la_name, region_name, ehcp_census, sen_total,
            identify = sen_pct_of_all,
            convert  = ehcp_pct_of_sen) %>%
  left_join(sen2_rate,    by = "la_code") %>%
  left_join(sen2_growth,  by = "la_code") %>%
  left_join(idaci_la,     by = "la_code") %>%
  left_join(class_lookup, by = "la_code")

MANUAL_EXCLUSIONS <- tribble(
  ~la_name,        ~reason,
  "Isle of Wight", "no 2025/26 return"
)

dat_class <- dat_all %>%
  filter(sen_total >= MIN_SEN_TOTAL,
         !la_code %in% EXCLUDED_LA_CODES,
         !la_name %in% MANUAL_EXCLUSIONS$la_name)

# Derived from the plotted rows, so the caption cannot drift from the filters.
exclusion_caption <- function(plotted) {
  dropped <- dat_all %>%
    anti_join(distinct(plotted, la_code), by = "la_code") %>%
    left_join(MANUAL_EXCLUSIONS, by = "la_name") %>%
    mutate(reason = coalesce(reason, case_when(
      la_code %in% EXCLUDED_LA_CODES ~ "very small pupil base",
      sen_total < MIN_SEN_TOTAL      ~ "SEN population below threshold",
      is.na(class)                   ~ "no authority class match",
      is.na(count_growth)            ~ paste0("no ", BASE_YEAR, " baseline"),
      TRUE                           ~ "no data for this measure"
    ))) %>%
    arrange(la_name)
  
  if (nrow(dropped) == 0) return("")
  paste0("Excluded: ",
         paste0(dropped$la_name, " (", dropped$reason, ")", collapse = "; "), ".")
}

CENSUS_NOTE    <- paste0("School census, ", current_year)
SEN2_RATE_NOTE <- paste0("SEN2 EHC plans aged 0-25, ", LATEST_YEAR)
SEN2_NOTE   <- paste0("SEN2 EHCPs aged 0-25, ", BASE_YEAR, " to ", LATEST_YEAR)
GROWTH_LAB  <- "Change in EHCPs, 0-25 (%)"


# ------ class summary ------

class_summary <- dat_class %>%
  filter(!is.na(class)) %>%
  group_by(class) %>%
  summarise(
    n              = n(),
    med_prevalence = median(prevalence,   na.rm = TRUE),
    iqr_prevalence = IQR(prevalence,      na.rm = TRUE),
    med_identify   = median(identify,     na.rm = TRUE),
    iqr_identify   = IQR(identify,        na.rm = TRUE),
    med_convert    = median(convert,      na.rm = TRUE),
    iqr_convert    = IQR(convert,         na.rm = TRUE),
    med_growth     = median(count_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(med_prevalence))

write_csv(class_summary, file.path(CLASS_DIR, "class_prevalence_summary.csv"))


# ------ plot: identification against conversion ------

d_decomp <- dat_class %>% filter(!is.na(class), !is.na(identify), !is.na(convert))

p_decomposition <- ggplot(d_decomp, aes(x = identify, y = convert, colour = class)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_hline(yintercept = median(d_decomp$convert),  linetype = "dashed", colour = mhclg_grey) +
  geom_vline(xintercept = median(d_decomp$identify), linetype = "dashed", colour = mhclg_grey) +
  scale_colour_manual(values = CLASS_COLOURS) +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(
    title    = "Identification and conversion by local authority",
    subtitle = paste0("School census, ", current_year, ". Dashed lines = median authority"),
    x        = "SEN as % of all pupils (identification)",
    y        = "EHCPs as % of identified SEN (conversion)",
    colour   = "Authority class"
  ) +
  theme_mhclg()

ggsave(file.path(CLASS_DIR, "identification_vs_conversion.png"), p_decomposition,
       width = 10, height = 6, dpi = 300, bg = "white")


# ------ boxplots with labelled outliers ------

class_box <- function(var, y_lab, plot_title, plot_subtitle) {
  d <- dat_class %>%
    filter(!is.na(class), !is.na(.data[[var]])) %>%
    group_by(class) %>%
    mutate(
      q1 = quantile(.data[[var]], .25),
      q3 = quantile(.data[[var]], .75),
      is_outlier = .data[[var]] < q1 - 1.5 * (q3 - q1) |
        .data[[var]] > q3 + 1.5 * (q3 - q1)
    ) %>%
    ungroup() %>%
    mutate(class = fct_reorder(class, .data[[var]], median))
  
  # Alternate label side in value order so clustered outliers do not collide.
  outliers <- d %>%
    filter(is_outlier) %>%
    group_by(class) %>%
    arrange(.data[[var]], .by_group = TRUE) %>%
    mutate(above = row_number() %% 2 == 1) %>%
    ungroup()
  
  ggplot(d, aes(x = class, y = .data[[var]], fill = class)) +
    geom_boxplot(alpha = 0.85, outlier.shape = NA) +
    geom_point(data = outliers, colour = mhclg_plum, size = 1.8) +
    geom_text(data = filter(outliers, above), aes(label = la_name),
              nudge_x = 0.33, size = 2.8, colour = mhclg_plum) +
    geom_text(data = filter(outliers, !above), aes(label = la_name),
              nudge_x = -0.33, size = 2.8, colour = mhclg_plum) +
    scale_fill_manual(values = CLASS_COLOURS) +
    coord_flip() +
    labs(title = plot_title, subtitle = plot_subtitle, x = NULL, y = y_lab,
         caption = str_wrap(exclusion_caption(d), 120)) +
    theme_mhclg() +
    theme(legend.position = "none",
          plot.caption = element_text(size = 8, hjust = 0, colour = "grey30"))
}

p_prevalence_box <- class_box("prevalence", "EHCPs as % of population aged 0-25",
                              "EHCP rate by local authority class",
                              paste0("SEN2 EHC plans aged 0-25, ", LATEST_YEAR))
p_growth_box     <- class_box("count_growth", GROWTH_LAB,
                              "EHCP growth by local authority class",
                              paste0(BASE_YEAR, " to ", LATEST_YEAR))

ggsave(file.path(CLASS_DIR, "rate_by_class_box.png"), p_prevalence_box,
       width = 10, height = 6, dpi = 300, bg = "white")
ggsave(file.path(CLASS_DIR, "growth_by_class_box.png"), p_growth_box,
       width = 10, height = 6, dpi = 300, bg = "white")


# ------ IDACI scatters ------

d_prev   <- dat_class %>% filter(!is.na(idaci), !is.na(prevalence),   !is.na(class))
d_growth <- dat_class %>% filter(!is.na(idaci), !is.na(count_growth), !is.na(class))

idaci_scatter <- function(df, var, y_lab, plot_title, source_note) {
  ggplot(df, aes(x = idaci, y = .data[[var]])) +
    geom_point(aes(colour = class), alpha = 0.8, size = 2) +
    geom_smooth(method = "lm", formula = y ~ x, colour = mhclg_grey,
                linetype = "dashed", se = FALSE) +
    annotate("label", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.1,
             label = cor_label(df$idaci, df[[var]]), size = 3.1,
             colour = mhclg_plum, fill = "white") +
    scale_colour_manual(values = CLASS_COLOURS) +
    guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
    labs(title = plot_title, subtitle = source_note,
         x = IDACI_LAB, y = y_lab, colour = "Authority class") +
    theme_mhclg()
}

p_prev_scatter <- idaci_scatter(d_prev, "prevalence", "EHCPs as % of population aged 0-25",
                                "Deprivation and EHCP prevalence, by authority class",
                                SEN2_RATE_NOTE)
p_growth_scatter <- idaci_scatter(d_growth, "count_growth", GROWTH_LAB,
                                  "Deprivation and EHCP growth, by authority class",
                                  SEN2_NOTE)

ggsave(file.path(CLASS_DIR, "idaci_vs_prevalence.png"), p_prev_scatter,
       width = 10, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(CLASS_DIR, "idaci_vs_growth.png"), p_growth_scatter,
       width = 10, height = 5.5, dpi = 300, bg = "white")


# ------ within-class correlations ------

class_correlation <- function(outcome) {
  dat_class %>%
    filter(!is.na(class), !is.na(idaci), !is.na(.data[[outcome]])) %>%
    group_by(class) %>%
    summarise(
      n = n(),
      r = if (n() >= MIN_CLASS_N) cor(idaci, .data[[outcome]]) else NA_real_,
      p = if (n() >= MIN_CLASS_N) cor.test(idaci, .data[[outcome]])$p.value else NA_real_,
      .groups = "drop"
    ) %>%
    mutate(label = if_else(is.na(r), sprintf("n = %d", n),
                           sprintf("r = %+.2f\np = %.3f\nn = %d", r, p, n)))
}

class_corr_prev   <- class_correlation("prevalence")
class_corr_growth <- class_correlation("count_growth")

write_csv(class_corr_prev,   file.path(CLASS_DIR, "idaci_prevalence_correlation_by_class.csv"))
write_csv(class_corr_growth, file.path(CLASS_DIR, "idaci_growth_correlation_by_class.csv"))

class_facet <- function(df, var, labels, y_lab, plot_title, source_note) {
  ggplot(df, aes(x = idaci, y = .data[[var]])) +
    geom_point(aes(colour = class), alpha = 0.75, size = 1.7, show.legend = FALSE) +
    geom_smooth(method = "lm", formula = y ~ x, colour = mhclg_plum,
                fill = mhclg_grey, linewidth = 0.6) +
    geom_label(data = labels, aes(label = label), x = Inf, y = Inf,
               hjust = 1.05, vjust = 1.1, size = 3, colour = mhclg_plum,
               fill = "white", inherit.aes = FALSE) +
    scale_colour_manual(values = CLASS_COLOURS) +
    facet_wrap(~ class) +
    labs(title = plot_title, subtitle = source_note,
         x = IDACI_LAB, y = y_lab) +
    theme_mhclg(10)
}

p_prev_facets <- class_facet(d_prev, "prevalence", class_corr_prev,
                             "EHCPs as % of population aged 0-25",
                             "Deprivation and EHCP prevalence by authority class",
                             SEN2_RATE_NOTE)
p_growth_facets <- class_facet(d_growth, "count_growth", class_corr_growth, GROWTH_LAB,
                               "Deprivation and EHCP growth by authority class",
                               SEN2_NOTE)

ggsave(file.path(CLASS_DIR, "idaci_prevalence_facets.png"), p_prev_facets,
       width = 11, height = 7, dpi = 300, bg = "white")
ggsave(file.path(CLASS_DIR, "idaci_growth_facets.png"), p_growth_facets,
       width = 11, height = 7, dpi = 300, bg = "white")


# ------ QA output ------

cat("\nCurrent year:", current_year, "| Baseline:", BASE_YEAR)
cat("\nAuthorities in analysis:", nrow(dat_class))
cat("\nWithout IDACI:", sum(is.na(dat_class$idaci)),
    "| Without class:", sum(is.na(dat_class$class)),
    "| Without baseline:", sum(is.na(dat_class$count_growth)), "\n\n")

print(class_summary, width = Inf)

check_exclusions <- function(p, label) {
  plotted <- p$data$la_name
  cap     <- str_squish(str_replace_all(p$labels$caption, "\\s+", " "))
  named   <- str_match(cap, "Excluded: (.*)\\.$")[, 2]
  named   <- if (is.na(named)) character(0) else
    str_trim(str_remove(str_split(named, ";")[[1]], "\\s*\\(.*\\)"))
  
  cat("\n[", label, "] plotted:", length(plotted),
      "| named as excluded:", length(named), "\n")
  cat("  named but still plotted:",
      paste(intersect(named, plotted), collapse = ", "), "\n")
  cat("  dropped but not named:",
      paste(setdiff(setdiff(dat_all$la_name, plotted), named), collapse = ", "), "\n")
}

check_exclusions(p_prevalence_box, "prevalence")
check_exclusions(p_growth_box,     "growth")


print(p_prevalence_box)
print(p_growth_box)

check_exclusions(p_prevalence_box, "prevalence")
check_exclusions(p_growth_box,     "growth")








#--------- slide 11 stats callouts -------

top_prev <- class_summary %>% slice_max(med_prevalence, n = 1)
bot_prev <- class_summary %>% slice_min(med_prevalence, n = 1)
top_grow <- class_summary %>% slice_max(med_growth,     n = 1)
bot_grow <- class_summary %>% slice_min(med_growth,     n = 1)

between_gap  <- diff(range(class_summary$med_prevalence))
within_iqr   <- median(class_summary$iqr_prevalence)
identify_gap <- diff(range(class_summary$med_identify))
convert_gap  <- diff(range(class_summary$med_convert))

national_prev   <- median(dat_class$prevalence,   na.rm = TRUE)
national_growth <- median(dat_class$count_growth, na.rm = TRUE)


# ------ callout: prevalence boxplot ------

prev_headline <- sprintf(
  "%s authorities have the highest median EHCP rate at %.1f%% of pupils, %s the lowest at %.1f%%",
  top_prev$class, top_prev$med_prevalence, bot_prev$class, bot_prev$med_prevalence
)

prev_subheading <- sprintf(
  paste0("The gap between authority classes is %.1f percentage points, but the typical spread ",
         "within a single class is %.1f points \u2014 variation sits between authorities, ",
         "not between types of authority"),
  between_gap, within_iqr
)

prev_decomposition <- sprintf(
  paste0("Prevalence is identification multiplied by conversion. Across classes the identification ",
         "rate varies by %.1f percentage points and the conversion rate by %.1f, so %s accounts ",
         "for more of the difference between classes"),
  identify_gap, convert_gap,
  if (identify_gap > convert_gap) "who is identified as having SEN" else "who is converted to a plan"
)


# ------ callout: growth boxplot ------

grow_headline <- sprintf(
  "EHCP numbers grew fastest in %s authorities (median %.0f%%) and slowest in %s (%.0f%%) since %s",
  top_grow$class, top_grow$med_growth, bot_grow$class, bot_grow$med_growth, BASE_YEAR
)

grow_subheading <- sprintf(
  paste0("The median authority saw EHCP numbers rise %.0f%% between %s and %s. Every class ",
         "grew, so the pressure is national rather than confined to one type of authority"),
  national_growth, BASE_YEAR, LATEST_YEAR
)


# ------ London disaggregation ------

london <- class_summary %>% filter(str_detect(class, "London"))

london_callout <- if (nrow(london) == 2) {
  inner <- london %>% filter(class == "Inner London")
  outer <- london %>% filter(class == "Outer London")
  sprintf(
    paste0("Inner and Outer London differ: median EHCP rate %.1f%% against %.1f%%, and median ",
           "growth %.0f%% against %.0f%% \u2014 treating London as one block hides this"),
    inner$med_prevalence, outer$med_prevalence, inner$med_growth, outer$med_growth
  )
} else NA_character_


# ------ outlier authorities by class ------

class_outliers <- function(var) {
  dat_class %>%
    filter(!is.na(class), !is.na(.data[[var]])) %>%
    group_by(class) %>%
    mutate(q1 = quantile(.data[[var]], .25), q3 = quantile(.data[[var]], .75)) %>%
    filter(.data[[var]] < q1 - 1.5 * (q3 - q1) | .data[[var]] > q3 + 1.5 * (q3 - q1)) %>%
    ungroup() %>%
    transmute(class, la_name, value = round(.data[[var]], 1)) %>%
    arrange(class, value)
}


# ------ print ------

cat("\n=== SLIDE: EHCP RATE BY CLASS ===\n\n")
cat("HEADLINE:\n",     prev_headline,      "\n\n")
cat("SUB-HEADING:\n",  prev_subheading,    "\n\n")
cat("SUPPORTING:\n",   prev_decomposition, "\n\n")

cat("\n=== SLIDE: EHCP GROWTH BY CLASS ===\n\n")
cat("HEADLINE:\n",     grow_headline,   "\n\n")
cat("SUB-HEADING:\n",  grow_subheading, "\n\n")
if (!is.na(london_callout)) cat("LONDON:\n", london_callout, "\n\n")

cat("\n=== CLASS SUMMARY ===\n")
class_summary %>%
  transmute(class, n,
            rate_pc   = round(med_prevalence, 1),
            rate_iqr  = round(iqr_prevalence, 1),
            identify  = round(med_identify,   1),
            convert   = round(med_convert,    1),
            growth_pc = round(med_growth,     0)) %>%
  print(n = Inf)

cat(sprintf("\nNational median rate: %.1f%% | national median growth: %.0f%%\n",
            national_prev, national_growth))
cat(sprintf("Between-class gap: %.1f pp | typical within-class IQR: %.1f pp\n",
            between_gap, within_iqr))

cat("\n=== OUTLIER AUTHORITIES: RATE ===\n")
print(class_outliers("prevalence"), n = Inf)

cat("\n=== OUTLIER AUTHORITIES: GROWTH ===\n")
print(class_outliers("count_growth"), n = Inf)

cat("\n=== DEPRIVATION WITHIN CLASS ===\n")
class_corr_prev   %>% select(class, n, r, p) %>% mutate(measure = "rate")   %>% print()
class_corr_growth %>% select(class, n, r, p) %>% mutate(measure = "growth") %>% print()

