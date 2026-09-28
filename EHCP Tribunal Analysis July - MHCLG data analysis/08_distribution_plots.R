
library(scales)

# ------ analysis years ------

if (!exists("BASE_YEAR"))   BASE_YEAR   <- "2018/19"
if (!exists("LATEST_YEAR")) LATEST_YEAR <- "2025/26"

PLOT_DIR <- "outputs/section1_2_plots"

# ------ annotation helpers ------

skewness_manual <- function(x) {
  x <- x[!is.na(x)]
  (sum((x - mean(x))^3) / length(x)) / (sd(x)^3)
}

skew_label <- function(s) {
  case_when(
    abs(s) < 0.5 ~ "roughly symmetric",
    abs(s) < 1   ~ "moderately skewed",
    TRUE         ~ "strongly skewed"
  )
}

skew_annotation <- function(x, prefix = "") {
  s <- skewness_manual(x)
  if (abs(s) < 0.5) return(NA_character_)
  paste0(prefix, skew_label(s), " (", ifelse(s > 0, "right", "left"),
         "-skewed, skewness = ", round(s, 2), ")")
}

relationship_strength_label <- function(r) {
  case_when(
    abs(r) < 0.10 ~ "negligible",
    abs(r) < 0.30 ~ "weak",
    abs(r) < 0.50 ~ "moderate",
    TRUE          ~ "strong"
  )
}

# ------ plot data ------

excluded_las <- c("City of London", "Isles of Scilly")

excluded_caption <- paste0(
  paste(excluded_las, collapse = " and "),
  " excluded — very small 0-25 population denominators and starting caseloads."
)

year_levels <- sort(unique(master_la_table$academic_year))

stopifnot(
  "BASE_YEAR not present in master_la_table"   = BASE_YEAR   %in% year_levels,
  "LATEST_YEAR not present in master_la_table" = LATEST_YEAR %in% year_levels
)

plot_data <- master_la_table %>%
  filter(!la_name %in% excluded_las) %>%
  mutate(academic_year = factor(academic_year, levels = year_levels))

stopifnot(
  "duplicate LA-year rows in plot_data" =
    nrow(count(plot_data, new_la_code_current, academic_year) %>% filter(n > 1)) == 0
)

la_labels <- plot_data %>%
  filter(academic_year == LATEST_YEAR) %>%
  distinct(new_la_code_current, la_name, region_name)

counts_data <- plot_data %>% filter(!is.na(ehcplans))
rate_data   <- plot_data %>% filter(!is.na(rate_per_1000))

la_count_range <- counts_data %>%
  count(academic_year) %>%
  pull(n) %>%
  range()

panel_caption <- paste0(
  "Raw EHCP counts are not adjusted for the underlying 0-25 population; see the EHCP rate ",
  "charts for a population-adjusted comparison. The number of local authorities included ",
  "ranges from ", la_count_range[1], " to ", la_count_range[2], " across the period due to ",
  "boundary changes and data availability, so part of the change shown reflects a shifting ",
  "panel of authorities rather than growth alone."
)

# ------ growth, baseline to latest ------

growth_data <- counts_data %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR)) %>%
  mutate(endpoint = if_else(academic_year == BASE_YEAR, "base", "latest")) %>%
  select(new_la_code_current, endpoint, ehcplans) %>%
  pivot_wider(names_from = endpoint, values_from = ehcplans) %>%
  filter(!is.na(base), !is.na(latest), base > 0) %>%
  mutate(growth_pct = 100 * (latest / base - 1)) %>%
  inner_join(la_labels, by = "new_la_code_current")

cat("\n=== GROWTH PANEL ===\n")
cat("Baseline:", BASE_YEAR, "-> latest:", LATEST_YEAR, "\n")
cat("LAs with growth calculable:", nrow(growth_data), "\n")
cat("LAs in", LATEST_YEAR, "without a", BASE_YEAR, "baseline:",
    nrow(anti_join(la_labels, growth_data, by = "new_la_code_current")), "\n")

# ------ latest-year rate ------

latest_rate_data <- rate_data %>% filter(academic_year == LATEST_YEAR)

# ------ totals and year-on-year growth ------

count_by_year <- counts_data %>%
  group_by(academic_year) %>%
  summarise(
    total_ehcplans = sum(ehcplans),
    las_reporting  = n(),
    .groups = "drop"
  ) %>%
  arrange(academic_year)

total_yoy <- count_by_year %>%
  mutate(
    previous_total = lag(total_ehcplans),
    yoy_pct        = 100 * (total_ehcplans / previous_total - 1)
  ) %>%
  filter(!is.na(yoy_pct), is.finite(yoy_pct))

peak_yoy <- total_yoy %>% slice_max(yoy_pct, n = 1)


# ------ plot: total EHCP count by year ------


p_total_count <- ggplot(count_by_year, aes(x = academic_year, y = total_ehcplans)) +
  geom_col(fill = mhclg_teal, alpha = 0.85) +
  geom_text(aes(label = comma(total_ehcplans)), vjust = -0.6, size = 3.4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "Total EHCP count by year",
    subtitle = paste0(BASE_YEAR, " to ", LATEST_YEAR),
    x = "Academic year",
    y = "Total EHCPs",
    caption = str_wrap(panel_caption, 110)
  ) +
  theme_mhclg() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p_total_count)

ggsave(file.path(PLOT_DIR, "total_ehcp_count_by_year.png"), p_total_count,
       width = 9, height = 5.5, dpi = 300, bg = "white")

# ------ plot: year-on-year growth in total EHCP count ------
peak_label <- paste0("Highest annual growth: +", round(peak_yoy$yoy_pct, 1),
                     "%\n(", as.character(peak_yoy$academic_year), ")")

p_total_yoy <- ggplot(total_yoy, aes(x = academic_year, y = yoy_pct, group = 1)) +
  geom_line(colour = mhclg_teal, linewidth = 1.2) +
  geom_point(colour = mhclg_teal, size = 2.5) +
  geom_point(data = peak_yoy, colour = mhclg_plum, size = 4) +
  geom_label(data = peak_yoy, label = peak_label, colour = mhclg_plum,
             fill = "white", size = 4, hjust = 0.5, vjust = 1.6,
             label.padding = unit(0.3, "lines")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  expand_limits(y = 0) +
  labs(
    title    = "Year-on-year growth in total EHCP count",
    subtitle = paste0("Local authorities in England, ",
                      as.character(first(total_yoy$academic_year)), " to ", LATEST_YEAR),
    x = "Academic year",
    y = "Year-on-year % growth in total EHCPs",
    caption = paste0(panel_caption)
  ) +
  theme_mhclg() +
  theme(
    axis.text.x  = element_text(angle = 35, hjust = 1, size = 12),
    axis.text.y  = element_text(size = 12),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13)
  )

print(p_total_yoy)
ggsave(file.path(PLOT_DIR, "total_ehcp_yoy_growth.png"), p_total_yoy,
       width = 15, height = 8, dpi = 300, bg = "white")



# ------ plot: EHCP rate histogram, latest year ------

top_rate    <- latest_rate_data %>% slice_max(rate_per_1000, n = 1)
bottom_rate <- latest_rate_data %>% slice_min(rate_per_1000, n = 1)

rate_callout <- paste0(
  "Highest: ", top_rate$la_name,    " (", round(top_rate$rate_per_1000, 1),    " per 1,000)\n",
  "Lowest: ",  bottom_rate$la_name, " (", round(bottom_rate$rate_per_1000, 1), " per 1,000)"
)

p_rate_hist <- ggplot(latest_rate_data, aes(x = rate_per_1000)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = 20) +
  geom_vline(xintercept = median(latest_rate_data$rate_per_1000),
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  labs(
    title    = "EHCP rate by local authority",
    subtitle = paste0("Plans per 1,000 population aged 0-25, ", LATEST_YEAR),
    x = "Plans per 1,000 population aged 0-25",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA (",
                     round(median(latest_rate_data$rate_per_1000), 1), " per 1,000).\n",
                     excluded_caption)
  ) +
  theme_mhclg()



p_rate_hist <- p_rate_hist +
  annotate("label", x = min(latest_rate_data$rate_per_1000), y = Inf,
           label = rate_callout, hjust = 0, vjust = 1.5, size = 3.2,
           fill = "white", colour = mhclg_plum)

print(p_rate_hist)

ggsave(file.path(PLOT_DIR, "ehcp_rate_histogram_latest.png"), p_rate_hist,
       width = 8, height = 5, dpi = 300, bg = "white")


# ------ plot: EHCP growth histogram, baseline to latest ------

top_growth    <- growth_data %>% slice_max(growth_pct, n = 1)
bottom_growth <- growth_data %>% slice_min(growth_pct, n = 1)

growth_callout <- paste0(
  "Highest: ", top_growth$la_name,    " (+", round(top_growth$growth_pct, 1),   "%)\n",
  "Lowest: ",  bottom_growth$la_name, " (",  round(bottom_growth$growth_pct, 1), "%)"
)

growth_skew_text <- skew_annotation(growth_data$growth_pct)

p_growth_hist <- ggplot(growth_data, aes(x = growth_pct)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = 20) +
  geom_vline(xintercept = median(growth_data$growth_pct),
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "EHCP growth by local authority",
    subtitle = paste0("% change in raw EHCP numbers, ", BASE_YEAR, " to ", LATEST_YEAR),
    x = "% growth in EHCP numbers",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA growth (",
                     round(median(growth_data$growth_pct), 1), "%).\n",
                     excluded_caption)
  ) +
  theme_mhclg()

if (!is.na(growth_skew_text)) {
  p_growth_hist <- p_growth_hist +
    annotate("label", x = max(growth_data$growth_pct), y = Inf,
             label = growth_skew_text, hjust = 1, vjust = 1.5, size = 3.2,
             fill = "white", colour = "black")
}

p_growth_hist <- p_growth_hist +
  annotate("label", x = min(growth_data$growth_pct), y = Inf,
           label = growth_callout, hjust = 0, vjust = 1.5, size = 3.2,
           fill = "white", colour = mhclg_plum)

print(p_growth_hist)

ggsave(file.path(PLOT_DIR, "ehcp_growth_histogram.png"), p_growth_hist,
       width = 8, height = 5, dpi = 300, bg = "white")
# ------ plot: baseline rate vs subsequent growth ------

baseline_rate <- rate_data %>%
  filter(academic_year == BASE_YEAR) %>%
  select(new_la_code_current, baseline_rate = rate_per_1000)

quadrant_data <- baseline_rate %>%
  inner_join(growth_data %>% select(new_la_code_current, la_name, region_name, growth_pct),
             by = "new_la_code_current")

rate_median   <- median(quadrant_data$baseline_rate)
growth_median <- median(quadrant_data$growth_pct)

quadrant_data <- quadrant_data %>%
  mutate(
    quadrant = case_when(
      baseline_rate >= rate_median & growth_pct >= growth_median ~
        "Above-median baseline rate / Above-median growth",
      baseline_rate >= rate_median & growth_pct <  growth_median ~
        "Above-median baseline rate / Below-median growth",
      baseline_rate <  rate_median & growth_pct >= growth_median ~
        "Below-median baseline rate / Above-median growth",
      TRUE ~
        "Below-median baseline rate / Below-median growth"
    ),
    distance_from_centre = sqrt(
      ((baseline_rate - rate_median)   / sd(baseline_rate))^2 +
        ((growth_pct    - growth_median) / sd(growth_pct))^2
    )
  )

quadrant_colours <- c(
  "Above-median baseline rate / Above-median growth" = mhclg_plum,
  "Above-median baseline rate / Below-median growth" = mhclg_navy,
  "Below-median baseline rate / Above-median growth" = mhclg_orange,
  "Below-median baseline rate / Below-median growth" = mhclg_teal
)

quadrant_examples <- quadrant_data %>%
  group_by(quadrant) %>%
  slice_max(distance_from_centre, n = 1) %>%
  ungroup()

quadrant_test <- cor.test(quadrant_data$baseline_rate, quadrant_data$growth_pct,
                          method = "pearson")
quadrant_r <- unname(quadrant_test$estimate)
quadrant_p <- quadrant_test$p.value

p_quadrant <- ggplot(quadrant_data, aes(x = baseline_rate, y = growth_pct)) +
  geom_vline(xintercept = rate_median,   linetype = "dashed", colour = "grey45") +
  geom_hline(yintercept = growth_median, linetype = "dashed", colour = "grey45") +
  geom_point(aes(colour = quadrant), alpha = 0.75, size = 2.3) +
  geom_smooth(method = "lm", se = TRUE, colour = mhclg_plum,
              fill = mhclg_grey, linewidth = 1) +
  geom_label(data = quadrant_examples, aes(label = la_name), size = 3,
             colour = "black", fill = "white",
             nudge_y = diff(range(quadrant_data$growth_pct)) * 0.04) +
  scale_colour_manual(values = quadrant_colours) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  guides(colour = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(
    title    = "Local authorities by initial EHCP rate and subsequent growth",
    subtitle = paste0(BASE_YEAR, " EHCP rate vs % change in EHCP numbers by ", LATEST_YEAR,
                      " (Pearson r = ", round(quadrant_r, 2), ")"),
    x = paste0("Plans per 1,000 population aged 0-25 in ", BASE_YEAR),
    y = paste0("% growth in EHCP numbers, ", BASE_YEAR, " to ", LATEST_YEAR),
    colour  = "Group",
    caption = excluded_caption
  ) +
  theme_mhclg() +
  theme(legend.position = "bottom")

print(p_quadrant)

ggsave(file.path(PLOT_DIR, "ehcp_rate_vs_growth_quadrant.png"), p_quadrant,
       width = 12, height = 7, dpi = 300, bg = "white")

# ------ console summary ------

cat("\n=== DISTRIBUTION SUMMARY ===\n")
cat(sprintf("Rate (%s, n=%d): median %.1f per 1,000, range %.1f-%.1f\n",
            LATEST_YEAR, nrow(latest_rate_data),
            median(latest_rate_data$rate_per_1000),
            min(latest_rate_data$rate_per_1000),
            max(latest_rate_data$rate_per_1000)))
cat(sprintf("Growth (%s -> %s, n=%d): median %.1f%%, range %.1f%% to %.1f%%, IQR %.1f%%-%.1f%%\n",
            BASE_YEAR, LATEST_YEAR, nrow(growth_data),
            median(growth_data$growth_pct),
            min(growth_data$growth_pct),
            max(growth_data$growth_pct),
            quantile(growth_data$growth_pct, 0.25),
            quantile(growth_data$growth_pct, 0.75)))
cat(sprintf("Baseline rate vs growth: r = %.2f, p = %s (%s)\n",
            quadrant_r, signif(quadrant_p, 3), relationship_strength_label(quadrant_r)))

cat("\nPlots written to", PLOT_DIR, "\n")

