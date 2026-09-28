


library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
PLOT_DIR <- "outputs/section1_2_plots"

TOTAL_BREAKDOWN <- "All EHC plans"

AGE_0_15  <- c("under 3", "age 2 and under", paste("age", 3:15))
AGE_16_24 <- paste("age", 16:24)
AGE_25    <- "age 25"
AGE_LABELS <- c(AGE_0_15, AGE_16_24, AGE_25)

SUPPRESSION_CODES <- c("c", "x", "z", "~", ":", "-", "")

parse_count <- function(x) {
  x <- str_remove_all(as.character(x), "[, ]")
  x[x %in% SUPPRESSION_CODES] <- NA_character_
  suppressWarnings(as.numeric(x))
}

# ------ national slice ------

national_raw <- data %>%
  filter(geographic_level == "National", country_name == "England") %>%
  mutate(
    year_order    = year_order_value(time_period),
    academic_year = format_academic_year(time_period),
    ehcplans_n    = parse_count(ehcplans)
  ) %>%
  arrange(year_order) %>%
  mutate(academic_year = factor(academic_year, levels = unique(academic_year)))

age_like <- unique(national_raw$breakdown)
age_like <- age_like[str_detect(age_like, regex("^(age|under)", ignore_case = TRUE))]

overlap_years <- national_raw %>%
  filter(breakdown %in% c("under 3", "age 2 and under")) %>%
  count(academic_year) %>%
  filter(n > 1)

# ------ national total series ------

national_total_by_year <- national_raw %>%
  filter(breakdown == TOTAL_BREAKDOWN) %>%
  group_by(academic_year, year_order) %>%
  summarise(total_ehcplans = sum(ehcplans_n), n_rows = n(), .groups = "drop") %>%
  arrange(year_order)


national_total_by_year <- select(national_total_by_year, -n_rows)

# ------ national age bands ------

ehcp_age_national <- national_raw %>%
  filter(breakdown %in% AGE_LABELS) %>%
  mutate(band = case_when(
    breakdown %in% AGE_0_15  ~ "ehcp_pop_0_15",
    breakdown %in% AGE_16_24 ~ "ehcp_pop_16_24",
    breakdown == AGE_25      ~ "ehcp_pop_25"
  )) %>%
  group_by(academic_year, year_order, band) %>%
  summarise(
    ehcplans     = sum(ehcplans_n, na.rm = TRUE),
    n_suppressed = sum(is.na(ehcplans_n)),
    .groups      = "drop"
  )

age_years <- sort(unique(as.character(ehcp_age_national$academic_year)))

ehcp_age_wide <- ehcp_age_national %>%
  select(academic_year, year_order, band, ehcplans) %>%
  pivot_wider(names_from = band, values_from = ehcplans) %>%
  mutate(ehcp_pop_0_25 = ehcp_pop_0_15 + ehcp_pop_16_24 + ehcp_pop_25) %>%
  arrange(year_order)

age_reconciliation <- ehcp_age_wide %>%
  left_join(
    ehcp_age_national %>%
      group_by(academic_year) %>%
      summarise(n_suppressed = sum(n_suppressed), .groups = "drop"),
    by = "academic_year"
  ) %>%
  inner_join(national_total_by_year, by = c("academic_year", "year_order")) %>%
  mutate(difference = ehcp_pop_0_25 - total_ehcplans)



# ------ year-on-year growth ------

total_yoy <- national_total_by_year %>%
  mutate(yoy_pct = 100 * (total_ehcplans / lag(total_ehcplans) - 1)) %>%
  filter(!is.na(yoy_pct), is.finite(yoy_pct))

peak_yoy <- slice_max(total_yoy, yoy_pct, n = 1)

national_caption <- str_wrap(paste0(
  "Source: DfE, Education, health and care plans. Published England totals, ",
  "not a sum of local authority rows."
), 110)

# ------ plot: total EHCP count by year ------

p_total_count <- ggplot(national_total_by_year,
                        aes(x = academic_year, y = total_ehcplans)) +
  geom_col(fill = mhclg_teal, alpha = 0.85) +
  geom_text(aes(label = comma(total_ehcplans)), vjust = -0.6, size = 3.4) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Total EHCP count by year",
       subtitle = paste0("England, ", BASE_YEAR, " to ", LATEST_YEAR),
       x = "Academic year", y = "Total EHCPs", caption = national_caption) +
  theme_mhclg() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

print(p_total_count)
ggsave(file.path(PLOT_DIR, "total_ehcp_count_by_year.png"), p_total_count,
       width = 9, height = 5.5, dpi = 300, bg = "white")

# ------ plot: year-on-year growth ------

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
  labs(title    = "Year-on-year growth in total EHCP count",
       subtitle = paste0("England, ", as.character(first(total_yoy$academic_year)),
                         " to ", LATEST_YEAR),
       x = "Academic year", y = "Year-on-year % growth in total EHCPs",
       caption = national_caption) +
  theme_mhclg() +
  theme(axis.text.x  = element_text(angle = 35, hjust = 1, size = 12),
        axis.text.y  = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13))
print(p_total_yoy)
ggsave(file.path(PLOT_DIR, "total_ehcp_yoy_growth.png"), p_total_yoy,
       width = 15, height = 8, dpi = 300, bg = "white")

