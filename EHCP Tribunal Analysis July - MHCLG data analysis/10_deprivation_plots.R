library(tidyverse)
library(scales)



# ------ parameters ------

BASE_YEAR   <- "2018/19"
LATEST_YEAR <- "2025/26"

dir.create("deprivation_plots", showWarnings = FALSE)

source_url <- paste0(
  "Source: DfE, Special educational needs in England (table tool): ",
  "https://explore-education-statistics.service.gov.uk/data-tables/special-educational-needs-in-england"
)

omitted_rate <- c(
  "Buckinghamshire", "Isle of Wight", "North Yorkshire", "Somerset"
)

omitted_growth <- c(
  "Bournemouth, Christchurch and Poole", "Buckinghamshire", "Cumberland",
  "Dorset", "Isle of Wight", "North Northamptonshire", "North Yorkshire",
  "Somerset", "West Northamptonshire", "Westmorland and Furness"
)

omitted_caption <- function(x) {
  paste0("Omitted local authorities: [", paste(x, collapse = ", "), "]")
}


# ------ IDACI ------

idaci_clean <- read_csv("data/LDACI_file11.csv", show_col_types = FALSE) %>%
  rename_with(str_trim) %>%
  transmute(
    ons_code    = as.character(`Upper Tier Local Authority District code (2024)`),
    idaci_score = as.numeric(`IDACI - Average score`)
  ) %>%
  filter(!is.na(ons_code), !is.na(idaci_score))

la_idaci <- master_la_table %>%
  mutate(new_la_code_current = normalise_geog(new_la_code)) %>%
  left_join(idaci_clean, by = c("new_la_code_current" = "ons_code"))


# ------ plot data ------

rate_data <- la_idaci %>%
  filter(
    academic_year == LATEST_YEAR,
    !is.na(idaci_score),
    !is.na(rate_per_1000),
    !la_name %in% omitted_rate
  )

growth_data <- la_idaci %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR), !la_name %in% omitted_growth) %>%
  group_by(la_id) %>%
  summarise(
    idaci_score  = first(idaci_score),
    base_plans   = ehcplans[academic_year == BASE_YEAR][1],
    latest_plans = ehcplans[academic_year == LATEST_YEAR][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(idaci_score), !is.na(base_plans), !is.na(latest_plans), base_plans > 0) %>%
  mutate(ehcp_growth_pc = 100 * (latest_plans / base_plans - 1))


# ------ plot builder ------

cor_fit <- function(df, y) {
  ct <- cor.test(df$idaci_score, df[[y]], method = "pearson")
  tibble(r = unname(ct$estimate), p = ct$p.value, n = nrow(df))
}

cor_note <- function(df, y) {
  sprintf(
    "Pearson r = %+.2f\nno meaningful correlation at LA level",
    cor_fit(df, y)$r
  )
}

deprivation_scatter <- function(df, y, title, y_lab, caption) {
  ggplot(df, aes(x = idaci_score, y = .data[[y]])) +
    geom_point(colour = mhclg_teal, alpha = 0.85, size = 2) +
    geom_smooth(method = "lm", formula = y ~ x,
                colour = mhclg_plum, fill = "grey85", linewidth = 1) +
    annotate("label", x = -Inf, y = Inf, label = cor_note(df, y),
             hjust = -0.03, vjust = 1.25, colour = mhclg_plum,
             fill = "white", size = 3.6, label.size = 0.4, lineheight = 1.1) +
    labs(
      title   = title,
      x       = "IDACI average score (more deprived \u2192)",
      y       = y_lab,
      caption = caption
    ) +
    theme_mhclg() +
    theme(plot.caption = element_text(size = 12, hjust = 0, lineheight = 1.4))
}


# ------ plot: IDACI vs EHCP rate ------

p_idaci_rate <- deprivation_scatter(
  rate_data,
  y     = "rate_per_1000",
  title = paste0("IDACI deprivation vs EHCP rate, ", LATEST_YEAR),
  y_lab = paste0("EHCP rate per 1,000 aged 0-25, ", LATEST_YEAR),
  caption = paste(
    paste0(LATEST_YEAR, " denominator uses ONS 2022-based SNPP projections, not published MYEs."),
    source_url,
    omitted_caption(omitted_rate),
    sep = "\n\n"
  )
)

print(p_idaci_rate)


# ------ plot: IDACI vs EHCP growth ------

p_idaci_growth <- deprivation_scatter(
  growth_data,
  y     = "ehcp_growth_pc",
  title = paste0("IDACI deprivation vs EHCP growth, ", BASE_YEAR, " to ", LATEST_YEAR),
  y_lab = paste0("% change in EHCP count, ", BASE_YEAR, " to ", LATEST_YEAR),
  caption = paste(
    "Based on EHCP plan counts",
    source_url,
    omitted_caption(omitted_growth),
    sep = "\n\n"
  )
)

print(p_idaci_growth)


# ------ save ------

ggsave("deprivation_plots/scatter_idaci_ehcp_rate.png", p_idaci_rate,
       width = 11, height = 7, dpi = 300, bg = "white")

ggsave("deprivation_plots/scatter_idaci_ehcp_growth.png", p_idaci_growth,
       width = 11, height = 7, dpi = 300, bg = "white")


# ------ correlations to console ------

bind_rows(
  cor_fit(rate_data,   "rate_per_1000")  %>% mutate(comparison = "IDACI vs EHCP rate"),
  cor_fit(growth_data, "ehcp_growth_pc") %>% mutate(comparison = "IDACI vs EHCP growth")
) %>%
  select(comparison, r, p, n) %>%
  mutate(across(c(r, p), ~ round(.x, 3))) %>%
  print()

