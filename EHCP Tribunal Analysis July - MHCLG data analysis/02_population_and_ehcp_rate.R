

library(tidyverse)
library(nomisr)


# ------ geography reference ------

geo_ref <- read_csv("data/GeoID Reference Table - 07062024.csv", show_col_types = FALSE)

code_changes <- geo_ref %>%
  filter(STATUS == "terminated", GEOGCD_current != "NULL", !is.na(GEOGCD_current)) %>%
  distinct(old_code = GEOGCD, current_code = GEOGCD_current)

manual_recodes <- tribble(
  ~old_code,   ~current_code,
  "E10000009", "E06000059"  # Dorset county -> Dorset unitary
)

geo_lookup <- bind_rows(code_changes, manual_recodes) %>%
  distinct(old_code, .keep_all = TRUE)

normalise_geog <- function(code) {
  tibble(code = as.character(code)) %>%
    left_join(geo_lookup, by = c("code" = "old_code")) %>%
    mutate(current_code = coalesce(current_code, code)) %>%
    pull(current_code)
}


# ------ LTLA to UTLA lookup ------

ltla_utla <- read_csv("data/LTA23_UTLA23_EW_LU.csv", show_col_types = FALSE) %>%
  filter(str_starts(LTLA23CD, "E")) %>%
  distinct(
    AREA_CODE = LTLA23CD,
    UTLA_CODE = UTLA23CD,
    UTLA_NAME = UTLA23NM
  )

utla_names <- ltla_utla %>%
  distinct(UTLA_CODE, UTLA_NAME)

to_utla_population <- function(df) {
  df %>%
    mutate(AREA_CODE = normalise_geog(AREA_CODE)) %>%
    group_by(year, AREA_CODE) %>%
    summarise(
      across(c(pop_0_15, pop_16_24, pop_25), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    left_join(
      ltla_utla %>% select(AREA_CODE, UTLA_CODE),
      by = "AREA_CODE"
    ) %>%
    # Codes absent from the lookup are already UTLA-level.
    mutate(UTLA_CODE = coalesce(UTLA_CODE, AREA_CODE)) %>%
    group_by(year, AREA_CODE = UTLA_CODE) %>%
    summarise(
      across(c(pop_0_15, pop_16_24, pop_25), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(pop_0_25 = pop_0_15 + pop_16_24 + pop_25) %>%
    left_join(utla_names, by = c("AREA_CODE" = "UTLA_CODE")) %>%
    rename(AREA_NAME = UTLA_NAME)
}

to_academic_year <- function(year) {
  year <- as.integer(year)
  paste0(year, "/", str_sub(as.character(year + 1), 3, 4))
}


# ------ NOMIS mid-year estimates: 2018/19 to 2024/25 ------

nomis_years <- 2018:2024

nomis_raw <- nomis_get_data(
  id = "NM_2002_1",
  geography = "TYPE423",
  tidy = TRUE,
  tidy_style = "snake_case",
  measures = c(
    20100,
    "date_name",
    "geography_name",
    "geography_code",
    "geography_type",
    "c_age_name",
    "obs_value"
  ),
  gender = 0,
  time = nomis_years,
  c_age = c(201, 250, 126)
) %>%
  filter(str_starts(geography_code, "E"))

# TYPE423 already contains the required 153 English authority geographies,
# so NOMIS data does not need to be aggregated through to_utla_population().

pop_actual <- nomis_raw %>%
  transmute(
    year = to_academic_year(date),
    AREA_CODE = as.character(geography_code),
    AREA_NAME = as.character(geography_name),
    age_group = recode(
      as.character(c_age),
      `201` = "pop_0_15",
      `250` = "pop_16_24",
      `126` = "pop_25"
    ),
    population = as.numeric(obs_value)
  ) %>%
  pivot_wider(
    id_cols = c(year, AREA_CODE, AREA_NAME),
    names_from = age_group,
    values_from = population
  ) %>%
  mutate(
    pop_0_25 = pop_0_15 + pop_16_24 + pop_25,
    pop_estimated = FALSE
  )

pop_actual_check <- pop_actual %>%
  count(year, name = "n_areas")



# ------ SNPP projection: 2025/26 ------

# The calendar-year 2025 projection is used as the denominator for
# the 2025/26 academic-year EHCP figures.

snpp_data <- read_csv(
  "data/2022 SNPP Population persons.csv",
  show_col_types = FALSE
)


pop_snpp <- snpp_data %>%
  filter(AGE_GROUP %in% as.character(0:25)) %>%
  transmute(
    AREA_CODE = as.character(AREA_CODE),
    age = as.integer(AGE_GROUP),
    year = "2025/26",
    value = as.numeric(`2025`),
    age_group = case_when(
      age <= 15 ~ "pop_0_15",
      age <= 24 ~ "pop_16_24",
      age == 25 ~ "pop_25"
    )
  ) %>%
  group_by(year, AREA_CODE, age_group) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = age_group,
    values_from = value,
    values_fill = 0
  ) %>%
  mutate(across(c(pop_0_15, pop_16_24, pop_25), round)) %>%
  to_utla_population() %>%
  mutate(pop_estimated = TRUE)

stopifnot(
  "No 2025/26 SNPP population rows were created" =
    nrow(pop_snpp) > 0,
  "Duplicate authority rows in pop_snpp" =
    !anyDuplicated(pop_snpp[c("AREA_CODE", "year")]),
  "Missing SNPP population denominator" =
    all(!is.na(pop_snpp$pop_0_25)),
  "Non-positive SNPP population denominator" =
    all(pop_snpp$pop_0_25 > 0)
)


# ------ combine actual and projected population ------

pop_data <- bind_rows(pop_actual, pop_snpp) %>%
  rename(
    ons_code_current = AREA_CODE,
    authority = AREA_NAME
  )


# ------ reconstruct former county geographies ------

# Current successor populations are summed to provide denominators for
# historical EHCP rows that retain the former county codes.

split_lookup <- tribble(
  ~successor,  ~old_code,   ~old_name,
  "E06000063", "E10000006", "Cumbria",
  "E06000064", "E10000006", "Cumbria",
  "E06000061", "E10000021", "Northamptonshire",
  "E06000062", "E10000021", "Northamptonshire"
)

county_reconstructions <- pop_data %>%
  inner_join(split_lookup, by = c("ons_code_current" = "successor")) %>%
  group_by(year, old_code, old_name, pop_estimated) %>%
  summarise(
    across(c(pop_0_15, pop_16_24, pop_25, pop_0_25), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  transmute(
    year,
    ons_code_current = old_code,
    authority = old_name,
    pop_0_15,
    pop_16_24,
    pop_25,
    pop_0_25,
    pop_estimated
  )

pop_data <- bind_rows(pop_data, county_reconstructions)

pop_check <- pop_data %>%
  count(year, name = "n_areas")

stopifnot(
  "Duplicate authority-year rows in final pop_data" =
    !anyDuplicated(pop_data[c("ons_code_current", "year")]),
  "Unexpected population authority count" =
    all(between(pop_check$n_areas, 148, 162)),
  "Reconstructed county codes are missing" =
    any(str_starts(pop_data$ons_code_current, "E10")),
  "2025/26 contains non-projected rows" =
    all(pop_data$pop_estimated[pop_data$year == "2025/26"]),
  "2025/26 contains missing population denominators" =
    all(!is.na(pop_data$pop_0_25[pop_data$year == "2025/26"]))
)


# ------ DfE EHC plan counts ------
data <- read.csv(
  "https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/49e7efca-f010-4044-9f70-578032a23cf7/csv"
)

year_order_value <- function(x) {
  as.numeric(str_sub(str_remove_all(as.character(x), "/"), 1, 4))
}

format_academic_year <- function(x) {
  x <- str_remove_all(as.character(x), "/")
  paste0(str_sub(x, 1, 4), "/", str_sub(x, 5, 6))
}

# national level dataframe for slides 1 & 2

national_table <- data %>%
  filter(geographic_level == "National") %>%
  mutate(
    year_order    = year_order_value(time_period),
    academic_year = format_academic_year(time_period)
  ) %>%
  arrange(year_order) %>%
  mutate(academic_year = factor(academic_year, levels = unique(academic_year)))
ehcp_la <- data %>%
  filter(
    breakdown == "All EHC plans",
    geographic_level == "Local authority",
    country_name == "England"
  ) %>%
  transmute(
    time_period_order = year_order_value(time_period),
    time_period_label = format_academic_year(time_period),
    new_la_code = as.character(new_la_code),
    old_la_code = as.character(old_la_code),
    la_name = as.character(la_name),
    ehcplans = as.numeric(ehcplans)
  ) %>%
  filter(
    !is.na(new_la_code),
    !is.na(la_name),
    !is.na(time_period_order),
    !is.na(ehcplans)
  )


# ------ EHCP age bands ------

age_0_15 <- c("under 3", "age 2 and under", paste("age", 3:15))
age_16_24 <- paste("age", 16:24)

ehcp_age_bands <- data %>%
  filter(
    geographic_level == "Local authority",
    country_name == "England",
    breakdown %in% c(age_0_15, age_16_24, "age 25")
  ) %>%
  transmute(
    time_period_label = format_academic_year(time_period),
    new_la_code = as.character(new_la_code),
    ehcplans = as.numeric(ehcplans),
    band = case_when(
      breakdown %in% age_0_15 ~ "ehcp_pop_0_15",
      breakdown %in% age_16_24 ~ "ehcp_pop_16_24",
      breakdown == "age 25" ~ "ehcp_pop_25"
    )
  ) %>%
  group_by(new_la_code, time_period_label, band) %>%
  summarise(ehcplans = sum(ehcplans, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = band,
    values_from = ehcplans,
    values_fill = 0
  ) %>%
  mutate(ehcp_pop_0_25 = ehcp_pop_0_15 + ehcp_pop_16_24 + ehcp_pop_25)


# ------ normalise EHCP geography ------

ehcp_la <- ehcp_la %>%
  left_join(
    ehcp_age_bands,
    by = c("new_la_code", "time_period_label")
  ) %>%
  mutate(new_la_code_current = normalise_geog(new_la_code)) %>%
  group_by(time_period_order, time_period_label, new_la_code_current) %>%
  summarise(
    across(
      c(ehcplans, ehcp_pop_0_15, ehcp_pop_16_24, ehcp_pop_25, ehcp_pop_0_25),
      ~ sum(.x, na.rm = TRUE)
    ),
    new_la_code = first(new_la_code),
    old_la_code = first(old_la_code),
    la_name = first(la_name),
    .groups = "drop"
  )

year_levels <- ehcp_la %>%
  distinct(time_period_label, time_period_order) %>%
  arrange(time_period_order) %>%
  pull(time_period_label)

ehcp_la <- ehcp_la %>%
  mutate(
    time_period_label = factor(
      time_period_label,
      levels = year_levels,
      ordered = TRUE
    )
  ) %>%
  select(
    time_period_label,
    new_la_code,
    new_la_code_current,
    old_la_code,
    la_name,
    ehcplans,
    ehcp_pop_0_15,
    ehcp_pop_16_24,
    ehcp_pop_25,
    ehcp_pop_0_25
  )

baseline_year <- min(ehcp_la$time_period_label)
latest_year <- max(ehcp_la$time_period_label)


# ------ EHCP rate per 1,000 population aged 0-25 ------

ehcp_rates <- ehcp_la %>%
  mutate(year = as.character(time_period_label)) %>%
  left_join(
    pop_data %>%
      select(year, ons_code_current, pop_0_25, pop_estimated),
    by = c(
      "new_la_code_current" = "ons_code_current",
      "year" = "year"
    )
  ) %>%
  select(-year) %>%
  mutate(
    rate_per_1000 = if_else(
      !is.na(pop_0_25) & pop_0_25 > 0,
      ehcplans / pop_0_25 * 1000,
      NA_real_
    )
  )

latest_population_check <- ehcp_rates %>%
  filter(as.character(time_period_label) == "2025/26")



# ------ rate growth since baseline ------

baseline_rates <- ehcp_rates %>%
  filter(time_period_label == baseline_year) %>%
  select(
    new_la_code_current,
    baseline_rate = rate_per_1000
  )

rate_growth_summary <- ehcp_rates %>%
  filter(time_period_label == latest_year) %>%
  left_join(baseline_rates, by = "new_la_code_current") %>%
  mutate(
    rate_growth_percent_since_baseline =
      100 * (rate_per_1000 - baseline_rate) / baseline_rate
  ) %>%
  summarise(
    local_authorities = n_distinct(new_la_code_current),
    mean_growth = round(mean(rate_growth_percent_since_baseline, na.rm = TRUE), 1),
    median_growth = round(median(rate_growth_percent_since_baseline, na.rm = TRUE), 1),
    q1 = round(quantile(rate_growth_percent_since_baseline, 0.25, na.rm = TRUE), 1),
    q3 = round(quantile(rate_growth_percent_since_baseline, 0.75, na.rm = TRUE), 1),
    min_growth = round(min(rate_growth_percent_since_baseline, na.rm = TRUE), 1),
    max_growth = round(max(rate_growth_percent_since_baseline, na.rm = TRUE), 1)
  )


# ------ rate summary over time ------

rate_summary_over_time <- ehcp_rates %>%
  group_by(time_period_label) %>%
  summarise(
    local_authorities = n_distinct(new_la_code_current),
    denominator = if_else(
      any(pop_estimated %in% TRUE),
      "Projected: ONS 2022-based SNPP",
      "Actual: ONS mid-year estimate"
    ),
    mean_rate = round(mean(rate_per_1000, na.rm = TRUE), 1),
    median_rate = round(median(rate_per_1000, na.rm = TRUE), 1),
    q1 = round(quantile(rate_per_1000, 0.25, na.rm = TRUE), 1),
    q3 = round(quantile(rate_per_1000, 0.75, na.rm = TRUE), 1),
    min_rate = round(min(rate_per_1000, na.rm = TRUE), 1),
    max_rate = round(max(rate_per_1000, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(time_period_label)


# ------ diagnostics A: population coverage and join ------

cat("\n[A1] Population coverage by year and source:\n")

pop_data %>%
  count(
    year,
    source = if_else(
      pop_estimated,
      "ONS 2022-based SNPP projection",
      "ONS mid-year estimate via NOMIS"
    )
  ) %>%
  arrange(year) %>%
  print(n = Inf)


cat("\n[A2] Percentage of EHCP rows matched to population:\n")

ehcp_rates %>%
  group_by(academic_year = as.character(time_period_label)) %>%
  summarise(
    n_la = n(),
    pct_joined = round(mean(!is.na(pop_0_25)) * 100, 1),
    pct_projected = round(mean(pop_estimated %in% TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  print(n = Inf)


cat("\n[A3] EHCP rows with no population match:\n")

a3 <- ehcp_rates %>%
  filter(is.na(pop_0_25)) %>%
  count(la_name, new_la_code, new_la_code_current, sort = TRUE)

if (nrow(a3) == 0) cat("  none\n") else print(a3, n = Inf)


cat("\n[A4] Population codes never matched to EHCP data:\n")

a4 <- pop_data %>%
  distinct(ons_code_current, authority) %>%
  anti_join(
    ehcp_rates %>% distinct(new_la_code_current),
    by = c("ons_code_current" = "new_la_code_current")
  )

if (nrow(a4) == 0) cat("  none\n") else print(a4, n = Inf)


cat("\n[A5] Reorganised authority spot-check:\n")

ehcp_rates %>%
  filter(
    la_name %in% c(
      "Buckinghamshire",
      "Northamptonshire",
      "North Northamptonshire",
      "West Northamptonshire",
      "Cumbria",
      "Cumberland",
      "Westmorland and Furness",
      "Dorset"
    )
  ) %>%
  transmute(
    la_name,
    academic_year = as.character(time_period_label),
    new_la_code,
    new_la_code_current,
    pop_0_25,
    rate_per_1000 = round(rate_per_1000, 1),
    pop_estimated
  ) %>%
  arrange(la_name, academic_year) %>%
  print(n = Inf)


cat("\n[A6] Authorities where 2025 SNPP differs from 2024 MYE by more than 5%:\n")

a6 <- pop_data %>%
  filter(year %in% c("2024/25", "2025/26")) %>%
  select(ons_code_current, authority, year, pop_0_25) %>%
  pivot_wider(names_from = year, values_from = pop_0_25) %>%
  filter(!is.na(`2024/25`), !is.na(`2025/26`)) %>%
  mutate(change_pc = round(100 * (`2025/26` / `2024/25` - 1), 1)) %>%
  filter(abs(change_pc) > 5) %>%
  arrange(desc(abs(change_pc)))

if (nrow(a6) == 0) cat("  none\n") else print(a6, n = Inf)


cat("\n[A7] Duplicate authority-year rows in ehcp_rates:\n")

a7 <- ehcp_rates %>%
  count(new_la_code_current, time_period_label) %>%
  filter(n > 1)

if (nrow(a7) == 0) cat("  none\n") else print(a7, n = Inf)


# ------ diagnostics C: tribunal recency ------

required_tribunal_objects <- c(
  "requests_raw",
  "assessments_raw",
  "la_requests",
  "la_assessments",
  "la_tribunal"
)

if (all(vapply(required_tribunal_objects, exists, logical(1)))) {
  
  cat("\n[C1] Raw calendar years loaded:\n")
  cat(
    "  requests_raw:    ",
    paste(sort(unique(as.integer(requests_raw$time_period))), collapse = ", "),
    "\n"
  )
  cat(
    "  assessments_raw: ",
    paste(sort(unique(as.integer(assessments_raw$time_period))), collapse = ", "),
    "\n"
  )
  
  cat("\n[C2] LA counts per academic year after cleaning:\n")
  
  bind_rows(
    la_requests %>%
      count(academic_year) %>%
      mutate(table = "la_requests"),
    la_assessments %>%
      count(academic_year) %>%
      mutate(table = "la_assessments")
  ) %>%
    pivot_wider(names_from = table, values_from = n) %>%
    arrange(academic_year) %>%
    print(n = Inf)
  
  cat("\n[C3] Percentage non-missing in key columns:\n")
  
  la_tribunal %>%
    filter(academic_year %in% c("2024/25", "2025/26")) %>%
    group_by(academic_year) %>%
    summarise(
      n_la = n(),
      pct_requests = round(mean(!is.na(requests_received_in_year)) * 100, 1),
      pct_gate1_inputs = round(mean(!is.na(requests_decided_to_assess)) * 100, 1),
      pct_assess_issued = round(mean(!is.na(assess_issued)) * 100, 1),
      pct_tribunal = round(mean(!is.na(tribunal_related_request)) * 100, 1),
      .groups = "drop"
    ) %>%
    print(n = Inf)
  
  stopifnot(
    "requests_raw is missing calendar year 2025" =
      2025 %in% as.integer(requests_raw$time_period),
    "assessments_raw is missing calendar year 2025" =
      2025 %in% as.integer(assessments_raw$time_period)
  )
  
  cat("\n[C4] Tribunal recency assertions passed\n")
  
  cat("\n[C5] Known suppressed authorities in 2025/26:\n")
  
  la_tribunal %>%
    filter(
      academic_year == "2025/26",
      la_name %in% c("Barnet", "Leicester", "Isle of Wight")
    ) %>%
    select(
      la_name,
      requests_received_in_year,
      assess_issued,
      tribunal_after_mediation_request
    ) %>%
    print()
} else {
  cat("\n[Diagnostics C skipped: required tribunal objects were not found]\n")
}


