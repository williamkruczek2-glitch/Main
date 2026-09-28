
library(sf)

# ------ analysis years ------

if (!exists("BASE_YEAR"))   BASE_YEAR   <- "2018/19"
if (!exists("LATEST_YEAR")) LATEST_YEAR <- "2025/26"

year_tag <- function(y) str_replace_all(y, "/", "-")

boundary_url <- paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BGC/",
  "FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
)
boundary_clean_df <- read_sf(boundary_url) %>%
  rename(ons_code = CTYUA24CD, authority_name = CTYUA24NM) %>%
  select(ons_code, authority_name, geometry) %>%
  filter(str_starts(ons_code, "E"))

# ------ build the two metrics ------

prevalence_latest <- master_la_table %>%
  filter(academic_year == LATEST_YEAR, !is.na(rate_per_1000)) %>%
  select(new_la_code_current, la_name, region_name,
         value = rate_per_1000)

growth <- master_la_table %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR)) %>%
  mutate(endpoint = if_else(academic_year == BASE_YEAR, "base", "latest")) %>%
  select(new_la_code_current, la_name, region_name, endpoint, ehcplans) %>%
  pivot_wider(names_from = endpoint, values_from = ehcplans) %>%
  filter(!is.na(base), !is.na(latest), base > 0) %>%
  mutate(value = 100 * (latest / base - 1)) %>%
  select(new_la_code_current, la_name, region_name, value)

# ------ quintile bins ------

bin_quintile <- function(x, digits = 1, suffix = "") {
  br   <- quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE)
  labs <- paste0(round(head(br, -1), digits), suffix, "–",
                 round(tail(br, -1), digits), suffix)
  cut(x, breaks = br, include.lowest = TRUE, labels = labs)
}

prevalence_latest <- prevalence_latest %>%
  mutate(bin = bin_quintile(value, digits = 1))
growth <- growth %>%
  mutate(bin = bin_quintile(value, digits = 0, suffix = "%"))

# ------ discrete-fill map helper ------

make_la_map_binned <- function(data, title, fill_label,
                               palette = c("#F0E5EF", "#C9AACB", "#9F72A6",
                                           "#6B3A73", "#3D1A44")) {
  map_data <- boundary_clean_df %>%
    left_join(data %>% select(new_la_code_current, bin),
              by = c("ons_code" = "new_la_code_current"))
  
  england_map <- ggplot(map_data) +
    geom_sf(aes(fill = bin), colour = "white", linewidth = 0.15) +
    scale_fill_manual(values = palette, name = fill_label, na.value = "grey90") +
    theme_void(base_family = mhclg_font) +
    theme(plot.title      = element_text(face = "bold", colour = mhclg_teal),
          legend.position = "right") +
    labs(title   = title,
         caption = "Contains National Statistics and OS data (c) Crown copyright and database right 2024")
  
  london_map <- ggplot(map_data %>% filter(str_starts(ons_code, "E09"))) +
    geom_sf(aes(fill = bin), colour = "white", linewidth = 0.3) +
    scale_fill_manual(values = palette, name = fill_label, na.value = "grey90") +
    theme_void(base_family = mhclg_font) +
    theme(plot.title      = element_text(face = "bold", colour = mhclg_teal),
          legend.position = "right") +
    labs(title   = paste0(title, " — London boroughs"),
         caption = "Contains National Statistics and OS data (c) Crown copyright and database right 2024")
  
  print(england_map)
  print(london_map)
  invisible(list(england = england_map, london = london_map))
}

# ------ duplicate-row checks ------

master_la_table %>% count(new_la_code_current, academic_year) %>% filter(n > 1)
la_requests    %>% count(la_id, academic_year) %>% filter(n > 1)
la_assessments %>% count(la_id, academic_year) %>% filter(n > 1)
ehcp_la        %>% count(new_la_code_current, time_period_label) %>% filter(n > 1)

# ------ build and save maps ------

map_prev <- make_la_map_binned(
  prevalence_latest,
  title      = paste0("EHCP rate, ", LATEST_YEAR),
  fill_label = "Plans per\n1,000 (0–25)"
)

ggsave(paste0("outputs/map_ehcp_rate_", year_tag(LATEST_YEAR), "_england.png"),
       map_prev$england, width = 8, height = 9, dpi = 300, bg = "white")
ggsave(paste0("outputs/map_ehcp_rate_", year_tag(LATEST_YEAR), "_london.png"),
       map_prev$london,  width = 8, height = 7, dpi = 300, bg = "white")

map_growth <- make_la_map_binned(
  growth,
  title      = paste0("EHCP count growth, ", BASE_YEAR, " to ", LATEST_YEAR),
  fill_label = "% change"
)

ggsave(paste0("outputs/map_ehcp_growth_", year_tag(BASE_YEAR), "_to_",
              year_tag(LATEST_YEAR), "_england.png"),
       map_growth$england, width = 8, height = 9, dpi = 300, bg = "white")
ggsave(paste0("outputs/map_ehcp_growth_", year_tag(BASE_YEAR), "_to_",
              year_tag(LATEST_YEAR), "_london.png"),
       map_growth$london,  width = 8, height = 7, dpi = 300, bg = "white")

# ------ validating negative growth: rate vs count ------

endpoint_slice <- function(yr, suffix) {
  master_la_table %>%
    filter(academic_year == yr) %>%
    transmute(
      new_la_code_current,
      !!paste0("rate_per_1000", suffix) := rate_per_1000,
      !!paste0("ehcplans",      suffix) := ehcplans,
      !!paste0("pop_0_25",      suffix) := pop_0_25
    )
}

la_labels <- master_la_table %>%
  filter(academic_year == LATEST_YEAR) %>%
  distinct(new_la_code_current, la_name, region_name)

endpoints <- la_labels %>%
  left_join(endpoint_slice(BASE_YEAR,   "_base"),   by = "new_la_code_current") %>%
  left_join(endpoint_slice(LATEST_YEAR, "_latest"), by = "new_la_code_current") %>%
  mutate(
    rate_change_pct    = 100 * (rate_per_1000_latest / rate_per_1000_base - 1),
    plans_change_pct   = 100 * (ehcplans_latest      / ehcplans_base      - 1),
    pop_change_pct     = 100 * (pop_0_25_latest      / pop_0_25_base      - 1),
    absolute_rate_diff = rate_per_1000_latest - rate_per_1000_base,
    absolute_plan_diff = ehcplans_latest      - ehcplans_base
  )

# ------ check A: negative rate change ------

cat("\n=== LAs WITH NEGATIVE RATE CHANGE (rate per 1,000 fell) ===\n")

neg_rate <- endpoints %>%
  filter(!is.na(rate_change_pct), rate_change_pct < 0) %>%
  arrange(rate_change_pct) %>%
  mutate(
    reason = case_when(
      plans_change_pct <  0 & pop_change_pct <  0 ~ "Both fell — plans down more than pop",
      plans_change_pct <  0 & pop_change_pct >= 0 ~ "Plans fell (real caseload decline)",
      plans_change_pct >= 0 & pop_change_pct >  0 &
        pop_change_pct > plans_change_pct         ~ "Pop grew faster than plans (denominator effect)",
      plans_change_pct == 0                       ~ "Plans flat — pop grew",
      TRUE                                        ~ "Other / check manually"
    )
  )

cat("\nTotal LAs with negative rate change:", nrow(neg_rate), "\n\n")

neg_rate %>%
  transmute(
    la_name, region_name,
    rate_base   = round(rate_per_1000_base,   1),
    rate_latest = round(rate_per_1000_latest, 1),
    rate_pct  = round(rate_change_pct, 1),
    plans_pct = round(plans_change_pct, 1),
    pop_pct   = round(pop_change_pct, 1),
    reason
  ) %>% print(n = Inf)

cat("\n=== BREAKDOWN OF WHY THE RATE FELL ===\n")
neg_rate %>%
  count(reason, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ------ check B: negative plans change ------

cat("\n=== LAs WITH NEGATIVE PLANS CHANGE (raw plan count fell) ===\n")

neg_plans <- endpoints %>%
  filter(!is.na(plans_change_pct), plans_change_pct < 0) %>%
  arrange(plans_change_pct) %>%
  mutate(
    scale = case_when(
      abs(absolute_plan_diff) <  50  ~ "Small absolute drop (<50 plans)",
      abs(absolute_plan_diff) <  200 ~ "Moderate drop (50–200 plans)",
      abs(absolute_plan_diff) >= 200 ~ "Large drop (200+ plans)"
    ),
    context = case_when(
      pop_change_pct < 0 ~ "Pop also fell (declining LA)",
      pop_change_pct > 0 ~ "Pop grew (unusual — plans fell against demographic tide)",
      TRUE               ~ "Pop unchanged"
    )
  )

cat("\nTotal LAs where the absolute number of EHC plans fell:", nrow(neg_plans), "\n")

if (nrow(neg_plans) == 0) {
  cat("\n(No LAs saw an absolute decline in plan counts. Any negative rate change\n")
  cat("is a denominator effect.)\n")
} else {
  cat("\n=== DETAIL ===\n")
  neg_plans %>%
    transmute(
      la_name, region_name,
      plans_base    = ehcplans_base,
      plans_latest  = ehcplans_latest,
      absolute_diff = absolute_plan_diff,
      plans_pct     = round(plans_change_pct, 1),
      rate_pct      = round(rate_change_pct, 1),
      pop_pct       = round(pop_change_pct, 1),
      scale, context
    ) %>% print(n = Inf)
  
  cat("\n=== SCALE OF DROP ===\n")
  neg_plans %>% count(scale, sort = TRUE) %>% print()
  
  cat("\n=== POPULATION CONTEXT ===\n")
  neg_plans %>% count(context, sort = TRUE) %>% print()
}

# ------ cross-check: rate and plans both fell ------

cat("\n=== LAs WHERE BOTH THE RATE AND PLAN COUNT FELL ===\n")

both_neg <- endpoints %>%
  filter(!is.na(rate_change_pct), !is.na(plans_change_pct),
         rate_change_pct < 0, plans_change_pct < 0) %>%
  arrange(plans_change_pct)

cat("\nTotal LAs with declines on BOTH measures:", nrow(both_neg), "\n")

if (nrow(both_neg) > 0) {
  both_neg %>%
    transmute(
      la_name, region_name,
      rate_pct  = round(rate_change_pct, 1),
      plans_pct = round(plans_change_pct, 1),
      pop_pct   = round(pop_change_pct, 1),
      abs_plans = absolute_plan_diff
    ) %>% print(n = Inf)
}

# ------ boundary change flags ------

cat("\n=== BOUNDARY-CHANGE LAs FLAGGED IN NEGATIVE LISTS ===\n")
boundary_change_las <- c(
  "Buckinghamshire", "North Northamptonshire", "West Northamptonshire",
  "Cumberland", "Westmorland and Furness",
  "Somerset", "North Yorkshire"
)

boundary_hits <- endpoints %>%
  filter(la_name %in% boundary_change_las,
         (rate_change_pct < 0 | plans_change_pct < 0)) %>%
  select(la_name, rate_change_pct, plans_change_pct, pop_change_pct) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

if (nrow(boundary_hits) > 0) {
  print(boundary_hits, n = Inf)
  cat("\nThese LAs had boundary/code changes since ", BASE_YEAR,
      " — apparent declines\n", sep = "")
  cat("may reflect reorganisation, not real change.\n")
} else {
  cat("(none of the negative-growth LAs are on the known boundary-change list)\n")
}

# ------ exports ------

if (!dir.exists("outputs")) dir.create("outputs")
write_csv(neg_rate,  "outputs/negative_rate_change_las.csv")
write_csv(neg_plans, "outputs/negative_plans_change_las.csv")
write_csv(both_neg,  "outputs/negative_both_measures_las.csv")

cat("\nExports written to /outputs\n")

# ------ map NA diagnostics ------

boundary_las <- read_sf(paste0(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/",
  "Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_BGC/",
  "FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
)) %>%
  st_drop_geometry() %>%
  rename(ons_code = CTYUA24CD, authority_name = CTYUA24NM) %>%
  filter(str_starts(ons_code, "E")) %>%
  select(ons_code, authority_name)

# ------ NAs on the rate map ------

rate_latest <- master_la_table %>%
  filter(academic_year == LATEST_YEAR) %>%
  select(new_la_code_current, la_name, rate_per_1000)

rate_map_join <- boundary_las %>%
  left_join(rate_latest, by = c("ons_code" = "new_la_code_current"))

cat("\n=== RATE MAP NAs (", LATEST_YEAR, ") ===\n", sep = "")

rate_nas <- rate_map_join %>%
  filter(is.na(rate_per_1000)) %>%
  mutate(reason = case_when(
    is.na(la_name) ~ "LA not found in master table (code/boundary mismatch)",
    TRUE           ~ "In master table but rate_per_1000 is NA"
  ))

cat("\nTotal LAs shown grey on rate map:", nrow(rate_nas), "\n\n")
print(rate_nas, n = Inf)

# ------ NAs on the growth map ------

growth_pair <- master_la_table %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR)) %>%
  mutate(endpoint = if_else(academic_year == BASE_YEAR, "base", "latest")) %>%
  select(new_la_code_current, la_name, endpoint, ehcplans) %>%
  pivot_wider(names_from = endpoint, values_from = ehcplans)

growth_calc <- growth_pair %>%
  mutate(growth_pct = 100 * (latest / base - 1))

growth_map_join <- boundary_las %>%
  left_join(growth_calc, by = c("ons_code" = "new_la_code_current"))

cat("\n=== GROWTH MAP NAs (", BASE_YEAR, " -> ", LATEST_YEAR, ") ===\n", sep = "")

growth_nas <- growth_map_join %>%
  filter(is.na(growth_pct) | growth_pct < 0) %>%
  mutate(reason = case_when(
    is.na(la_name)              ~ "LA not in master table (code/boundary mismatch)",
    is.na(base) & is.na(latest) ~ "Missing both years",
    is.na(base)                 ~ paste0("Missing ", BASE_YEAR,
                                         " baseline (LA didn't exist / didn't report)"),
    is.na(latest)               ~ paste0("Missing ", LATEST_YEAR, " latest year"),
    base == 0                   ~ "Zero baseline (growth undefined)",
    growth_pct < 0              ~ "Negative growth (excluded per method)"
  ))

cat("\nTotal LAs shown grey on growth map:", nrow(growth_nas), "\n\n")
print(growth_nas %>%
        select(ons_code, authority_name, la_name,
               plans_base = base, plans_latest = latest,
               growth_pct, reason) %>%
        mutate(across(where(is.numeric), ~ round(., 1))),
      n = Inf)

cat("\n=== NA REASONS — SUMMARY ===\n")
growth_nas %>% count(reason, sort = TRUE) %>% print()

if (!dir.exists("outputs")) dir.create("outputs")
write_csv(rate_nas,   "outputs/map_na_rate.csv")
write_csv(growth_nas, "outputs/map_na_growth.csv")

cat("\nWritten to outputs/map_na_rate.csv and outputs/map_na_growth.csv\n")

# ------ known ONS boundary changes since the baseline year ------

ons_changes <- tribble(
  ~ons_code,     ~authority_name,              ~created,   ~predecessors,
  "E06000060",   "Buckinghamshire",            "2020/21",  "4x districts merged into unitary",
  "E06000061",   "North Northamptonshire",     "2021/22",  "Corby, East Northants, Kettering, Wellingborough",
  "E06000062",   "West Northamptonshire",      "2021/22",  "Daventry, Northampton, South Northants",
  "E06000063",   "Cumberland",                 "2023/24",  "Allerdale, Carlisle, Copeland",
  "E06000064",   "Westmorland and Furness",    "2023/24",  "Barrow, Eden, South Lakeland",
  "E06000065",   "North Yorkshire",            "2023/24",  "7x districts + N. Yorks CC",
  "E06000066",   "Somerset",                   "2023/24",  "Mendip, S Somerset, Sedgemoor, Somerset West & Taunton"
)

# ------ earliest and latest year per LA ------

earliest_data <- master_la_table %>%
  filter(!is.na(ehcplans)) %>%
  group_by(new_la_code_current, la_name) %>%
  summarise(
    earliest_year = min(academic_year, na.rm = TRUE),
    latest_year   = max(academic_year, na.rm = TRUE),
    n_years       = n_distinct(academic_year),
    .groups       = "drop"
  )

plans_pair <- master_la_table %>%
  filter(academic_year %in% c(BASE_YEAR, LATEST_YEAR)) %>%
  mutate(endpoint = if_else(academic_year == BASE_YEAR, "base", "latest")) %>%
  select(new_la_code_current, la_name, endpoint, ehcplans) %>%
  pivot_wider(names_from = endpoint, values_from = ehcplans)

# ------ root cause per boundary LA ------

diagnostic <- boundary_las %>%
  left_join(plans_pair, by = c("ons_code" = "new_la_code_current")) %>%
  left_join(earliest_data %>% select(-la_name),
            by = c("ons_code" = "new_la_code_current")) %>%
  left_join(ons_changes %>% select(ons_code, created),
            by = "ons_code") %>%
  mutate(
    growth_pct = 100 * (latest / base - 1),
    na_on_map  = is.na(growth_pct) | (!is.na(growth_pct) & growth_pct < 0),
    root_cause = case_when(
      is.na(la_name)  ~ "Not in master table at all — data extract missing this LA",
      !is.na(created) ~ paste0("ONS boundary change — LA created ", created),
      !is.na(latest) & !is.na(base) & growth_pct < 0
      ~ "Negative growth excluded",
      is.na(base) & !is.na(latest)
      ~ paste0("Data-reporting gap — no ", BASE_YEAR, " plan count reported"),
      is.na(latest) & !is.na(base)
      ~ paste0("Data-reporting gap — no ", LATEST_YEAR, " plan count reported"),
      is.na(base) & is.na(latest)
      ~ "Data-reporting gap — no plan count reported in either year",
      TRUE            ~ "No issue — appears on map"
    )
  )

cat("\n=== WHY IS EACH LA NA ON THE GROWTH MAP? ===\n")

na_diagnostic <- diagnostic %>% filter(na_on_map)

cat("\nTotal LAs shown grey (NA) on growth map:", nrow(na_diagnostic), "\n\n")

na_diagnostic %>%
  select(authority_name, root_cause,
         earliest_year, plans_base = base, plans_latest = latest) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  arrange(root_cause, authority_name) %>%
  print(n = Inf)

cat("\n=== BREAKDOWN BY ROOT CAUSE ===\n")
na_diagnostic %>% count(root_cause, sort = TRUE) %>% print()

# ------ coverage by year ------

coverage_by_year <- master_la_table %>%
  filter(!is.na(ehcplans)) %>%
  group_by(academic_year) %>%
  summarise(las_reporting = n_distinct(new_la_code_current), .groups = "drop") %>%
  arrange(academic_year)

cat("\n=== LAs REPORTING ehcplans BY YEAR ===\n")
print(coverage_by_year)

cat("\n=== INTERPRETATION ===\n")
cat("- Baseline in use:", BASE_YEAR, "\n")
cat("- 2018/19 and 2019/20 baselines: exclude 4 unitaries created in 2020-21 and 6 more in 2021-24\n")
cat("- 2021/22 baseline: covers Buckinghamshire, N/W Northants but excludes the 4x 2023 unitaries\n")
cat("- 2023/24 baseline: first year where all current LA codes exist as reporting units\n")
cat("- Recommendation: keep", BASE_YEAR, "->", LATEST_YEAR,
    "as headline, footnote the NA LAs\n")

# ------ alternative: growth from each LA's own earliest year ------

la_first_last <- master_la_table %>%
  filter(!is.na(ehcplans)) %>%
  group_by(new_la_code_current, la_name) %>%
  arrange(academic_year) %>%
  summarise(
    earliest_year  = first(academic_year),
    earliest_plans = first(ehcplans),
    latest_year    = last(academic_year),
    latest_plans   = last(ehcplans),
    growth_from_earliest_pct = 100 * (last(ehcplans) / first(ehcplans) - 1),
    .groups = "drop"
  )

recovered <- la_first_last %>%
  filter(new_la_code_current %in% na_diagnostic$ons_code,
         !is.na(growth_from_earliest_pct))

cat("\n=== LAs PREVIOUSLY NA THAT COULD BE PLOTTED FROM THEIR OWN BASELINE ===\n")
recovered %>%
  select(la_name, earliest_year, latest_year, growth_from_earliest_pct) %>%
  mutate(growth_from_earliest_pct = round(growth_from_earliest_pct, 1)) %>%
  print(n = Inf)

