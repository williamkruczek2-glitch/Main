

library(tidyverse)


# ------ join tribunal data onto EHCP rates (old_la_code = stable key) ------

master_la_table <- ehcp_rates %>%
  mutate(academic_year = as.character(time_period_label),
         old_la_code = as.character(old_la_code)) %>%
  left_join(
    la_tribunal %>% select(-la_name, -region_name) %>%
      mutate(la_id = as.character(la_id)),
    by = c("old_la_code" = "la_id", "academic_year")
  ) %>%
  left_join(
    la_tribunal %>%
      filter(!is.na(region_name), region_name != "") %>%
      distinct(la_id, region_name),
    by = c("old_la_code" = "la_id")
  ) %>%
  mutate(la_id = old_la_code)

# ------ fill regions by current code (covers BCP/Dorset 2018/19, whose old
# ------ codes never appear in the tribunal data) ------

region_fill <- master_la_table %>%
  filter(!is.na(region_name), region_name != "") %>%
  distinct(new_la_code_current, fill_region = region_name)
master_la_table <- master_la_table %>%
  left_join(region_fill, by = "new_la_code_current") %>%
  mutate(region_name = coalesce(region_name, fill_region)) %>%
  select(
    academic_year, la_id, new_la_code, new_la_code_current, old_la_code,
    la_name, region_name,
    ehcplans, pop_0_25, rate_per_1000, pop_estimated,
    requests_received_in_year, requests_decided_to_assess, requests_decided_not_to_assess,
    gate1_approval_rate,
    assess_in_year, assess_issued, assess_not_issued, assess_withdrawn,
    post_assess_approval, end_to_end_approval,
    refusal_mediation_rate, refusal_tribunal_rate,
    content_mediation_rate, content_tribunal_rate,
    mediation_rate, tribunal_rate,
    mediation_to_tribunal_pc
  ) %>%
  arrange(la_name, academic_year)


# ------ diagnostics B: master table ------

cat("\n[B1] coverage by year:\n")
master_la_table %>%
  group_by(academic_year) %>%
  summarise(
    n_la                = n(),
    pct_has_ehcp_rate   = round(mean(!is.na(rate_per_1000)) * 100, 1),
    pct_has_gate1       = round(mean(!is.na(gate1_approval_rate)) * 100, 1),
    pct_has_post_assess = round(mean(!is.na(post_assess_approval)) * 100, 1),
    pct_has_tribunal    = round(mean(!is.na(tribunal_rate)) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(academic_year) %>%
  as_tibble() %>% print(n = Inf)

cat("\n[B2] duplicate LA x year rows (by current code):\n")
b2 <- master_la_table %>% count(new_la_code_current, academic_year) %>% filter(n > 1)
if (nrow(b2) == 0) cat("  none \u2713\n") else print(as_tibble(b2), n = Inf)

cat("\n[B3] rows with missing region_name, by year:\n")
b3 <- master_la_table %>%
  group_by(academic_year) %>%
  summarise(n_missing_region = sum(is.na(region_name) | region_name == ""),
            .groups = "drop") %>%
  filter(n_missing_region > 0)
if (nrow(b3) == 0) cat("  none \u2713\n") else print(as_tibble(b3), n = Inf)

cat("\n[B4] implausible values (rate>200/1000 or approval outside 0-100):\n")
b4 <- master_la_table %>%
  filter(rate_per_1000 > 200 |
           gate1_approval_rate  < 0 | gate1_approval_rate  > 100 |
           post_assess_approval < 0 | post_assess_approval > 100) %>%
  select(la_name, academic_year, rate_per_1000,
         gate1_approval_rate, post_assess_approval)
if (nrow(b4) == 0) cat("  none \u2713\n") else print(as_tibble(b4), n = Inf)

cat("\n[B5] reorganised-LA spot-check (expect rates in the years each existed):\n")
master_la_table %>%
  filter(la_name %in% c("Cumbria", "Cumberland", "Westmorland and Furness",
                        "Northamptonshire", "North Northamptonshire",
                        "West Northamptonshire", "Dorset",
                        "Bournemouth, Christchurch and Poole")) %>%
  transmute(la_name, academic_year, rate_per_1000 = round(rate_per_1000, 1),
            has_tribunal = !is.na(tribunal_rate)) %>%
  arrange(la_name, academic_year) %>%
  as_tibble() %>% print(n = Inf)

stopifnot(
  "duplicate LA x year rows in master_la_table" = nrow(b2) == 0,
  "master table missing 2025/26 tribunal data - check 01's release URLs" =
    master_la_table %>% filter(academic_year == "2025/26",
                               !is.na(gate1_approval_rate)) %>% nrow() > 0
)

