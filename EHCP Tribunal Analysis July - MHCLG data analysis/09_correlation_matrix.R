library(scales)



# ------ analysis years ------

if (!exists("BASE_YEAR"))   BASE_YEAR   <- "2018/19"
if (!exists("LATEST_YEAR")) LATEST_YEAR <- "2025/26"

# ------ build corr_data ------

ehcp_growth_lookup <- master_la_table %>%
  filter(!is.na(ehcplans)) %>%
  group_by(la_id) %>%
  summarise(
    base_plans   = ehcplans[academic_year == BASE_YEAR][1],
    latest_plans = ehcplans[academic_year == LATEST_YEAR][1],
    .groups = "drop"
  ) %>%
  filter(!is.na(base_plans), !is.na(latest_plans), base_plans > 0) %>%
  transmute(la_id, ehcp_growth_pc = 100 * (latest_plans / base_plans - 1))

corr_data <- master_la_table %>%
  mutate(
    request_rate_per_1000 = 1000 * requests_received_in_year / pop_0_25,
    assess_rate_per_1000  = 1000 * assess_in_year / pop_0_25
  ) %>%
  left_join(ehcp_growth_lookup, by = "la_id")

# ------ cor_pair(): Pearson headline, Spearman diagnostic ------

cor_pair <- function(df, col_x, col_y) {
  d <- df %>%
    select(x = all_of(col_x), y = all_of(col_y)) %>%
    filter(!is.na(x), !is.na(y))
  if (nrow(d) < 3) {
    return(tibble(n = nrow(d), pearson = NA_real_,
                  p_value = NA_real_, spearman = NA_real_))
  }
  ct  <- cor.test(d$x, d$y, method = "pearson")
  rho <- suppressWarnings(cor(d$x, d$y, method = "spearman"))
  tibble(n = nrow(d), pearson = unname(ct$estimate),
         p_value = ct$p.value, spearman = rho)
}

# ------ join IDACI ------

# --- Join IDACI (UTLA 2024 codes) onto the analysis data ---
# Fail with a useful message if the data file didn't make it into the
# repo (a data/ folder in .gitignore is the usual culprit).


idaci_clean <- read_csv("data/LDACI_file11.csv", show_col_types = FALSE) %>%
  rename_with(str_trim) %>%
  transmute(
    ons_code    = as.character(`Upper Tier Local Authority District code (2024)`),
    idaci_score = as.numeric(`IDACI - Average score`)
  ) %>%
  filter(!is.na(ons_code), !is.na(idaci_score))

# ------ EHCP share of identified SEN population ------

# Numerator and denominator both come from the school census, which is a
# different universe from the SEN2 counts in master_la_table.

SEN_URL <- paste0(
  "https://explore-education-statistics.service.gov.uk/data-catalogue/",
  "data-set/730c131b-6383-443d-ae53-880a69799f11/csv"
)

MIN_SEN_TOTAL <- 100

provision_map <- c(
  "Education, health and care plan"       = "ehcp_census",
  "SEN support / SEN without an EHC plan" = "sen_support",
  "Total"                                 = "total_pupils"
)

to_num <- function(x) {
  x <- str_trim(as.character(x))
  x[tolower(x) %in% c("x", "z", "c", ":", "low", "..", "n/a", "", "no data")] <- NA
  suppressWarnings(as.numeric(x))
}

sen_raw <- read_csv(SEN_URL, col_types = cols(.default = col_character()),
                    show_col_types = FALSE)

stopifnot(
  "wrong SEN dataset - check SEN_URL" =
    all(c("sen_provision", "phase_type_grouping", "hospital_school",
          "establishment_type", "pupil_count") %in% names(sen_raw)),
  "SEN_URL is stale - repoint at the latest release" =
    max(as.integer(sen_raw$time_period), na.rm = TRUE) >= 202526,
  "unmapped sen_provision values" =
    all(unique(sen_raw$sen_provision) %in% names(provision_map))
)

sen_share <- sen_raw %>%
  filter(geographic_level    == "Local authority",
         phase_type_grouping == "Total",
         hospital_school     == "Total",
         establishment_type  == "Total") %>%
  transmute(
    academic_year = paste0(str_sub(time_period, 1, 4), "/", str_sub(time_period, 5, 6)),
    la_code       = normalise_geog(as.character(new_la_code)),
    provision     = unname(provision_map[sen_provision]),
    pupil_count   = to_num(pupil_count)
  ) %>%
  group_by(academic_year, la_code, provision) %>%
  summarise(
    pupil_count = if (any(is.na(pupil_count))) NA_real_ else sum(pupil_count),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = provision, values_from = pupil_count) %>%
  mutate(sen_total = sen_support + ehcp_census) %>%
  transmute(
    academic_year, la_code,
    ehcp_share_of_sen = if_else(sen_total >= MIN_SEN_TOTAL,
                                100 * ehcp_census / sen_total, NA_real_)
  )

stopifnot(
  "no SEN rows survived the all-Total filter" = nrow(sen_share) > 0,
  "ehcp_share_of_sen outside 0-100" =
    all(between(sen_share$ehcp_share_of_sen, 0, 100), na.rm = TRUE)
)

corr_data <- corr_data %>%
  mutate(new_la_code_current = normalise_geog(new_la_code)) %>%
  left_join(idaci_clean, by = c("new_la_code_current" = "ons_code")) %>%
  left_join(sen_share, by = c("new_la_code_current" = "la_code", "academic_year"))

# ------ matrix variables ------

all_vars <- tribble(
  ~label,                                        ~col,
  "EHCP rate",                                   "rate_per_1000",
  "EHCP growth",                                 "ehcp_growth_pc",
  "% EHCPs as share of SEN pupil population",    "ehcp_share_of_sen",
  "IDACI deprivation",                           "idaci_score",
  "EHCP assessment request rate",                "request_rate_per_1000",
  "% of requests for EHCP assessment approved",  "gate1_approval_rate",
  "EHCP assessment rate",                        "assess_rate_per_1000",
  "% of assessments resulting in an EHCP",       "post_assess_approval",
  "EHCP mediation rate",                         "mediation_rate",
  "% of mediation cases going to Tribunal",      "mediation_to_tribunal_pc",
  "Tribunal rate",                               "tribunal_rate"
)

display_vars <- all_vars$label[1:4]

pairwise <- expand_grid(i = seq_len(nrow(all_vars)),
                        j = seq_len(nrow(all_vars))) %>%
  filter(i >= j) %>%
  mutate(
    var_row = all_vars$label[i], col_row = all_vars$col[i],
    var_col = all_vars$label[j], col_col = all_vars$col[j]
  ) %>%
  rowwise() %>%
  mutate(result = list(
    if (col_row == col_col) tibble(n = NA_integer_, pearson = 1, p_value = NA_real_, spearman = 1)
    else cor_pair(corr_data, col_row, col_col)
  )) %>%
  ungroup() %>%
  mutate(
    r = map_dbl(result, ~ .x$pearson),
    p = map_dbl(result, ~ .x$p_value),
    n = map_int(result, ~ .x$n)
  )

star <- function(p) {
  case_when(is.na(p) ~ "", p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
}

pairwise_table <- pairwise %>%
  filter(var_col %in% display_vars) %>%
  mutate(cell = ifelse(col_row == col_col, "1", sprintf("%.2f%s", r, star(p)))) %>%
  select(var_row, var_col, cell) %>%
  pivot_wider(names_from = var_col, values_from = cell, values_fill = "") %>%
  mutate(var_row = factor(var_row, levels = all_vars$label)) %>%
  arrange(var_row) %>%
  rename(Variable = var_row)

kable(pairwise_table,
      caption = paste("Pairwise Pearson correlations against headline measures.",
                      "* p<0.05, ** p<0.01, *** p<0.001.",
                      "IDACI is the 2024-code UTLA average score; higher = more deprived."))

# ------ heatmap ------

pairwise_plot_data <- pairwise %>%
  filter(var_col %in% display_vars) %>%
  mutate(
    var_row   = factor(var_row, levels = rev(all_vars$label)),
    var_col   = factor(var_col, levels = display_vars),
    r_fill    = ifelse(col_row == col_col, NA_real_, r),
    label_txt = ifelse(col_row == col_col, "1", sprintf("%.2f", r))
  )

ggplot(pairwise_plot_data, aes(x = var_col, y = var_row)) +
  geom_tile(aes(fill = r_fill), colour = "white", linewidth = 0.8) +
  geom_text(aes(label = label_txt,
                colour = ifelse(!is.na(r_fill) & abs(r_fill) > 0.4, "white", "black")),
            size = 3) +
  scale_colour_identity() +
  scale_fill_gradient2(low = mhclg_red, mid = "white", high = mhclg_teal,
                       midpoint = 0, limits = c(-1, 1),
                       na.value = "grey85", name = "Pearson r") +
  labs(title = "Correlation matrix - all variables",
       subtitle = "Correlations against headline measures; diagonal in grey. IDACI: higher score = more deprived",
       x = NULL, y = NULL) +
  theme_mhclg() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/heatmap_correlation_matrix_all_variables.png",
       width = 9, height = 7.5, dpi = 300, bg = "white")

# ------ histogram setup ------

excluded_las <- c("City of London", "Isles of Scilly")
BINS         <- 20
MIN_PER_BIN  <- 2

assess_latest_year <- corr_data %>%
  filter(!is.na(assess_rate_per_1000)) %>%
  summarise(y = max(academic_year)) %>% pull(y)

conversion_latest_year <- corr_data %>%
  filter(!is.na(post_assess_approval)) %>%
  summarise(y = max(academic_year)) %>% pull(y)

assess_data <- corr_data %>%
  filter(academic_year == assess_latest_year,
         !is.na(assess_rate_per_1000),
         !la_name %in% excluded_las)

conversion_data <- corr_data %>%
  filter(academic_year == conversion_latest_year,
         !is.na(post_assess_approval),
         !la_name %in% excluded_las)

# ------ outlier flagging via ggplot's own bins ------

base_hist <- function(data, col, bins = BINS) {
  ggplot(data, aes(x = .data[[col]])) +
    geom_histogram(bins = bins)
}

flag_sparse_bins_true <- function(data, col, bins = BINS, min_per_bin = MIN_PER_BIN) {
  built <- ggplot_build(base_hist(data, col, bins))$data[[1]]
  
  data %>%
    rowwise() %>%
    mutate(
      .bar      = which(.data[[col]] >= built$xmin & .data[[col]] <= built$xmax)[1],
      bin_count = built$count[.bar]
    ) %>%
    ungroup() %>%
    mutate(is_outlier = bin_count < min_per_bin)
}

assess_data     <- flag_sparse_bins_true(assess_data,     "assess_rate_per_1000")
conversion_data <- flag_sparse_bins_true(conversion_data, "post_assess_approval")

assess_stats     <- assess_data     %>% summarise(median = median(assess_rate_per_1000), n = n())
conversion_stats <- conversion_data %>% summarise(median = median(post_assess_approval), n = n())

extreme_la_caption <- paste0(paste(excluded_las, collapse = " and "),
                             " excluded due to very small 0-25 population denominators.")

# ------ histogram: assessment rate ------

p_assess_rate <- ggplot(assess_data, aes(x = assess_rate_per_1000)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = BINS) +
  geom_vline(xintercept = assess_stats$median,
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  geom_rug(data = filter(assess_data, is_outlier),
           colour = mhclg_plum, linewidth = 0.8) +
  geom_text(
    data = filter(assess_data, is_outlier),
    aes(x = assess_rate_per_1000, y = 0.5, label = la_name),
    colour = mhclg_plum, size = 4.5, fontface = "bold",
    angle = 90, hjust = 0, vjust = 0.5
  ) +
  labs(
    title    = paste0("EHCP assessment rate by local authority, ",BASE_YEAR , " to ", assess_latest_year),
    subtitle = "Assessments per 1,000 population aged 0-25 (isolated LAs labelled)",
    x = "Assessments per 1,000 population aged 0-25",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA (", round(assess_stats$median, 1),
                     " per 1,000). Labelled = Outliers\n", extreme_la_caption)
  ) +
  theme_mhclg()

print(p_assess_rate)

# ------ histogram: conversion rate ------

p_conversion <- ggplot(conversion_data, aes(x = post_assess_approval)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = BINS) +
  geom_vline(xintercept = conversion_stats$median,
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  geom_rug(data = filter(conversion_data, is_outlier),
           colour = mhclg_plum, linewidth = 0.8) +
  geom_text(
    data = filter(conversion_data, is_outlier),
    aes(x = post_assess_approval, y = 0.5,
        label = paste0(la_name, " (", round(post_assess_approval), "%)")),
    colour = mhclg_plum, size = 4.5, fontface = "bold",
    angle = 90, hjust = 0, vjust = 0.5
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = paste0("% of assessments resulting in an EHCP, ", conversion_latest_year),
    subtitle = "Post-assessment approval rate by local authority (isolated LAs labelled)",
    x = "Assessments resulting in an EHCP (%)",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA (", round(conversion_stats$median, 1),
                     "%). Labelled = Outliers\n", extreme_la_caption)
  ) +
  theme_mhclg()

print(p_conversion)

ggsave("outputs/hist_assessment_rate.png", p_assess_rate,
       width = 10, height = 6.2, dpi = 300, bg = "white")
ggsave("outputs/hist_assessment_conversion.png", p_conversion,
       width = 10, height = 6.2, dpi = 300, bg = "white")

# ------ histogram: request rate and gate-1 approval ------

request_latest_year <- corr_data %>%
  filter(!is.na(request_rate_per_1000)) %>%
  summarise(y = max(academic_year)) %>% pull(y)

gate1_latest_year <- corr_data %>%
  filter(!is.na(gate1_approval_rate)) %>%
  summarise(y = max(academic_year)) %>% pull(y)

request_data <- corr_data %>%
  filter(academic_year == request_latest_year,
         !is.na(request_rate_per_1000),
         !la_name %in% excluded_las)

gate1_data <- corr_data %>%
  filter(academic_year == gate1_latest_year,
         !is.na(gate1_approval_rate),
         !la_name %in% excluded_las)

request_data <- flag_sparse_bins_true(request_data, "request_rate_per_1000")
gate1_data   <- flag_sparse_bins_true(gate1_data,   "gate1_approval_rate")

request_stats <- request_data %>% summarise(median = median(request_rate_per_1000), n = n())
gate1_stats   <- gate1_data   %>% summarise(median = median(gate1_approval_rate),   n = n())

p_request_rate <- ggplot(request_data, aes(x = request_rate_per_1000)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = BINS) +
  geom_vline(xintercept = request_stats$median,
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  geom_rug(data = filter(request_data, is_outlier),
           colour = mhclg_plum, linewidth = 0.8) +
  geom_text(
    data = filter(request_data, is_outlier),
    aes(x = request_rate_per_1000, y = 0.5, label = la_name),
    colour = mhclg_plum, size = 4.5, fontface = "bold",
    angle = 90, hjust = 0, vjust = 0.5
  ) +
  labs(
    title    = paste0("EHCP assessment request rate by local authority, ", request_latest_year),
    subtitle = "Requests per 1,000 population aged 0-25 (isolated LAs labelled)",
    x = "Requests per 1,000 population aged 0-25",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA (", round(request_stats$median, 1),
                     " per 1,000). Labelled = Outliers\n", extreme_la_caption)
  ) +
  theme_mhclg()

print(p_request_rate)

p_gate1 <- ggplot(gate1_data, aes(x = gate1_approval_rate)) +
  geom_histogram(fill = mhclg_teal, colour = "white", alpha = 0.85, bins = BINS) +
  geom_vline(xintercept = gate1_stats$median,
             colour = mhclg_plum, linetype = "dashed", linewidth = 1) +
  geom_rug(data = filter(gate1_data, is_outlier),
           colour = mhclg_plum, linewidth = 0.8) +
  geom_text(
    data = filter(gate1_data, is_outlier),
    aes(x = gate1_approval_rate, y = 0.5,
        label = paste0(la_name, " (", round(gate1_approval_rate), "%)")),
    colour = mhclg_plum, size = 4.5, fontface = "bold",
    angle = 90, hjust = 0, vjust = 0.5
  ) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title    = paste0("% of requests proceeding to assessment, ", gate1_latest_year),
    subtitle = "Requests approved for assessment by local authority (isolated LAs labelled)",
    x = "Requests proceeding to assessment (%)",
    y = "Number of local authorities",
    caption = paste0("Dashed line = median LA (", round(gate1_stats$median, 1),
                     "%). Labelled = Outliers\n", extreme_la_caption)
  ) +
  theme_mhclg()

print(p_gate1)

ggsave("outputs/hist_request_rate.png", p_request_rate,
       width = 10, height = 6.2, dpi = 300, bg = "white")
ggsave("outputs/hist_request_gate1_approval.png", p_gate1,
       width = 10, height = 6.2, dpi = 300, bg = "white")

# ------ outliers to console ------

cat("\n=== REQUEST RATE: LAs ALONE IN THEIR BIN ===\n")
request_data %>% filter(is_outlier) %>%
  arrange(request_rate_per_1000) %>%
  select(la_name, region_name, request_rate_per_1000, bin_count) %>%
  mutate(request_rate_per_1000 = round(request_rate_per_1000, 1)) %>%
  print(n = Inf)

cat("\n=== GATE-1 APPROVAL: LAs ALONE IN THEIR BIN ===\n")
gate1_data %>% filter(is_outlier) %>%
  arrange(gate1_approval_rate) %>%
  select(la_name, region_name, gate1_approval_rate, bin_count) %>%
  mutate(gate1_approval_rate = round(gate1_approval_rate, 1)) %>%
  print(n = Inf)

cat("\n=== ASSESSMENT RATE: LAs ALONE IN THEIR BIN ===\n")
assess_data %>% filter(is_outlier) %>%
  arrange(assess_rate_per_1000) %>%
  select(la_name, region_name, assess_rate_per_1000, bin_count) %>%
  mutate(assess_rate_per_1000 = round(assess_rate_per_1000, 1)) %>%
  print(n = Inf)

cat("\n=== CONVERSION RATE: LAs ALONE IN THEIR BIN ===\n")
conversion_data %>% filter(is_outlier) %>%
  arrange(post_assess_approval) %>%
  select(la_name, region_name, post_assess_approval, bin_count) %>%
  mutate(post_assess_approval = round(post_assess_approval, 1)) %>%
  print(n = Inf)

# ------ slide text: request rate and gate-1 ------

request_full <- request_data %>%
  summarise(median = median(request_rate_per_1000),
            q1 = quantile(request_rate_per_1000, .25),
            q3 = quantile(request_rate_per_1000, .75),
            min = min(request_rate_per_1000),
            max = max(request_rate_per_1000), n = n())

gate1_full <- gate1_data %>%
  summarise(median = median(gate1_approval_rate),
            q1 = quantile(gate1_approval_rate, .25),
            q3 = quantile(gate1_approval_rate, .75),
            min = min(gate1_approval_rate),
            max = max(gate1_approval_rate), n = n())

request_headline <- sprintf(
  paste0(
    "In %s, the median LA received %.1f EHCP assessment requests per 1,000 population aged 0-25, ",
    "ranging from %.1f to %.1f across authorities. Half of LAs sat between %.1f and %.1f."
  ),
  request_latest_year, request_full$median, request_full$min, request_full$max,
  request_full$q1, request_full$q3
)

request_subheading <- sprintf(
  paste0(
    "The median LA approved %.0f%% of requests for assessment in %s, ranging from %.0f%% to %.0f%% ",
    "across authorities, with half of LAs between %.0f%% and %.0f%%. This is the widest gate in the ",
    "process - unlike post-assessment approval, which is near-universal, the decision to assess ",
    "varies substantially between LAs."
  ),
  gate1_full$median, gate1_latest_year, gate1_full$min, gate1_full$max,
  gate1_full$q1, gate1_full$q3
)

cat("\n=== SLIDE TEXT: REQUESTS AND GATE-1 ===\n\n")
cat("HEADLINE:\n",    request_headline,   "\n\n")
cat("SUB-HEADING:\n", request_subheading, "\n\n")

# ------ slide text: assessment rate and conversion ------

assess_full <- assess_data %>%
  summarise(median = median(assess_rate_per_1000),
            q1 = quantile(assess_rate_per_1000, .25),
            q3 = quantile(assess_rate_per_1000, .75),
            min = min(assess_rate_per_1000),
            max = max(assess_rate_per_1000), n = n())

conversion_full <- conversion_data %>%
  summarise(median = median(post_assess_approval),
            q1 = quantile(post_assess_approval, .25),
            q3 = quantile(post_assess_approval, .75),
            min = min(post_assess_approval),
            max = max(post_assess_approval), n = n())

assess_headline <- sprintf(
  paste0(
    "In %s, the median LA carried out %.1f EHCP assessments per 1,000 population aged 0-25, ",
    "with wide variation - from %.1f to %.1f per 1,000. Half of LAs sat between %.1f and %.1f."
  ),
  assess_latest_year, assess_full$median, assess_full$min, assess_full$max,
  assess_full$q1, assess_full$q3
)

assess_subheading <- sprintf(
  paste0(
    "The %% of assessments resulting in an EHCP is uniformly high and tightly clustered: the ",
    "median LA converted %.1f%% of assessments into plans in %s, with half of LAs between %.1f%% ",
    "and %.1f%%. The distribution is strongly left-skewed - once assessed, a plan is nearly always ",
    "issued, so variation between LAs lies in who gets assessed, not what happens afterwards."
  ),
  conversion_full$median, conversion_latest_year, conversion_full$q1, conversion_full$q3
)

cat("\n=== SLIDE TEXT: ASSESSMENTS AND CONVERSION ===\n\n")
cat("HEADLINE:\n",    assess_headline,   "\n\n")
cat("SUB-HEADING:\n", assess_subheading, "\n\n")

# ------ slide 7 callouts ------

approval_latest_year <- corr_data %>%
  filter(!is.na(gate1_approval_rate)) %>%
  summarise(y = max(academic_year)) %>% pull(y)

request_stats_callout <- corr_data %>%
  filter(academic_year == request_latest_year,
         !is.na(request_rate_per_1000),
         !la_name %in% excluded_las) %>%
  summarise(median = median(request_rate_per_1000),
            min = min(request_rate_per_1000),
            max = max(request_rate_per_1000), n = n())

approval_stats <- corr_data %>%
  filter(academic_year == approval_latest_year,
         !is.na(gate1_approval_rate),
         !la_name %in% excluded_las) %>%
  summarise(median = median(gate1_approval_rate),
            min = min(gate1_approval_rate),
            max = max(gate1_approval_rate), n = n())

request_callout <- sprintf(
  "The median LA received %.1f EHCP assessment requests per 1,000 of the 0-25 population.",
  request_stats_callout$median
)

approval_callout <- sprintf(
  "The median LA approved %.0f%% of requests for assessment, ranging from %.0f%% to %.0f%% across authorities.",
  approval_stats$median, approval_stats$min, approval_stats$max
)

cat("\n=== CALL-OUTS ===\n\n")
cat(request_callout,  "\n\n")
cat(approval_callout, "\n\n")

cat("=== DETAIL ===\n")
cat(sprintf("Request rate  (%s, n=%d): median %.1f, range %.1f-%.1f\n",
            request_latest_year, request_stats_callout$n, request_stats_callout$median,
            request_stats_callout$min, request_stats_callout$max))
cat(sprintf("Gate-1 approval (%s, n=%d): median %.1f%%, range %.1f-%.1f%%\n",
            approval_latest_year, approval_stats$n, approval_stats$median,
            approval_stats$min, approval_stats$max))

# ------ growth callout ------

baseline_yr <- BASE_YEAR
latest_yr   <- LATEST_YEAR

ehcp_growth_since_base <- corr_data %>%
  filter(academic_year %in% c(baseline_yr, latest_yr),
         !la_name %in% excluded_las) %>%
  select(la_name, academic_year, ehcplans) %>%
  pivot_wider(names_from = academic_year, values_from = ehcplans) %>%
  filter(!is.na(.data[[baseline_yr]]), !is.na(.data[[latest_yr]]),
         .data[[baseline_yr]] > 0) %>%
  mutate(growth_pct = 100 * (.data[[latest_yr]] / .data[[baseline_yr]] - 1))

g <- ehcp_growth_since_base %>%
  summarise(
    median = median(growth_pct),
    min    = min(growth_pct),
    max    = max(growth_pct),
    q1     = quantile(growth_pct, 0.25),
    q3     = quantile(growth_pct, 0.75),
    n      = n()
  )

growth_callout <- sprintf(
  "Growth since %s was equally uneven - median %.0f%%, ranging from %.0f%% to %.0f%%, with half of LAs between %.0f%% and %.0f%%.",
  baseline_yr, g$median, g$min, g$max, g$q1, g$q3
)

cat("\n", growth_callout, "\n\n")
cat(sprintf("(baseline %s -> %s, n = %d LAs)\n", baseline_yr, latest_yr, g$n))

