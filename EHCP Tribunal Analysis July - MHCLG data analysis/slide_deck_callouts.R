library(tidyverse)

if (!exists("corr_data")) source("09_correlation_matrix.R")

if (!exists("BASE_YEAR"))    BASE_YEAR    <- "2018/19"
if (!exists("LATEST_YEAR"))  LATEST_YEAR  <- "2025/26"
if (!exists("excluded_las")) excluded_las <- c("City of London", "Isles of Scilly")


# ------ helpers ------

short_year <- function(x) paste0("20", str_sub(x, 6, 7))

dist_stats <- function(df, col, year = LATEST_YEAR) {
  df %>%
    filter(academic_year == year, !is.na(.data[[col]]), !la_name %in% excluded_las) %>%
    summarise(
      n      = n(),
      median = median(.data[[col]]),
      min    = min(.data[[col]]),
      max    = max(.data[[col]]),
      q1     = quantile(.data[[col]], .25),
      q3     = quantile(.data[[col]], .75)
    )
}


# ------ stats ------

assess    <- dist_stats(corr_data, "assess_rate_per_1000")
convert   <- dist_stats(corr_data, "post_assess_approval")
gate1     <- dist_stats(corr_data, "gate1_approval_rate")
ehcp_rate <- dist_stats(corr_data, "rate_per_1000")

growth <- corr_data %>%
  filter(!is.na(ehcp_growth_pc), !la_name %in% excluded_las) %>%
  distinct(la_id, ehcp_growth_pc) %>%
  summarise(
    n      = n(),
    median = median(ehcp_growth_pc),
    min    = min(ehcp_growth_pc),
    max    = max(ehcp_growth_pc),
    q1     = quantile(ehcp_growth_pc, .25),
    q3     = quantile(ehcp_growth_pc, .75)
  )

convert_cluster <- corr_data %>%
  filter(academic_year == LATEST_YEAR, !is.na(post_assess_approval),
         !la_name %in% excluded_las) %>%
  summarise(pc_above_95 = 100 * mean(post_assess_approval >= 95)) %>%
  pull(pc_above_95)

gate1_tribunal <- corr_data %>%
  filter(academic_year == LATEST_YEAR,
         !is.na(gate1_approval_rate), !is.na(tribunal_rate)) %>%
  summarise(r = cor(gate1_approval_rate, tribunal_rate)) %>%
  pull(r)


# ------ callout: assessment rate ------

assess_headline <- sprintf(
  "The median LA completed %.1f EHCP assessments per 1,000 of the 0\u201325 population",
  assess$median
)

assess_subheading <- sprintf(
  paste0("Most assessments result in a plan \u2014 %.0f%% of LAs sit at 95%% or above, ",
         "but a long tail of authorities falls as low as %.0f%%"),
  convert_cluster, convert$min
)


# ------ callout: request handling and gatekeeping ------

request_headline <- paste0(
  "How local authorities handle EHCP requests varies sharply \u2013 ",
  "and stricter gatekeeping is linked to more tribunals"
)

request_subheading <- sprintf(
  "The median LA approved %.0f%% of requests for assessment, ranging from %.0f%% to %.0f%% across authorities",
  gate1$median, gate1$min, gate1$max
)


# ------ callout: EHCP rate and growth ------

rate_headline <- sprintf(
  "In %s the median LA EHCP rate was %.1f%% of the 0\u201325 population, ranging from %.1f%% to %.1f%%",
  short_year(LATEST_YEAR), ehcp_rate$median / 10, ehcp_rate$min / 10, ehcp_rate$max / 10
)

rate_subheading <- sprintf(
  paste0("Growth since %s was equally uneven \u2014 median %.0f%%, ranging from %.0f%% to %.0f%%, ",
         "with half of LAs between %.0f%% and %.0f%%"),
  short_year(BASE_YEAR), growth$median, growth$min, growth$max, growth$q1, growth$q3
)


# ------ print ------

print_callout <- function(label, headline, subheading) {
  cat("\n===", label, "===\n\n")
  cat("HEADLINE:\n",    headline,   "\n\n")
  cat("SUB-HEADING:\n", subheading, "\n")
}

print_callout("ASSESSMENT RATE",  assess_headline,  assess_subheading)
print_callout("REQUESTS / GATE-1", request_headline, request_subheading)
print_callout("EHCP RATE / GROWTH", rate_headline,   rate_subheading)

cat("\n=== CHECKS ===\n")
cat(sprintf("Assessment rate (n=%d): median %.1f, range %.1f-%.1f\n",
            assess$n, assess$median, assess$min, assess$max))
cat(sprintf("Conversion      (n=%d): median %.1f%%, %.0f%% of LAs >=95%%\n",
            convert$n, convert$median, convert_cluster))
cat(sprintf("Gate-1 approval (n=%d): median %.0f%%, range %.0f-%.0f%%\n",
            gate1$n, gate1$median, gate1$min, gate1$max))
cat(sprintf("EHCP rate       (n=%d): median %.1f per 1,000 = %.1f%%\n",
            ehcp_rate$n, ehcp_rate$median, ehcp_rate$median / 10))
cat(sprintf("EHCP growth     (n=%d): median %.0f%%, range %.0f-%.0f%%\n",
            growth$n, growth$median, growth$min, growth$max))
cat(sprintf("Gate-1 vs tribunal rate: Pearson r = %+.2f\n", gate1_tribunal))

