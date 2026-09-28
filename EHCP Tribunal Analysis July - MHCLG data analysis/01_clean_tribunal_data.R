
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(tibble)
library(knitr)
library(tidyverse)

# --- SOURCE DATA -------------------------------------------------
# NAMING: these are two SEPARATE DfE datasets (requests vs assessment
# outcomes), not year-partitions of one dataset - names describe the
# content, not the coverage, so they don't go stale at each release.
#
# NOTE: DfE issue a NEW dataset ID with every annual release, so these
# URLs pin a specific release and never update in place. At each new
# release (usually late June), find the new IDs in the data catalogue
# and update the URLs here. The freshness checks below will flag it
# if this is forgotten.
#
# requests_raw:    "Requests for an EHC needs assessment received
#                  during the calendar year" - Reporting year 2026
#                  release (published 25 Jun 2026), covers 2019-2025.
# assessments_raw: "EHC needs assessment outcomes" - covers 2019-2025.
requests_raw    <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/aec1cce1-d932-4bba-b0c6-8645c31f3cbb/csv")
assessments_raw <- read.csv("https://explore-education-statistics.service.gov.uk/data-catalogue/data-set/866331e6-4be2-4242-985a-c00c7ac274b5/csv")

# Freshness checks: fail loudly if either file doesn't reach the year
# we expect, e.g. because a URL is pointing at a superseded release.
stopifnot(
  "requests_raw is stale - repoint URL at latest release" =
    max(as.integer(requests_raw$time_period), na.rm = TRUE) >= 2025,
  "assessments_raw is stale - repoint URL at latest release" =
    max(as.integer(assessments_raw$time_period), na.rm = TRUE) >= 2025
)

# "no data" added for the 2026 release: Leicester returned no
# request/assessment dates in the 2026 collection, shown literally as
# "no data". Barnet (tribunal-after-request figures) and Isle of Wight
# (all 2025 figures) are suppressed via the usual codes.
suppression_codes <- c("x", "z", "c", ":", "low", "..", "n/a", "", "no data")

to_num <- function(col) {
  col_chr <- as.character(col)
  col_chr[col_chr %in% suppression_codes] <- NA
  suppressWarnings(as.numeric(col_chr))
}

# --- DECISION: calendar year -> academic year label -----------------
# Both datasets report time_period as a plain calendar year ("2019"),
# but the EHCP-plans/population side of this project (see
# 02_population_and_ehcp_rate.R) is natively "20xx/xx". T.
to_fy_year <- function(year) {
  year <- as.integer(year)
  paste0(year, "/", substr(year + 1, 3, 4))
}

# -----------------------------------------------------------------
# REQUESTS dataset - LA level only
# -----------------------------------------------------------------
# NOTE: the breakdown label changed wording between releases. The 2026
# release uses "All requests for an EHC needs assessment" (singular).
# If this filter ever returns zero rows, check:
#   requests_raw |> distinct(breakdown_topic, breakdown)
total_breakdown <- "All requests for an EHC needs assessment"

request_cols <- c(
  "requests_received_in_year", "requests_rya", "requests_decided_to_assess",
  "requests_decided_not_to_assess", "requests_decision_not_made", "requests_withdrawn",
  "request_assess_pc", "request_not_assess_pc", "request_outstanding_pc", "request_withdrawn_pc",
  "request_outcome_six_weeks", "request_outcome_over_6_weeks",
  "request_outcome_six_weeks_pc", "request_outcome_over_6_weeks_pc",
  "mediation_related_request", "tribunal_related_request", "tribunal_after_mediation_request"
)

la_requests <- requests_raw %>%
  filter(breakdown_topic == total_breakdown, geographic_level == "Local authority") %>%
  mutate(across(all_of(request_cols), to_num)) %>%
  mutate(
    time_period  = as.integer(time_period),
    academic_year = to_fy_year(time_period),
    # DECISION: old_la_code, not new_la_code, is the stable LA key.
    # new_la_code changes for Buckinghamshire (E10000002 -> E06000060 in
    # 2020) while old_la_code (825) doesn't. Northamptonshire is a genuine
    # split (928 -> 940/941 from 2021), not a recode - old_la_code can't
    # fix that, but it's still the right key for everything else here.
    # Cast to character deliberately: read.csv() infers old_la_code as
    # integer since it's all-digit, but 02_population_and_ehcp_rate.R
    # casts its own old_la_code to character (needed there because it
    # sits alongside new_la_code, which is genuinely alphanumeric).
    # IDs shouldn't be numeric anyway - character avoids exactly this
    # kind of cross-file type mismatch at the 03 join.
    la_id = as.character(old_la_code)
  )

stopifnot(
  "la_requests is empty - breakdown_topic label has likely changed, run:
   requests_raw |> distinct(breakdown_topic)" = nrow(la_requests) > 0
)

# -----------------------------------------------------------------
# ASSESSMENTS dataset - LA level only
# -----------------------------------------------------------------
assessment_total_breakdown <- "All EHC needs assessments"

assessment_cols <- c(
  "assess_in_year", "assess_issued", "assess_issued_pc",
  "assess_not_issued", "assess_not_issued_pc",
  "assess_not_made", "assess_not_made_pc",
  "assess_withdrawn", "assess_withdrawn_pc",
  "outcome_decision_within_time", "outcome_decision_over_time",
  "outcome_decision_within_time_pc", "outcome_decision_over_time_pc",
  "number_assess_mediation", "number_assess_tribunal", "number_asssess_mediation_tribunal",
  "number_other_mediation", "number_other_tribunal", "number_other_mediation_tribunal"
)

la_assessments <- assessments_raw %>%
  filter(breakdown_topic == assessment_total_breakdown, geographic_level == "Local authority") %>%
  mutate(across(all_of(assessment_cols), to_num)) %>%
  mutate(
    time_period  = as.integer(time_period),
    academic_year = to_fy_year(time_period),
    la_id = as.character(old_la_code)
  )

stopifnot(
  "la_assessments is empty - breakdown_topic label has likely changed, run:
   assessments_raw |> distinct(breakdown_topic)" = nrow(la_assessments) > 0
)

# -----------------------------------------------------------------
# JOIN: one row per LA x academic_year with both pipeline stages
# -----------------------------------------------------------------
# COVERAGE (as of the Jun 2026 release): both datasets run 2019-2025
# (calendar) -> academic 2019/20-2025/26. Neither covers 2018/19 at all
# (that year only exists in the EHCP-plans/population side) - so any
# 2018/19 row in the eventual master table will have every tribunal
# column as a genuine NA, not a suppressed value.
# Wight (all 2025 figures suppressed). All arrive as NA via to_num().
la_tribunal <- la_assessments %>%
  select(la_id, la_name, region_name, academic_year,
         assess_in_year, assess_issued, assess_issued_pc, assess_not_issued,
         assess_withdrawn,
         number_assess_mediation, number_assess_tribunal,
         number_other_mediation, number_other_tribunal) %>%
  full_join(
    la_requests %>%
      select(la_id, academic_year, requests_received_in_year,
             requests_decided_to_assess, requests_decided_not_to_assess,
             mediation_related_request, tribunal_related_request,
             tribunal_after_mediation_request),
    by = c("la_id", "academic_year")
  )
# full_join (not left_join) so a year present in only one source still
# shows up as a row - the two sources have matched coverage as of the
# 2026 release, but that hasn't always held and may not in future.

# -----------------------------------------------------------------
# APPROVAL RATES - the three definitions
# -----------------------------------------------------------------
la_tribunal <- la_tribunal %>%
  mutate(
    gate1_approval_rate  = 100 * requests_decided_to_assess / requests_received_in_year,
    post_assess_approval = 100 * assess_issued / assess_in_year,
    # CAVEAT: numerator and denominator are same-year counts, but a plan
    # issued in year Y often stems from a request in Y-1 (the 20-week
    # clock alone crosses year-ends) - deflates the apparent rate in a
    # growing system. Indicative only, not exact.
    end_to_end_approval  = 100 * assess_issued / requests_received_in_year
  )

# -----------------------------------------------------------------
# DISPUTE RATES - refusal (assess-decision) vs content, % of requests
# -----------------------------------------------------------------
la_tribunal <- la_tribunal %>%
  mutate(
    refusal_mediation_rate = 100 * number_assess_mediation / requests_received_in_year,
    refusal_tribunal_rate  = 100 * number_assess_tribunal  / requests_received_in_year,
    content_mediation_rate = 100 * number_other_mediation  / requests_received_in_year,
    content_tribunal_rate  = 100 * number_other_tribunal   / requests_received_in_year,
    # Legacy single-category rates from the requests dataset, kept for
    # comparison against the richer four-way split above
    mediation_rate = 100 * mediation_related_request / requests_received_in_year,
    tribunal_rate  = 100 * tribunal_related_request  / requests_received_in_year,
    # % of mediation cases going to tribunal. NOTE the denominator here
    # is MEDIATIONS, not requests - this is a conversion/escalation rate,
    # the one metric in this project not expressed per request. Measures
    # how often mediation fails to settle the dispute.
    mediation_to_tribunal_pc = 100 * tribunal_after_mediation_request / mediation_related_request
  )


# -----------------------------------------------------------------
# 1.5 x IQR outlier helper, both tails - reused in 04 and 05
# -----------------------------------------------------------------
iqr_outliers <- function(df, value_col, by_year = TRUE) {
  grouping <- if (by_year) "academic_year" else character(0)
  df %>%
    filter(!is.na(.data[[value_col]])) %>%
    group_by(across(all_of(grouping))) %>%
    mutate(
      q1 = quantile(.data[[value_col]], .25), q3 = quantile(.data[[value_col]], .75),
      lo = q1 - 1.5 * (q3 - q1), hi = q3 + 1.5 * (q3 - q1),
      side = case_when(.data[[value_col]] < lo ~ "low", .data[[value_col]] > hi ~ "high", TRUE ~ NA_character_)
    ) %>%
    ungroup() %>%
    filter(!is.na(side)) %>%
    select(any_of("academic_year"), la_name, region_name, side,
           value = all_of(value_col), lower_fence = lo, upper_fence = hi) %>%
    arrange(across(any_of("academic_year")), side, value)
}

