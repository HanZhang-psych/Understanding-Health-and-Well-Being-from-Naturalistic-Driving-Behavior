###############################################################################
# aggregate_data_yearly.R
#
# Build subject-level data sets for the exploratory year 1 -> confirmatory year 2 analysis flow

# Pairing (prospective):
#   Year 1 (exploratory):    driving from Interval 0  ->  health at Interval 1
#   Year 2 (confirmatory): driving from Interval 1  ->  health at Interval 2
#
# Notes on preprocessing:
#   * The cleaned monthly driving data already had outlier flagging applied
#     within Site x Interval, so monthly outliers in Interval 0 (Year 1) are
#     defined within Year 1 only, and likewise for Interval 1 (Year 2).
#     We therefore do not re-do per-month outlier removal here; we only
#     restrict to the relevant Interval, then aggregate to the subject level.
#   * The driving variable set, demographic covariate set, and case-complete
#     drop rule are the same as in the original pipeline, so the only thing
#     that changes here is the temporal slicing.
#
# Outputs:
#   ../data/y1_subject_level.csv
#   ../data/y2_subject_level.csv
###############################################################################

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
source('funcs.R')

# ---- Load source files -------------------------------------------------------
D = read.csv('../data/demographics.csv')
GPS = read.csv('../data/cleaned_monthly_driving_data.csv')
MH = read.csv('../data/mental.csv')
PH = read.csv('../data/physical.csv')
SH = read.csv('../data/social.csv')
CH = read.csv('../data/cognitive.csv')

# ---- Combine all health files into a single long format ----------------------
health_long = CH %>%
  left_join(MH, by = c("XID", "Site", "Interval")) %>%
  left_join(PH, by = c("XID", "Site", "Interval")) %>%
  left_join(SH, by = c("XID", "Site", "Interval"))


# ---- Year-specific aggregation function --------------------------------------
# drv_interval = which driving Interval to use for driving aggregates
# health_interval = which subsequent health Interval to use for outcomes
# year_label = "Y1" or "Y2"
build_year_dataset = function(drv_interval, health_interval, year_label) {

  # filter driving data to the selected interval and aggregate to subject
  gps_by_subject = GPS %>%
    filter(Interval == drv_interval) %>%
    group_by(XID, Site) %>%
    summarise(
      n_months = n(),
      across(all_of(ivs), ~ mean(.x, na.rm = TRUE)),
      .groups = 'drop'
    )

  # pull the subsequent-year health outcomes
  outcomes_yr = health_long %>%
    filter(Interval == health_interval) %>%
    select(XID, Site, all_of(dvs))

  # merge with demographics (which are baseline)
  dat = D %>%
    left_join(gps_by_subject, by = c('XID', 'Site')) %>%
    left_join(outcomes_yr, by = c('XID', 'Site')) %>%
    select(XID, n_months, all_of(ivs), all_of(dvs), all_of(demos))

  n_subj_raw = n_distinct(dat$XID)

  # all subjects are kept. subject deletion occurs at analysis level.
  n_subj_post = n_distinct(dat$XID)

  cat(sprintf("[%s] driving Interval = %d, health Interval = %d\n",
              year_label, drv_interval, health_interval))
  cat(sprintf("    subjects (any data):                       %d\n", n_subj_raw))
  cat(sprintf("    subjects after case-complete drop:         %d\n", n_subj_post))
  cat(sprintf("    subjects dropped due to missing values:    %d\n\n",
              n_subj_raw - n_subj_post))

  attr(dat, "year_label") = year_label
  dat
}

# ---- Build Year 1 and Year 2 datasets ----------------------------------------
y1 = build_year_dataset(drv_interval = 0, health_interval = 1, year_label = "Y1")
y2 = build_year_dataset(drv_interval = 1, health_interval = 2, year_label = "Y2")

# how many subjects appear in both Y1 and Y2?
n_both = length(intersect(y1$XID, y2$XID))
cat(sprintf("Subjects with complete data in BOTH Y1 and Y2: %d\n", n_both))

# ---- Save --------------------------------------------------------------------
write.csv(y1, '../data/y1_subject_level.csv', row.names = FALSE)
write.csv(y2, '../data/y2_subject_level.csv', row.names = FALSE)
