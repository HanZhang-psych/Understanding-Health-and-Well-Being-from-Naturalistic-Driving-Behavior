# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
source('../analysis/funcs.R')

# Demograhics
D = read.csv('../data/demographics.csv') 

# driving data
GPS = read.csv('../data/cleaned_monthly_driving_data.csv') 

# mental/emotional health
MH = read.csv('../data/mental.csv') 

# physical health
PH = read.csv('../data/physical.csv')

# social health
SH = read.csv('../data/social.csv') 

# cognitive health
CH = read.csv('../data/cognitive.csv') 

# use available datalogger intervals as base for merge
outcome_by_interval = GPS %>% distinct(XID, Site, Interval) %>%
            left_join(CH, by = c("XID", "Site", "Interval")) %>%
            left_join(MH, by = c("XID", "Site", "Interval")) %>%
            left_join(PH, by = c("XID", "Site", "Interval")) %>%
            left_join(SH, by = c("XID", "Site", "Interval")) 

# compute subject-level average for outcome variables
outcome_by_subject = outcome_by_interval %>%
  group_by(XID, Site) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-Interval)

# compute subject-level average for driving variables
gps_by_subject = GPS %>%
  group_by(XID, Site) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  select(-Interval, -Year, -Month, -FractionOfMonth, -year_since, -month_since)

# how many sessions are available?
sessions = outcome_by_interval %>% summarize(n_sessions=n(), .by=c(XID, Site)) 
months = GPS %>% summarise(n_months=n(), .by=c(XID, Site))

# Merge all data together
dat = D %>% 
  left_join(sessions, by=c('XID','Site')) %>%
  left_join(months, by=c('XID','Site')) %>%
  left_join(gps_by_subject, by=c('XID','Site')) %>% 
  left_join(outcome_by_subject, by=c('XID','Site')) %>%
  select(XID, n_sessions, n_months, all_of(ivs), all_of(dvs), all_of(demos))
  
# number of unique subjects
n_subj = n_distinct(dat$XID)

# drop na
dat = dat %>% drop_na()

# number of unique subjects after dropping missing data
n_subj_post = n_distinct(dat$XID)

cat(paste0("Number of subjects before dropping missing values: ", n_subj, "\n"))
cat(paste0("Number of subjects after dropping missing values: ", n_subj_post, "\n"))
cat(paste0("Number of subjects dropped due to missing values: ", n_subj - n_subj_post, "\n"))

# Save the aggregated data
write.csv(dat, '../data/subject_level_data_for_analysis.csv', row.names = FALSE)
