# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(tidyverse)
library(table1)
source('funcs.R')

# ---- Load and prepare -------------------------------------------------------
Y1_PATH <- '../data/y1_subject_level.csv'
Y2_PATH <- '../data/y2_subject_level.csv'

y1 <- read.csv(Y1_PATH, stringsAsFactors = FALSE)
y2 <- read.csv(Y2_PATH, stringsAsFactors = FALSE)

y1$Year <- "Year 1"
y2$Year <- "Year 2"

################## Supplemental Analysis ##################
# controlling for MilesPerTrip_n
y1_0 <- fit_lm(y1, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', scale = TRUE)$params %>%
  filter(Parameter=='IV') %>%
  select(Coefficient, CI_low, CI_high, t, p, N)

y1_1 <- fit_lm(y1, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', COV_name = c('MilesPerTrip_n'), scale = TRUE)$params %>%
  filter(Parameter=='IV') %>%
  select(Coefficient, CI_low, CI_high, t, p, N)

y2_0 <- fit_lm(y2, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', scale = TRUE)$params %>%
  filter(Parameter=='IV') %>%
  select(Coefficient, CI_low, CI_high, t, p, N)

y2_1 <- fit_lm(y2, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', COV_name = c('MilesPerTrip_n'), scale = TRUE)$params %>%
  filter(Parameter=='IV') %>%
  select(Coefficient, CI_low, CI_high, t, p, N)

# combine results
results <- rbind(
  cbind(Year = 'Year 1', Model = 'Unadjusted', y1_0),
  cbind(Year = 'Year 1', Model = 'Adjusted for MilesPerTrip_n', y1_1),
  cbind(Year = 'Year 2', Model = 'Unadjusted', y2_0),
  cbind(Year = 'Year 2', Model = 'Adjusted for MilesPerTrip_n', y2_1)
)

results
