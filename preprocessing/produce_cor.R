# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# disable scientific notation
options(scipen = 999)

library(fastDummies)
library(tidyverse)
source('../analysis/funcs.R')

# load data
dat = read.csv('../data/subject_level_data_for_analysis.csv')

# create dummy variables
dat = dat %>% 
  dummy_cols(select_columns = c('Site','EDUCATION','INCOME','GENDER','RACE_ETH','WORK','MARRIAGE'), remove_first_dummy = FALSE)

# rename variables
dat = dat %>% rename(
  EDUCATION_1st_to_8th_grade=EDUCATION_2,
  EDUCATION_9th_to_12th_grade=EDUCATION_3,
  EDUCATION_High_school_graduate=EDUCATION_4,
  EDUCATION_Vocational_technical_business_or_trade_school_beyond_high_school=EDUCATION_5,
  EDUCATION_Some_college_but_no_degree=EDUCATION_6,
  EDUCATION_Associate_degree=EDUCATION_7,
  EDUCATION_Bachelor_degree=EDUCATION_8,
  EDUCATION_Master_professional_or_doctoral_degree=EDUCATION_9,
  INCOME_Less_than_20K=INCOME_1,
  INCOME_20K_to_49999=INCOME_2,
  INCOME_50K_to_79999=INCOME_3,
  INCOME_80K_to_99999=INCOME_4,
  INCOME_100K_or_more=INCOME_5,
  GENDER_Male=GENDER_1,
  GENDER_Female=GENDER_2,
  RACE_White=RACE_ETH_1,
  RACE_Black=RACE_ETH_2,
  RACE_American_Indian=RACE_ETH_3,
  RACE_Asian=RACE_ETH_4,
  RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander=RACE_ETH_5,
  RACE_Other_Non_Hispanic=RACE_ETH_6,
  RACE_Hispanic=RACE_ETH_7,
  WORK_Not_working = WORK_0,
  WORK_Working=WORK_1,
  MARRIAGE_Married=MARRIAGE_1,
  MARRIAGE_Living_with_a_partner=MARRIAGE_2,
  MARRIAGE_Separated=MARRIAGE_3,
  MARRIAGE_Divorced=MARRIAGE_4,
  MARRIAGE_Widowed=MARRIAGE_5,
  MARRIAGE_Never_married=MARRIAGE_6
)

# mean, min, and max for all numeric variables
means = dat %>% select(where(is.numeric)) %>%  report::report_table()
  
# save means
write.csv(means, '../data/shareable/aggregate_descriptive_stats.csv', row.names =F)

# correlation matrix
cor_matrix = cor(dat %>% select(where(is.numeric)), use='pairwise.complete.obs')

# save correlation matrix
write.csv(cor_matrix, '../data/shareable/correlation_matrix.csv', row.names =F)
