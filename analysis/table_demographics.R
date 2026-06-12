# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(tidyverse)
library(table1)
source('funcs.R')

# read data
# Table 2 reports the FULL RECRUITED sample (N = 2,990).
# Demographics are time-invariant here, so the Year-1 file is used; the Year-2
# file yields identical demographic counts. No listwise filtering is applied,
# because of analysis-specific (not global) missing-data handling.
dat = read.csv('../data/y1_subject_level.csv')

################## Demographics ################## 
# convert dtypes
dat = dat %>%
  rename(
    AGE = Age,
    RACE = RACE_ETH,
    `HOUSEHOLD INCOME` = INCOME) %>%
  mutate(XID = as.factor(XID),
         SITE = as.factor(Site),
         AGE = as.numeric(AGE),
         GENDER = as.factor(GENDER),
         RACE = as.factor(RACE),
         EDUCATION = as.factor(EDUCATION), # treating as factor here to show each category
         `HOUSEHOLD INCOME` = as.factor(`HOUSEHOLD INCOME`), # treating as factor here to show each category
         WORK = as.factor(WORK),
         MARRIAGE = as.factor(MARRIAGE)) 

# assign labels to factors
levels(dat$SITE) = c('Cooperstown, New York', 'Baltimore, Maryland', 'Denver, Colorado', 'La Jolla, California', 'Ann Arbor, Michigan')
levels(dat$GENDER) = c('Male','Female')
levels(dat$RACE) = c('White, Non-Hispanic', 'Black, Non-Hispanic','American Indian','Asian','Alaska Native, Native Hawaiian, Pacific Islander','Other, Non-Hispanic','Hispanic')
levels(dat$EDUCATION) = c("1st-8th grade",'9th-12th grade (no diploma)', 'High school graduate (high school diploma or equivalent)','Vocational, technical, business, or trade school (beyond high school level)','Some college but no degree','Associate degree','Bachelor degree','Master, professional, or doctoral degree')
levels(dat$`HOUSEHOLD INCOME`) = c('Less than $20,000', '$20,000 to $49,999', '$50,000 to $79,999', '$80,000 to $99,999', '$100,000 or more')
levels(dat$WORK) = c('No', 'Yes')
levels(dat$MARRIAGE) = c('Married', 'Living with a partner', 'Separated', 'Divorced', 'Widowed', 'Never married')

table1(~ SITE + AGE + GENDER + RACE + EDUCATION + `HOUSEHOLD INCOME` + WORK + MARRIAGE, data=dat, overall='Number (%) of Participants', render.continuous=c(.="Mean (SD)"))
