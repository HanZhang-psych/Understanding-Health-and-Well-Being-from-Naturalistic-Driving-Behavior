# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(tidyverse)
library(table1)
source('funcs.R')

# read data
dat = read.csv('../data/subject_level_data_for_analysis.csv')

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

# how many repeated measurements are available?
dat %>% summarise(median=median(n_months), min=min(n_months), max=max(n_months))
dat %>% count(n_sessions) %>% mutate(prop=n/sum(n))

################## Driving Variables ################## 
# histogram of all driving behaviors
plot_df = dat %>%
  select(all_of(ivs)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels= names(driving_var_recode_list))) %>%
  mutate(variable = recode(variable,  !!!driving_var_recode_list)) 
mean_vals = plot_df %>% summarize(mean=mean(value), sd=sd(value), .by = variable)
  
ggplot(plot_df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(data=mean_vals, aes(xintercept = mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol=3) +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank())+
  labs(title = NULL, x = "Value", y = "Frequency")

# save plot
ggsave('../figs/Driving_Histogram.png', width = 10, height =12, bg = 'white')


################## Health Variables ################## 
plot_df = dat %>%
  select(all_of(dvs)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels= names(health_var_recode_list))) %>%
  mutate(variable = recode(variable,  !!!health_var_recode_list)) 
mean_vals = plot_df %>% summarize(mean=mean(value), .by = variable)

health_histogram = 
  ggplot(plot_df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(data=mean_vals, aes(xintercept = mean), color = "red", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol=3) +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank())+
  labs(title = NULL, x = "Value", y = "Frequency")
health_histogram

# save plot
ggsave('../figs/Health_Histogram.png', width = 8, height =8, bg = 'white')
