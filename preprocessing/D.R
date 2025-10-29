######## Demographics ######## 
# Clear environment
rm(list = ls())

# load libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggdist)
library(psych)
library(easystats)
library(lmerTest)
library(ggcorrplot)

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data from csv
D_raw = read.csv('../../Data Files/CSV/DR.D VALUES.csv') 
Age = read.csv('../../Data Files/CSV/Age_Calculated_from_VISITDate.csv') %>% 
  filter(INTERVAL==0) %>% select(XID, Age)

D = D_raw

############# Cleaning #############
# rename columns to match GPS data
D = D %>% rename(
  Site = SITE,
  Interval = INTERVAL,
)

# recode SITE values
D = D %>% mutate(Site = case_when(
  Site == 11 ~ 'UCDENVER',
  Site == 12 ~ 'BASSETT',
  Site == 13 ~ 'JHSPH',
  Site == 14 ~ 'UMTRI',
  Site == 15 ~ 'UCSD',
  TRUE ~ NA_character_
))

############# Demographic Variables #############
demo_cols = c('XID','Site','Interval', 'GENDER','RACE_ETH','EDUCATION','INCOME','HOMETYPE','WORK','MARRIAGE','VOLUNTEER') 
D = D %>% select(all_of(demo_cols))

### Gender ###
# verify possible values
unique(D$GENDER) %>% sort(na.last = T)
table(D$GENDER)

### Race ###
# verify possible values
unique(D$RACE_ETH) %>% sort(na.last = T)

# recode values
D = D %>% mutate(RACE_ETH = case_when(RACE_ETH == 8 ~ NA, .default = RACE_ETH))

# verify possible values
unique(D$RACE_ETH) %>% sort(na.last = T)

### Education ###
# verify possible values
unique(D$EDUCATION) %>% sort(na.last = T)

### INCOME ###
# verify possible values
unique(D$INCOME) %>% sort(na.last = T)

# recode values
D = D %>% mutate(INCOME = case_when(INCOME %in% c(-1, -2, -3) ~ NA, .default = INCOME))

# verify possible values
unique(D$INCOME) %>% sort(na.last = T)

### Home Type ###
# verify possible values
unique(D$HOMETYPE) %>% sort(na.last = T)

### Work Status ###
# verify possible values
unique(D$WORK) %>% sort(na.last = T)

# recode values
D = D %>% mutate(WORK = case_when(WORK %in% c(-1, -2, -3) ~ NA, .default = WORK))

# verify possible values
unique(D$WORK) %>% sort(na.last = T)

### Marital Status ###
# verify possible values
unique(D$MARRIAGE) %>% sort(na.last = T)

# recode values
D = D %>% mutate(MARRIAGE = case_when(MARRIAGE %in% c(-1, -2, -3) ~ NA, .default = MARRIAGE))

# verify possible values
unique(D$MARRIAGE) %>% sort(na.last = T)


### Volunteer Status ###
# verify possible values
unique(D$VOLUNTEER) %>% sort(na.last = T)

# recode values
D = D %>% mutate(VOLUNTEER = case_when(VOLUNTEER %in% c(-1, -2, -3) ~ NA, .default = VOLUNTEER))

# verify possible values
unique(D$VOLUNTEER) %>% sort(na.last = T)

############# Checking for Status Change #############

# check unique values per XID
status_change = D %>% 
  filter(Interval %in% c(0,1,2 )) %>%
  select(-Interval, -Site) %>%
  group_by(XID) %>%
  summarise(across(everything(), n_distinct)) %>%
  ungroup() %>%
  filter(if_any(-XID, ~ . > 1))

# histogram of number of status changes
status_change %>%
  pivot_longer(-XID, names_to = "variable", values_to = "n_unique") %>%
  ggplot(aes(x = n_unique)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution of Number of Unique Statuses per Demographic Variable", 
       x = "Number of Unique Statuses", y = "Frequency") # volunteer and work have the most changes

# Merge Age with D
D = D %>% 
  filter(Interval==0) %>%
  select(-Interval, -HOMETYPE, -VOLUNTEER) %>%
  left_join(Age, by='XID')

# save data
write.csv(D, '../data/demographics.csv', row.names = F)

