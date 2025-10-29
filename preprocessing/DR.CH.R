######## Cognitive Health ######## 
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
CH_raw = read.csv('../../Data Files/CSV/DR.CH VALUES.csv')

CH = CH_raw

############# Cleaning #############
# rename columns to match GPS data
CH = CH %>% rename(
  Site = SITE,
  Interval = INTERVAL,
)

# recode SITE values
CH = CH %>% mutate(Site = case_when(
  Site == 11 ~ 'UCDENVER',
  Site == 12 ~ 'BASSETT',
  Site == 13 ~ 'JHSPH',
  Site == 14 ~ 'UMTRI',
  Site == 15 ~ 'UCSD',
  TRUE ~ NA_character_
))

############# TICS #############
tics_cols = c('XID','Site','Interval','TICS') 
tics = CH[,tics_cols]

# verify possible values
unique(tics$TICS) %>% sort(na.last = T)
hist(tics$TICS)

############# PROMIS SF v1.0-Applied Cognition-General Concerns #############
gc_cols = c('XID','Site','Interval','GENERAL_T', 'CH_2A','CH_2B','CH_2C','CH_2D') 
gc = CH[,gc_cols]

# verify possible values
unique(gc$GENERAL_T) %>% sort(na.last = T)
hist(gc$GENERAL_T)

# recode values
gc = gc %>% mutate(CH_2A = case_when(CH_2A %in% c(-1,-2,-3) ~ NA, .default = CH_2A),
                   CH_2B = case_when(CH_2B %in% c(-1,-2,-3) ~ NA, .default = CH_2B),
                   CH_2C = case_when(CH_2C %in% c(-1,-2,-3) ~ NA, .default = CH_2C),
                   CH_2D = case_when(CH_2D %in% c(-1,-2,-3) ~ NA, .default = CH_2D),
                   cog = (CH_2A + CH_2B + CH_2C + CH_2D)/4)

gc %>% filter(Interval==0) %>% select(CH_2A, CH_2B, CH_2C, CH_2D) %>% pairs.panels(.)

# alpha
gc %>%
  filter(Interval==0) %>%
  select(CH_2A, CH_2B, CH_2C, CH_2D) %>% alpha()

# visualize trend
gc %>%
  ggplot(aes(x=Interval, y=GENERAL_T))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10, point_interval='mean_qi')

# save data
processed_data = CH %>% 
  select(XID, Site, Interval) %>%
  distinct() %>%
  left_join(gc, by=c('XID','Site','Interval')) %>%
  select(XID, Site, Interval, cog) 


write.csv(processed_data, '../data/cognitive.csv', row.names = F)

