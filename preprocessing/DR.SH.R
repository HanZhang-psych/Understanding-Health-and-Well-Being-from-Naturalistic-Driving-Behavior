######## Social Health ######## 
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
SH_raw = read.csv('../../Data Files/CSV/DR.SH VALUES.csv')

SH = SH_raw

############# Cleaning #############
# rename columns to match GPS data
SH = SH %>% rename(
  Site = SITE,
  Interval = INTERVAL,
)

# recode SITE values
SH = SH %>% mutate(Site = case_when(
  Site == 11 ~ 'UCDENVER',
  Site == 12 ~ 'BASSETT',
  Site == 13 ~ 'JHSPH',
  Site == 14 ~ 'UMTRI',
  Site == 15 ~ 'UCSD',
  TRUE ~ NA_character_
))

############# Social Roles #############
sr_cols = c('XID','Site','Interval','SOCIALROLE_T','SH_1A','SH_1B','SH_1C','SH_1D') 
sr = SH[,sr_cols]

# verify possible values
unique(sr$SOCIALROLE_T) %>% sort(na.last = T)
hist(sr$SOCIALROLE_T)

# recode values
sr = sr %>% 
  mutate(SH_1A = case_when(SH_1A %in% c(-1,-2,-3)~NA, .default = SH_1A),
         SH_1B = case_when(SH_1B %in% c(-1,-2,-3)~NA, .default = SH_1B),
         SH_1C = case_when(SH_1C %in% c(-1,-2,-3)~NA, .default = SH_1C),
         SH_1D = case_when(SH_1D %in% c(-1,-2,-3)~NA, .default = SH_1D),
         socialrole = (SH_1A + SH_1B + SH_1C + SH_1D)/4)

# verify possible values
unique(sr$socialrole) %>% sort(na.last = T)
hist(sr$socialrole)

# corr among items
sr %>%
  filter(Interval==0) %>%
  select(SH_1A, SH_1B, SH_1C, SH_1D) %>%
  pairs.panels(.)

# alpha
sr %>%
  filter(Interval==0) %>%
  select(SH_1A, SH_1B, SH_1C, SH_1D) %>% alpha()

# visualize trend
sr %>%
  ggplot(aes(x=Interval, y=socialrole))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10)

############# Informational Support #############
info_cols = c('XID','Site','Interval','INFORMATION_T','SH_2A','SH_2B','SH_2C','SH_2D') 
info = SH[,info_cols]

# verify possible values
unique(info$INFORMATION_T) %>% sort(na.last = T)
hist(info$INFORMATION_T)

# recode values
info = info %>% 
  mutate(SH_2A = case_when(SH_2A %in% c(-1,-2,-3)~NA, .default = SH_2A),
         SH_2B = case_when(SH_2B %in% c(-1,-2,-3)~NA, .default = SH_2B),
         SH_2C = case_when(SH_2C %in% c(-1,-2,-3)~NA, .default = SH_2C),
         SH_2D = case_when(SH_2D %in% c(-1,-2,-3)~NA, .default = SH_2D),
         info = ((6-SH_2A) + (6-SH_2B) + (6-SH_2C) + (6-SH_2D))/4)

# verify possible values
unique(info$info) %>% sort(na.last = T)
hist(info$info)

# corr among items
info %>%
  filter(Interval==0) %>%
  select(SH_2A, SH_2B, SH_2C, SH_2D) %>%
  pairs.panels(.)

# alpha
info %>%
  filter(Interval==0) %>%
  select(SH_2A, SH_2B, SH_2C, SH_2D) %>%
  alpha()

# visualize trend
info %>%
  ggplot(aes(x=Interval, y=info))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10)

############# Emotional Support #############
emo_cols = c('XID','Site','Interval','EMOTIONAL_T','SH_3A','SH_3B','SH_3C','SH_3D') 
emo = SH[,emo_cols]

# verify possible values
unique(emo$EMOTIONAL_T) %>% sort(na.last = T)
hist(emo$EMOTIONAL_T)

# recode values
emo = emo %>% 
  mutate(SH_3A = case_when(SH_3A %in% c(-1,-2,-3)~NA, .default = SH_3A),
         SH_3B = case_when(SH_3B %in% c(-1,-2,-3)~NA, .default = SH_3B),
         SH_3C = case_when(SH_3C %in% c(-1,-2,-3)~NA, .default = SH_3C),
         SH_3D = case_when(SH_3D %in% c(-1,-2,-3)~NA, .default = SH_3D),
         emo = ((6-SH_3A) + (6-SH_3B) + (6-SH_3C) + (6-SH_3D))/4)

# verify possible values
unique(emo$emo) %>% sort(na.last = T)
hist(emo$emo)

# corr among items
emo %>%
  filter(Interval==0) %>%
  select(SH_3A, SH_3B, SH_3C, SH_3D) %>%
  pairs.panels(.)

# alpha
emo %>%
  filter(Interval==0) %>%
  select(SH_3A, SH_3B, SH_3C, SH_3D) %>%
  alpha()

# visualize trend
emo %>%
  ggplot(aes(x=Interval, y=emo))+
  facet_wrap(~Site)+
  stat_summary(fun.data = 'mean_se')

############# Isolation #############
iso_cols = c('XID','Site','Interval','ISOLATION_T','SH_4A','SH_4B','SH_4C','SH_4D') 
iso = SH[,iso_cols]

# verify possible values
unique(iso$ISOLATION_T) %>% sort(na.last = T)
hist(iso$ISOLATION_T)

# recode values
iso = iso %>% 
  mutate(SH_4A = case_when(SH_4A %in% c(-1,-2,-3)~NA, .default = SH_4A),
         SH_4B = case_when(SH_4B %in% c(-1,-2,-3)~NA, .default = SH_4B),
         SH_4C = case_when(SH_4C %in% c(-1,-2,-3)~NA, .default = SH_4C),
         SH_4D = case_when(SH_4D %in% c(-1,-2,-3)~NA, .default = SH_4D),
         iso = (SH_4A + SH_4B + SH_4C + SH_4D)/4)

# verify possible values
unique(iso$iso) %>% sort(na.last = T)
hist(iso$iso)

# corr among items
iso %>%
  filter(Interval==0) %>%
  select(SH_4A, SH_4B, SH_4C, SH_4D) %>%
  pairs.panels(.)

# alpha
iso %>%
  filter(Interval==0) %>%
  select(SH_4A, SH_4B, SH_4C, SH_4D) %>%
  alpha()

# visualize trend
iso %>%
  ggplot(aes(x=Interval, y=iso))+
  facet_wrap(~Site)+
  stat_summary(fun.data = 'mean_se')

############# Instrumental Support #############
ins_cols = c('XID','Site','Interval','INSTRUMENTAL_T','SH_5A','SH_5B','SH_5C','SH_5D') 
ins = SH[,ins_cols]

# verify possible values
unique(ins$INSTRUMENTAL_T) %>% sort(na.last = T)
hist(ins$INSTRUMENTAL_T)

# recode values
ins = ins %>% 
  mutate(SH_5A = case_when(SH_5A %in% c(-1,-2,-3)~NA, .default = SH_5A),
         SH_5B = case_when(SH_5B %in% c(-1,-2,-3)~NA, .default = SH_5B),
         SH_5C = case_when(SH_5C %in% c(-1,-2,-3)~NA, .default = SH_5C),
         SH_5D = case_when(SH_5D %in% c(-1,-2,-3)~NA, .default = SH_5D),
         ins = ((6-SH_5A) + (6-SH_5B) + (6-SH_5C) + (6-SH_5D))/4)

# verify possible values
unique(ins$ins) %>% sort(na.last = T)
hist(ins$ins)

# corr among items
ins %>%
  filter(Interval==0) %>%
  select(SH_5A, SH_5B, SH_5C, SH_5D) %>%
  pairs.panels(.)

# alpha
ins %>%
  filter(Interval==0) %>%
  select(SH_5A, SH_5B, SH_5C, SH_5D) %>%
  alpha()

# visualize trend
ins %>%
  ggplot(aes(x=Interval, y=ins))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10)

############# LIFE SATISFACTION #############
life_cols = c('XID','Site','Interval','LIFESATISFACTION','SH_7A','SH_7B','SH_7C','SH_7D','SH_7E') 
life = SH[,life_cols]

# verify possible values
unique(life$LIFESATISFACTION) %>% sort(na.last = T)
hist(life$LIFESATISFACTION)

# recode values
life = life %>% 
  mutate(SH_7A = case_when(SH_7A %in% c(-1,-2,-3)~NA, .default = SH_7A),
         SH_7B = case_when(SH_7B %in% c(-1,-2,-3)~NA, .default = SH_7B),
         SH_7C = case_when(SH_7C %in% c(-1,-2,-3)~NA, .default = SH_7C),
         SH_7D = case_when(SH_7D %in% c(-1,-2,-3)~NA, .default = SH_7D),
         SH_7E = case_when(SH_7E %in% c(-1,-2,-3)~NA, .default = SH_7E))

# corr among items
life %>%
  filter(Interval==0) %>%
  select(SH_7A, SH_7B, SH_7C, SH_7D, SH_7E) %>%
  pairs.panels(.)

# alpha
life %>%
  filter(Interval==0) %>%
  select(SH_7A, SH_7B, SH_7C, SH_7D, SH_7E) %>%
  alpha()

# visualize trend
life %>%
  ggplot(aes(x=Interval, y=LIFESATISFACTION))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10)

########### Merge and Save Data ###########
# merge all data
processed_data = SH %>% 
  select(XID, Site, Interval) %>% 
  distinct() %>%
  left_join(sr, by = c('XID','Site','Interval')) %>%
  left_join(info, by = c('XID','Site','Interval')) %>%
  left_join(emo, by = c('XID','Site','Interval')) %>%
  left_join(iso, by = c('XID','Site','Interval')) %>%
  left_join(ins, by = c('XID','Site','Interval')) %>%
  left_join(life, by = c('XID','Site','Interval')) %>%
  select(XID, Site, Interval, socialrole, info, emo, iso, ins, LIFESATISFACTION)

write.csv(processed_data, '../data/social.csv', row.names = FALSE)
