######## Mental Health ######## 
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
MH_raw = read.csv('../../Data Files/CSV/DR.MH VALUES.csv')

MH = MH_raw

############# Cleaning #############
# rename columns to match GPS data
MH = MH %>% rename(
  Site = SITE,
  Interval = INTERVAL,
)

# recode SITE values
MH = MH %>% mutate(Site = case_when(
  Site == 11 ~ 'UCDENVER',
  Site == 12 ~ 'BASSETT',
  Site == 13 ~ 'JHSPH',
  Site == 14 ~ 'UMTRI',
  Site == 15 ~ 'UCSD',
  TRUE ~ NA_character_
))

############# Depression #############
dep_cols = c('XID','Site','Interval','DEPRESSION_T','MH_1A','MH_1B','MH_1C','MH_1D') 
dep = MH[,dep_cols]

# verify possible values
unique(dep$DEPRESSION_T) %>% sort(na.last = T)
hist(dep$DEPRESSION_T)

# recode values
dep = dep %>% 
  mutate(MH_1A = case_when(MH_1A %in% c(-1,-2,-3)~NA, .default = MH_1A),
         MH_1B = case_when(MH_1B %in% c(-1,-2,-3)~NA, .default = MH_1B),
         MH_1C = case_when(MH_1C %in% c(-1,-2,-3)~NA, .default = MH_1C),
         MH_1D = case_when(MH_1D %in% c(-1,-2,-3)~NA, .default = MH_1D),
         dep = (MH_1A + MH_1B + MH_1C + MH_1D)/4)

# verify possible values
unique(dep$dep) %>% sort(na.last = T)
hist(dep$dep)

# corr among items
dep %>%
  filter(Interval==0) %>%
  select(MH_1A, MH_1B, MH_1C, MH_1D) %>%
  pairs.panels(.)

# alpha
dep %>%
  filter(Interval==0) %>%
  select(MH_1A, MH_1B, MH_1C, MH_1D) %>% alpha()

# visualize trend
dep %>%
  ggplot(aes(x=as.factor(Interval), y=dep))+
  #facet_wrap(~Site)+
  stat_summary(fun.data = 'mean_se')

############# Anxiety #############
anx_cols = c('XID','Site','Interval','ANXIETY_T','MH_2A','MH_2B','MH_2C','MH_2D') 
anx = MH[,anx_cols]

# verify possible values
unique(anx$ANXIETY_T) %>% sort(na.last = T)
hist(anx$ANXIETY_T)

# recode values
anx = anx %>% 
  mutate(MH_2A = case_when(MH_2A %in% c(-1,-2,-3)~NA, .default = MH_2A),
         MH_2B = case_when(MH_2B %in% c(-1,-2,-3)~NA, .default = MH_2B),
         MH_2C = case_when(MH_2C %in% c(-1,-2,-3)~NA, .default = MH_2C),
         MH_2D = case_when(MH_2D %in% c(-1,-2,-3)~NA, .default = MH_2D),
         anx = (MH_2A + MH_2B + MH_2C + MH_2D)/4)

# verify possible values
unique(anx$anx) %>% sort(na.last = T)
hist(anx$anx)

# corr among items
anx %>%
  filter(Interval==0) %>%
  select(MH_2A, MH_2B, MH_2C, MH_2D) %>%
  pairs.panels(.)

# alpha
anx %>% 
  filter(Interval==0) %>%
  select(MH_2A, MH_2B, MH_2C, MH_2D) %>% 
  alpha()

# visualize trend
anx %>%
  ggplot(aes(x=Interval, y=anx))+
  #facet_wrap(~Site)+
  stat_summary(fun.data = 'mean_se')

############# Anger #############
ang_cols = c('XID','Site','Interval','ANGER_T','MH_3A','MH_3B','MH_3C','MH_3D','MH_3E') 
ang = MH[,ang_cols]

# verify possible values
unique(ang$ANGER_T) %>% sort(na.last = T)
hist(ang$ANGER_T)

# recode values
ang = ang %>% 
  mutate(MH_3A = case_when(MH_3A %in% c(-1,-2,-3)~NA, .default = MH_3A),
         MH_3B = case_when(MH_3B %in% c(-1,-2,-3)~NA, .default = MH_3B),
         MH_3C = case_when(MH_3C %in% c(-1,-2,-3)~NA, .default = MH_3C),
         MH_3D = case_when(MH_3D %in% c(-1,-2,-3)~NA, .default = MH_3D),
         MH_3E = case_when(MH_3E %in% c(-1,-2,-3)~NA, .default = MH_3E),
         ang = (MH_3A + MH_3B + MH_3C + MH_3D + MH_3E)/4)

# verify possible values
unique(ang$ang) %>% sort(na.last = T)
hist(ang$ang)

# corr among items
ang %>%
  filter(Interval==0) %>%
  select(MH_3A, MH_3B, MH_3C, MH_3D, MH_3E) %>%
  pairs.panels(.)

# alpha
ang %>% 
  filter(Interval==0) %>%
  select(MH_3A, MH_3B, MH_3C, MH_3D, MH_3E) %>%
  alpha()

# visualize trend
ang %>%
  ggplot(aes(x=Interval, y=ang))+
  #facet_wrap(~Site)+
  stat_summary(fun.data = 'mean_se')

##### Save data #####
processed_data = MH %>%
  select(XID, Site, Interval) %>%
  distinct() %>%
  left_join(anx, by=c('XID','Site','Interval')) %>%
  left_join(dep, by=c('XID','Site','Interval')) %>%
  left_join(ang, by=c('XID','Site','Interval')) %>%
  select(XID, Site, Interval, dep, anx, ang)

write.csv(processed_data, '../data/mental.csv', row.names = FALSE)
