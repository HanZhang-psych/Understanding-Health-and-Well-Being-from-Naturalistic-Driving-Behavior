######## Physical Health ######## 
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
PH_raw = read.csv('../../Data Files/CSV/DR.PH VALUES.csv')

PH = PH_raw

############# Cleaning #############
# rename columns to match GPS data
PH = PH %>% rename(
  Site = SITE,
  Interval = INTERVAL,
)

# recode SITE values
PH = PH %>% mutate(Site = case_when(
  Site == 11 ~ 'UCDENVER',
  Site == 12 ~ 'BASSETT',
  Site == 13 ~ 'JHSPH',
  Site == 14 ~ 'UMTRI',
  Site == 15 ~ 'UCSD',
  TRUE ~ NA_character_
))

############# Physical Function #############
phys_cols = c('XID','Site','Interval','PHYSFUNCTION_T','PH_1A','PH_1B','PH_1C','PH_1D') 
phys = PH[,phys_cols]

# verify possible values
unique(phys$PHYSFUNCTION_T) %>% sort(na.last = T)
hist(phys$PHYSFUNCTION_T)

# recode values
phys = phys %>% 
  mutate(PH_1A = case_when(PH_1A %in% c(-1,-2,-3)~NA, .default = PH_1A),
         PH_1B = case_when(PH_1B %in% c(-1,-2,-3)~NA, .default = PH_1B),
         PH_1C = case_when(PH_1C %in% c(-1,-2,-3)~NA, .default = PH_1C),
         PH_1D = case_when(PH_1D %in% c(-1,-2,-3)~NA, .default = PH_1D),
         phys = (PH_1A + PH_1B + PH_1C + PH_1D)/4)

# verify possible values
unique(phys$phys) %>% sort(na.last = T)
hist(phys$phys)

# corr among items
phys %>%
  filter(Interval==0) %>%
  select(PH_1A, PH_1B, PH_1C, PH_1D) %>%
  pairs.panels(.)

# alpha
phys %>%
  filter(Interval==0) %>%
  select(PH_1A, PH_1B, PH_1C, PH_1D) %>% 
  alpha()

# visualize trend
phys %>%
  ggplot(aes(x=Interval, y=phys))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10, point_Interval='mean_qi')

############# Fatigue #############
fati_cols = c('XID','Site','Interval','FATIGUE_T','PH_4A','PH_4B','PH_4C','PH_4D') 
fati = PH[,fati_cols]

# verify possible values
unique(fati$FATIGUE_T) %>% sort(na.last = T)
hist(fati$FATIGUE_T)

# recode values
fati = fati %>% 
  mutate(PH_4A = case_when(PH_4A %in% c(-1,-2,-3)~NA, .default = PH_4A),
         PH_4B = case_when(PH_4B %in% c(-1,-2,-3)~NA, .default = PH_4B),
         PH_4C = case_when(PH_4C %in% c(-1,-2,-3)~NA, .default = PH_4C),
         PH_4D = case_when(PH_4D %in% c(-1,-2,-3)~NA, .default = PH_4D),
         fati = (PH_4A + PH_4B + PH_4C + PH_4D)/4)

# verify possible values
unique(fati$fati) %>% sort(na.last = T)
hist(fati$fati)

# corr among items
fati %>%
  filter(Interval==0) %>%
  select(PH_4A, PH_4B, PH_4C, PH_4D) %>%
  pairs.panels(.)

# alpha
fati %>%
  filter(Interval==0) %>%
  select(PH_4A, PH_4B, PH_4C, PH_4D) %>%
  alpha()

# visualize trend
fati %>%
  ggplot(aes(x=Interval, y=fati))+
  facet_wrap(~Site)+
  stat_histinterval(breaks=10, point_Interval='mean_qi')

#### Merge and Save Data ####
processed_data = PH %>%
  select(XID, Site, Interval) %>%
  distinct() %>%
  left_join(phys, by = c("XID","Site","Interval")) %>%
  left_join(fati, by = c("XID","Site","Interval")) %>%
  select(XID, Site, Interval, phys, fati)

# save data
write.csv(processed_data, '../data/physical.csv', row.names = F)
