# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# disable scientific notation
options(scipen = 999)

# load libraries
library(tidyverse)
library(easystats)
library(lavaan)
library(fastDummies)
source('funcs.R')

# whether to use correlation matrix or raw data
USE_RAW = FALSE

################## Prepare Data ################## 
sigs = read.csv('significant_bivariate_relationships.csv')

if (USE_RAW){
  # load data
  dat = read.csv('../data/subject_level_data_for_analysis.csv')
  
  # convert dtypes
  dat = dat %>%
    mutate(XID = as.factor(XID),
           Site = as.factor(Site),
           Age = as.numeric(Age),
           GENDER = as.factor(GENDER),
           RACE_ETH = as.factor(RACE_ETH),
           EDUCATION = as.numeric(EDUCATION),
           INCOME = as.numeric(INCOME),
           WORK = as.factor(WORK),
           MARRIAGE = as.factor(MARRIAGE)) %>%
    mutate(across(all_of(ivs), as.numeric)) %>%
    mutate(across(all_of(dvs), as.numeric)) 
  
  # scale all numeric variables
  dat = dat %>%
    mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
  
  # lavaan needs manually adding dummy variables for factors
  dat = dummy_cols(dat, select_columns = c('Site','GENDER','RACE_ETH','WORK','MARRIAGE'), remove_first_dummy = TRUE) 
  
  # rename variables
  dat = dat %>% rename(
    GENDER_Female=GENDER_2,
    RACE_Black=RACE_ETH_2,
    RACE_American_Indian=RACE_ETH_3,
    RACE_Asian=RACE_ETH_4,
    RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander=RACE_ETH_5,
    RACE_Other_Non_Hispanic=RACE_ETH_6,
    RACE_Hispanic=RACE_ETH_7,
    WORK_Working=WORK_1,
    MARRIAGE_Living_with_a_partner=MARRIAGE_2,
    MARRIAGE_Separated=MARRIAGE_3,
    MARRIAGE_Divorced=MARRIAGE_4,
    MARRIAGE_Widowed=MARRIAGE_5,
    MARRIAGE_Never_married=MARRIAGE_6
  )
} else {
  cormat_all = read.csv('../data/shareable/correlation_matrix.csv') %>% as.matrix()
  rownames(cormat_all) = colnames(cormat_all) 
}

######### Average_speed #########
# finding the largest association
sigs %>% filter(IV == "Average_speed") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.Average_speed = '
  cog ~ b_cog*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*Average_speed + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.Average_speed = sem(model.Average_speed, data = dat, estimator='ML')
} else {
  fit.Average_speed = sem(model.Average_speed, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.Average_speed, standardized = TRUE)

bivariate_Average_speed = parameters(fit.Average_speed) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Average_speed')

tests_Average_speed = parameters(fit.Average_speed) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Average_speed') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### Days Driving #########
# finding the largest association
sigs %>% filter(IV == "DaysDriving") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.DaysDriving = '
  cog ~ b_cog*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*DaysDriving + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.DaysDriving = sem(model.DaysDriving, data = dat, estimator='ML')
} else {
  fit.DaysDriving = sem(model.DaysDriving, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.DaysDriving, standardized = TRUE)

bivariate_DaysDriving = parameters(fit.DaysDriving) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='DaysDriving')

tests_DaysDriving = parameters(fit.DaysDriving) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='DaysDriving') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### MinutesPerTrip_n #########
# finding the largest association
sigs %>% filter(IV == "MinutesPerTrip_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.MinutesPerTrip_n = '
  cog ~ b_cog*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*MinutesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'
if (USE_RAW){
  fit.MinutesPerTrip_n = sem(model.MinutesPerTrip_n, data = dat, estimator='ML')
} else {
  fit.MinutesPerTrip_n = sem(model.MinutesPerTrip_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.MinutesPerTrip_n, standardized = TRUE)

bivariate_MinutesPerTrip_n = parameters(fit.MinutesPerTrip_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MinutesPerTrip_n')

tests_MinutesPerTrip_n = parameters(fit.MinutesPerTrip_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MinutesPerTrip_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### MinutesPerChain_n #########
# finding the largest association
sigs %>% filter(IV == "MinutesPerChain_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.MinutesPerChain_n = '
  cog ~ b_cog*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*MinutesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.MinutesPerChain_n = sem(model.MinutesPerChain_n, data = dat, estimator='ML')
} else {
  fit.MinutesPerChain_n = sem(model.MinutesPerChain_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.MinutesPerChain_n, standardized = TRUE)

bivariate_MinutesPerChain_n = parameters(fit.MinutesPerChain_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MinutesPerChain_n')

tests_MinutesPerChain_n = parameters(fit.MinutesPerChain_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MinutesPerChain_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### PercentTripsAMPeak_n #########
# finding the largest association
sigs %>% filter(IV == "PercentTripsAMPeak_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.PercentTripsAMPeak_n = '
  cog ~ b_cog*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*PercentTripsAMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.PercentTripsAMPeak_n = sem(model.PercentTripsAMPeak_n, data = dat, estimator='ML')
} else {
  fit.PercentTripsAMPeak_n = sem(model.PercentTripsAMPeak_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.PercentTripsAMPeak_n, standardized = TRUE)

bivariate_PercentTripsAMPeak_n = parameters(fit.PercentTripsAMPeak_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='PercentTripsAMPeak_n')

tests_PercentTripsAMPeak_n = parameters(fit.PercentTripsAMPeak_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='PercentTripsAMPeak_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))


######### TripChains #########
# finding the largest association
sigs %>% filter(IV == "TripChains") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.TripChains = '
  cog ~ b_cog*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*TripChains + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.TripChains = sem(model.TripChains, data = dat, estimator='ML')
} else {
  fit.TripChains = sem(model.TripChains, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.TripChains, standardized = TRUE)

bivariate_TripChains = parameters(fit.TripChains) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripChains')

tests_TripChains = parameters(fit.TripChains) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripChains') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### Trips #########
# finding the largest association
sigs %>% filter(IV == "Trips") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.Trips = '
  cog ~ b_cog*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*Trips + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.Trips = sem(model.Trips, data = dat, estimator='ML')
} else {
  fit.Trips = sem(model.Trips, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.Trips, standardized = TRUE)

bivariate_Trips = parameters(fit.Trips) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Trips')

tests_Trips = parameters(fit.Trips) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Trips') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### TripsAMPeak #########
# finding the largest association
sigs %>% filter(IV == "TripsAMPeak") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.TripsAMPeak = '
  cog ~ b_cog*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*TripsAMPeak + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'

if (USE_RAW){
  fit.TripsAMPeak = sem(model.TripsAMPeak, data = dat, estimator='ML')
} else {
  fit.TripsAMPeak = sem(model.TripsAMPeak, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.TripsAMPeak, standardized = TRUE)

bivariate_TripsAMPeak = parameters(fit.TripsAMPeak) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripsAMPeak')

tests_TripsAMPeak = parameters(fit.TripsAMPeak) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripsAMPeak') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))


######### TripsLt15Miles #########
# finding the largest association
sigs %>% filter(IV == "TripsLt15Miles") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.TripsLt15Miles = '
  cog ~ b_cog*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*TripsLt15Miles+ Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*TripsLt15Miles + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'
if (USE_RAW){
  fit.TripsLt15Miles = sem(model.TripsLt15Miles, data = dat, estimator = "ML")
  } else {
  fit.TripsLt15Miles = sem(model.TripsLt15Miles, sample.cov = cormat_all, sample.nobs = 2658, estimator = "ML")
}
summary(fit.TripsLt15Miles, standardized=T)

bivariate_TripsLt15Miles = parameters(fit.TripsLt15Miles) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripsLt15Miles')

tests_TripsLt15Miles = parameters(fit.TripsLt15Miles) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripsLt15Miles') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### TripMinutes_n #########
# finding the largest association
sigs %>% filter(IV == "TripMinutes_n") %>% select(IV, DV, beta) %>% arrange(beta)

model.TripMinutes_n = '
  cog ~ b_cog*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*TripMinutes_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  iso_vs_cog := b_iso - b_cog
  iso_vs_phys := b_iso - b_phys
  iso_vs_fati := b_iso - b_fati
  iso_vs_anx := b_iso - b_anx
  iso_vs_dep := b_iso - b_dep
  iso_vs_ang := b_iso - b_ang
  iso_vs_socialrole := b_iso - b_socialrole
  iso_vs_info := b_iso - b_info
  iso_vs_emo := b_iso - b_emo
  iso_vs_ins := b_iso - b_ins
'
if (USE_RAW){
  fit.TripMinutes_n = sem(model.TripMinutes_n, data = dat, estimator='ML')
} else {
  fit.TripMinutes_n = sem(model.TripMinutes_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.TripMinutes_n, standardized = TRUE)

bivariate_TripMinutes_n = parameters(fit.TripMinutes_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripMinutes_n')

tests_TripMinutes_n = parameters(fit.TripMinutes_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='TripMinutes_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))


######### LeftTurnCount #########
# finding the largest association
sigs %>% filter(IV == "LeftTurnCount") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.LeftTurnCount = '
  cog ~ b_cog*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*LeftTurnCount + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  phys_vs_cog := b_phys - b_cog
  phys_vs_fati := b_phys - b_fati
  phys_vs_anx := b_phys - b_anx
  phys_vs_dep := b_phys - b_dep
  phys_vs_ang := b_phys - b_ang
  phys_vs_socialrole := b_phys - b_socialrole
  phys_vs_info := b_phys - b_info
  phys_vs_ins := b_phys - b_ins
  phys_vs_emo := b_phys - b_emo
  phys_vs_iso := b_phys - b_iso
'
if (USE_RAW){
  fit.LeftTurnCount = sem(model.LeftTurnCount, data = dat, estimator='ML')
} else {
  fit.LeftTurnCount = sem(model.LeftTurnCount, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.LeftTurnCount, standardized = TRUE)

bivariate_LeftTurnCount = parameters(fit.LeftTurnCount) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='LeftTurnCount')

tests_LeftTurnCount = parameters(fit.LeftTurnCount) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='LeftTurnCount') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### Miles_n #########
# finding the largest association
sigs %>% filter(IV == "Miles_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.Miles_n = '
  cog ~ b_cog*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*Miles_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  iso_vs_cog := b_iso - b_cog
  iso_vs_phys := b_iso - b_phys
  iso_vs_fati := b_iso - b_fati
  iso_vs_anx := b_iso - b_anx
  iso_vs_dep := b_iso - b_dep
  iso_vs_ang := b_iso - b_ang
  iso_vs_socialrole := b_iso - b_socialrole
  iso_vs_info := b_iso - b_info
  iso_vs_emo := b_iso - b_emo
  iso_vs_ins := b_iso - b_ins
'
if (USE_RAW){
  fit.Miles_n = sem(model.Miles_n, data = dat, estimator='ML')
} else {
  fit.Miles_n = sem(model.Miles_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.Miles_n, standardized = TRUE)

bivariate_Miles_n = parameters(fit.Miles_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Miles_n')

tests_Miles_n = parameters(fit.Miles_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='Miles_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))


######### PercentTripsPMPeak_n #########
# finding the largest association
sigs %>% filter(IV == "PercentTripsPMPeak_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.PercentTripsPMPeak_n = '
  cog ~ b_cog*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*PercentTripsPMPeak_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  socialrole_vs_cog := b_socialrole - b_cog
  socialrole_vs_phys := b_socialrole - b_phys
  socialrole_vs_fati := b_socialrole - b_fati
  socialrole_vs_anx := b_socialrole - b_anx
  socialrole_vs_dep := b_socialrole - b_dep
  socialrole_vs_ang := b_socialrole - b_ang
  socialrole_vs_iso := b_socialrole - b_iso
  socialrole_vs_info := b_socialrole - b_info
  socialrole_vs_emo := b_socialrole - b_emo
  socialrole_vs_ins := b_socialrole - b_ins
'
if (USE_RAW){
  fit.PercentTripsPMPeak_n = sem(model.PercentTripsPMPeak_n, data = dat, estimator='ML')
} else {
  fit.PercentTripsPMPeak_n = sem(model.PercentTripsPMPeak_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.PercentTripsPMPeak_n, standardized = TRUE)

bivariate_PercentTripsPMPeak_n = parameters(fit.PercentTripsPMPeak_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='PercentTripsPMPeak_n')

tests_PercentTripsPMPeak_n = parameters(fit.PercentTripsPMPeak_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='PercentTripsPMPeak_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

######### MilesPerChain_n #########
# finding the largest association
sigs %>% filter(IV == "MilesPerChain_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.MilesPerChain_n = '
  cog ~ b_cog*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*MilesPerChain_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  dep_vs_cog := b_dep - b_cog
  dep_vs_fati := b_dep - b_fati
  dep_vs_anx := b_dep - b_anx
  dep_vs_phys := b_dep - b_phys
  dep_vs_ang := b_dep - b_ang
  dep_vs_socialrole := b_dep - b_socialrole
  dep_vs_info := b_dep - b_info
  dep_vs_ins := b_dep - b_ins
  dep_vs_emo := b_dep - b_emo
  dep_vs_iso := b_dep - b_iso
'
if (USE_RAW){
  fit.MilesPerChain_n = sem(model.MilesPerChain_n, data = dat, estimator='ML')
} else {
  fit.MilesPerChain_n = sem(model.MilesPerChain_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.MilesPerChain_n, standardized = TRUE)

bivariate_MilesPerChain_n = parameters(fit.MilesPerChain_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MilesPerChain_n')

tests_MilesPerChain_n = parameters(fit.MilesPerChain_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MilesPerChain_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))


######### MilesPerTrip_n #########
# finding the largest association
sigs %>% filter(IV == "MilesPerTrip_n") %>% select(IV, DV, beta, p.adj) %>% arrange(beta)

model.MilesPerTrip_n = '
  cog ~ b_cog*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  phys ~ b_phys*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  fati ~ b_fati*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  anx ~ b_anx*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  dep ~ b_dep*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ang ~ b_ang*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  socialrole ~ b_socialrole*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  info ~ b_info*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  emo ~ b_emo*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  iso ~ b_iso*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  ins ~ b_ins*MilesPerTrip_n + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian + RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander + RACE_Other_Non_Hispanic + RACE_Hispanic +  WORK_Working + MARRIAGE_Living_with_a_partner + MARRIAGE_Separated + MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married
  
  # Test equality / differences of effects
  anx_vs_cog := b_anx - b_cog
  anx_vs_fati := b_anx - b_fati
  anx_vs_dep := b_anx - b_dep
  anx_vs_phys := b_anx - b_phys
  anx_vs_ang := b_anx - b_ang
  anx_vs_socialrole := b_anx - b_socialrole
  anx_vs_info := b_anx - b_info
  anx_vs_ins := b_anx - b_ins
  anx_vs_emo := b_anx - b_emo
  anx_vs_iso := b_anx - b_iso
'

if (USE_RAW){
  fit.MilesPerTrip_n = sem(model.MilesPerTrip_n, data = dat, estimator='ML')
} else {
  fit.MilesPerTrip_n = sem(model.MilesPerTrip_n, sample.cov = cormat_all, sample.nobs = 2658, estimator='ML')
}
summary(fit.MilesPerTrip_n, standardized = TRUE)

bivariate_MilesPerTrip_n = parameters(fit.MilesPerTrip_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Regression' & Label != '') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MilesPerTrip_n')

tests_MilesPerTrip_n = parameters(fit.MilesPerTrip_n) %>% 
  as.data.frame() %>% 
  filter(Component=='Defined') %>% 
  select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>% 
  mutate(IV='MilesPerTrip_n') %>%
  mutate(p.adj = p.adjust(p, method = "bonferroni"))

################ Combine Test Results ################ 
# regression paths
bivariate_all = rbind(bivariate_Average_speed, bivariate_DaysDriving, bivariate_LeftTurnCount, bivariate_Miles_n, bivariate_MilesPerChain_n, bivariate_MinutesPerTrip_n,
                      bivariate_MilesPerTrip_n, bivariate_PercentTripsAMPeak_n, bivariate_PercentTripsPMPeak_n, bivariate_TripChains, 
                      bivariate_TripMinutes_n, bivariate_Trips, bivariate_TripsAMPeak, bivariate_TripsLt15Miles, bivariate_MinutesPerChain_n)
bivariate_all = bivariate_all %>% mutate(
  DV = str_split(Label, "_") %>% map_chr(2),
)

# nicer labels
bivariate_all = bivariate_all %>% mutate(IV = recode(IV, !!!driving_var_recode_list),
                                         DV = recode(DV, !!!health_var_recode_list))

# pairwise tests
tests_all = rbind(tests_Average_speed, tests_DaysDriving, tests_LeftTurnCount, tests_Miles_n, tests_MilesPerChain_n, tests_MinutesPerTrip_n,
                  tests_MilesPerTrip_n, tests_PercentTripsAMPeak_n, tests_PercentTripsPMPeak_n, tests_TripChains, 
                  tests_TripMinutes_n, tests_Trips, tests_TripsAMPeak, tests_TripsLt15Miles, tests_MinutesPerChain_n)

# parsing labels
tests_all = tests_all %>% mutate(
  candidate_DV = str_split(Label, "_vs_") %>% map_chr(1),
  against_DV = str_split(Label, "_vs_") %>% map_chr(2),
)

# nicer labels
tests_all = tests_all %>% mutate(IV = recode(IV, !!!driving_var_recode_list),
                                 candidate_DV = recode(candidate_DV, !!!health_var_recode_list),
                                 against_DV = recode(against_DV, !!!health_var_recode_list))


# mark significance
tests_all$sig = ifelse(tests_all$p.adj < 0.05, T, F)

# tidy up and save
tests_all %>% mutate(CI=paste0('[', round(CI_low,3), ", ", round(CI_high,3), "]"),
                     p.adj = round(p.adj, 4),
                     Coefficient = round(Coefficient, 3),
                     z = round(z, 3)) %>%
  filter(sig==T) %>%
  select(IV, candidate_DV, against_DV, Coefficient, CI, z, p.adj) %>%
  write.csv(., "multivariate_tests.csv", row.names = F)

################ Visualization ################
# rank order plot
# compute specificity score
rankorder_df = tests_all %>% 
  summarise(dom = sum(sig)/n(), .by = c(IV, candidate_DV)) %>%
  arrange(reorder(IV, -dom))

rankorder_df$IV = factor(rankorder_df$IV, levels=rankorder_df$IV)

rankorder_plot =  rankorder_df %>%
  #filter(dom!=0) %>%
  ggplot(aes(x=IV, y=dom, fill=candidate_DV)) +
  geom_bar(stat='identity', position=position_dodge(), color='black', linewidth=0.3, alpha=0.8) +
  labs(x=NULL, y='Specificity Score', fill='Predicted Health Measure') +
  scale_fill_manual(values=DV_colors)+
  scale_y_continuous(expand=c(0,0), breaks=seq(0,1,0.1), limits=c(0,1)) +
  theme_modern()+ 
  theme(axis.text.x = element_text(angle=45, hjust=1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        legend.position = 'inside',
        legend.position.inside = c(.8,.82),
        axis.ticks.y = element_line(color = "black", linewidth = 1))
rankorder_plot

ggsave("../figs/specificity_score.png", rankorder_plot, width=8, height=6)


# dominance matrix
candidate_effects = tests_all %>% 
  select(IV, candidate_DV) %>% 
  distinct() %>%
  rename(DV=candidate_DV)

compared_effects = tests_all %>% 
  select(Coefficient, z, p, IV, against_DV, p.adj, sig) %>%
  rename(DV=against_DV) %>%
  filter(sig==T) %>%
  mutate(label = 'stripe')

dominance_matrix = ggplot(data=bivariate_all, aes(IV, DV, fill=Coefficient)) + 
  geom_tile(color='white') +
  geom_tile(data=candidate_effects, aes(x=IV, y=DV), color='white', fill=NA, linewidth = 1, inherit.aes = F) +
  ggpattern::geom_tile_pattern(data = compared_effects,
                               aes(
                                 x=IV, y=DV,
                                 pattern=label), 
                               pattern_color='white',
                               pattern_fill = 'white',
                               pattern_angle = 45,
                               pattern_density = 0.1,
                               pattern_size = 0,
                               pattern_spacing = 0.02,
                               fill=NA) +
  scale_fill_viridis_c(option = "D") +
  scale_y_discrete(expand = c(0,0), limits=rev(unlist(health_var_recode_list)[-1]))+
  scale_x_discrete(expand = c(0, 0), limits=rankorder_df$IV) +
  theme_minimal(base_size = 15) +
  theme(axis.text.x =  element_text(angle = 45,  hjust=1),
        axis.ticks.x = element_line(color = "black", linewidth = 1),
        axis.ticks.y = element_line(color = "black", linewidth = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right') +
  labs(x=NULL, y=NULL, fill='Coefficient') 
dominance_matrix

ggsave("../figs/dominance_matrix.png", dominance_matrix, width=12, height=6)

library(patchwork)
multivariate_combined =  dominance_matrix / free(rankorder_plot) + plot_annotation(tag_levels = 'A')

ggsave("../figs/multivariate_combined.png", multivariate_combined, width=10, height=12, dpi=1200)
