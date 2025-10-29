# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# disable scientific notation
options(scipen = 999)

# load libraries
library(tidyverse)
library(easystats)
source('funcs.R')

# whether to use correlation matrix or raw data
USE_RAW = FALSE

################## Prepare Data ################## 
if (USE_RAW){
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
} else {
  cormat_all = read.csv('../data/shareable/correlation_matrix.csv') %>% as.matrix()
  rownames(cormat_all) = colnames(cormat_all)
}

################## Run Models ##################
# Loop through each combination of driving variable and cognitive all_paramscome variable
all_params = expand.grid(IV = ivs, DV = dvs, beta = NA, se = NA, 
                         CI_low = NA, CI_high = NA, t = NA, df_error = NA, p = NA)

for (IV in ivs) {
  for (DV in dvs) {
    
    if (USE_RAW){
      fit = fit_lm(dat, IV, DV, scale = TRUE)
      # store params of IV
      IV_params = fit$params %>% filter(Parameter=='IV')
      all_params[all_params$IV == IV & all_params$DV == DV, 'beta'] = IV_params$Coefficient
      all_params[all_params$IV == IV & all_params$DV == DV, 'se'] = IV_params$SE
      all_params[all_params$IV == IV & all_params$DV == DV, 'CI_low'] = IV_params$CI_low
      all_params[all_params$IV == IV & all_params$DV == DV, 'CI_high'] = IV_params$CI_high
      all_params[all_params$IV == IV & all_params$DV == DV, 't'] = IV_params$t
      all_params[all_params$IV == IV & all_params$DV == DV, 'df_error'] = IV_params$df_error
      all_params[all_params$IV == IV & all_params$DV == DV, 'p'] = IV_params$p
    } else {
      fit = fit_lm_cor(IV_name = IV, DV_name = DV, cormat = cormat_all)
      # store params of IV
      IV_params = fit %>% filter(Parameter==IV)
      all_params[all_params$IV == IV & all_params$DV == DV, 'beta'] = IV_params$beta
      all_params[all_params$IV == IV & all_params$DV == DV, 'se'] = IV_params$se
      all_params[all_params$IV == IV & all_params$DV == DV, 'CI_low'] = IV_params$CI_low
      all_params[all_params$IV == IV & all_params$DV == DV, 'CI_high'] = IV_params$CI_high
      all_params[all_params$IV == IV & all_params$DV == DV, 't'] = IV_params$t
      all_params[all_params$IV == IV & all_params$DV == DV, 'df_error'] = IV_params$df_error
      all_params[all_params$IV == IV & all_params$DV == DV, 'p'] = IV_params$p
    }
  }
}

# correct p
all_params$p.adj = p.adjust(all_params$p, method='bonferroni')

# p < .05
all_params.sig = all_params %>% filter(p.adj < .05)

# N of sig relationships
all_params.sig %>% count(DV)

all_params.sig %>% count(IV)

# exclusive relationship: an IV only predicts one DV, among all DVs (marker)
all_params.sig %>% filter(IV %in% (count(., IV) %>% filter(n == 1) %>% pull(IV)))

################## Save Results ##################
write.csv(all_params, 'all_bivariate_relationships.csv', row.names = F)
write.csv(all_params.sig, 'significant_bivariate_relationships.csv', row.names = F)

################## Supplemental Analysis ##################
# controlling for MilesPerTrip_n
if (USE_RAW){
  fit_lm(dat, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', scale = TRUE)$params
} else {
  fit_lm_cor(IV_name = 'MinutesPerTrip_n', DV_name = 'phys', cormat = cormat_all)
}

if (USE_RAW){
  fit_lm(dat, IV_name = 'MinutesPerTrip_n', DV_name = 'phys', COV_name = c('MilesPerTrip_n'), scale = TRUE)$params
} else {
  fit_lm_cor(IV_name = 'MinutesPerTrip_n', DV_name = 'phys', COV_name = c('MilesPerTrip_n'), cormat = cormat_all)
}

# controlling for MilesPerChain_n
if (USE_RAW){
  fit_lm(dat, IV_name = 'MinutesPerChain_n', DV_name = 'phys', scale = TRUE)$params
} else {
  fit_lm_cor(IV_name = 'MinutesPerChain_n', DV_name = 'phys', cormat = cormat_all)
}

if (USE_RAW){
  fit_lm(dat, IV_name = 'MinutesPerChain_n', DV_name = 'phys', COV_name = c('MilesPerChain_n'), scale = TRUE)$params
} else {
  fit_lm_cor(IV_name = 'MinutesPerChain_n', DV_name = 'phys', COV_name = c('MilesPerChain_n'), cormat = cormat_all)
}

