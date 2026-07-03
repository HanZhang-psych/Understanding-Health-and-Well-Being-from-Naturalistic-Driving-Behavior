###############################################################################
# Exploratory (Year 1) bivariate analyses.

# Outputs:
#   output/y1_bivariate_all.csv
#   output/y1_bivariate_sig.csv
###############################################################################

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

library(tidyverse)
library(easystats)
source('funcs.R')

# ---- Load Year 1 subject-level data ------------------------------------------
dat = read.csv('../data/y1_subject_level.csv')

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

cat(sprintf("Y1 sample N = %d\n", nrow(dat)))

# ---- Run all 24 x 12 = 288 bivariate regressions ----------------------------
all_params = expand.grid(IV = ivs, DV = dvs,
                         N = NA,
                         beta = NA, se = NA,
                         CI_low = NA, CI_high = NA,
                         t = NA, df_error = NA, p = NA,
                         stringsAsFactors = FALSE)

for (IV in ivs) {
  for (DV in dvs) {
    fit = fit_lm(dat, IV, DV, scale = TRUE)
    IV_params = fit$params %>% filter(Parameter == 'IV')
    sel = all_params$IV == IV & all_params$DV == DV
    all_params[sel, 'N']        = IV_params$N
    all_params[sel, 'beta']     = IV_params$Coefficient
    all_params[sel, 'se']       = IV_params$SE
    all_params[sel, 'CI_low']   = IV_params$CI_low
    all_params[sel, 'CI_high']  = IV_params$CI_high
    all_params[sel, 't']        = IV_params$t
    all_params[sel, 'df_error'] = IV_params$df_error
    all_params[sel, 'p']        = IV_params$p
  }
}

# p-value adjustment
all_params$p.adj = p.adjust(all_params$p, method = 'BY')

# Survivors carried forward to confirmation
all_params.sig = all_params %>% filter(p.adj < .05)

cat(sprintf("Y1 significant bivariate associations: %d\n",
            nrow(all_params.sig)))
cat("Breakdown by DV:\n")
print(all_params.sig %>% count(DV))
cat("Breakdown by IV:\n")
print(all_params.sig %>% count(IV))

# ---- Save --------------------------------------------------------------------
write.csv(all_params,     'output/y1_bivariate_all.csv',  row.names = FALSE)
write.csv(all_params.sig, 'output/y1_bivariate_sig.csv',  row.names = FALSE)