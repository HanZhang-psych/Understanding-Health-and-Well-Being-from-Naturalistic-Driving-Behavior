###############################################################################
# Confirmatory (Year 2) bivariate analyses.
#
# The tested family is exactly the set of (IV, DV) pairs that survived
# p-value correction in the Y1 bivariate analysis. No additional pairs are tested.

# Outputs:
#   output/y2_bivariate_sig.csv  - confirmatory tests with replication flag
#   output/y2_bivariate_all.csv  - all 288 Y2 tests, for reference / supplement
###############################################################################

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

library(tidyverse)
library(easystats)
source('funcs.R')

# ---- Load Year 2 subject-level data ------------------------------------------
dat = read.csv('../data/y2_subject_level.csv')

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

cat(sprintf("Y2 confirmation sample N = %d\n", nrow(dat)))

# ---- Load the Y1 discovery pairs (the only ones we test confirmatorily) ------
y1_sig = read.csv('output/y1_bivariate_sig.csv',
                  stringsAsFactors = FALSE)

cat(sprintf("Confirmatory family size (pairs surviving Y1): %d\n",
            nrow(y1_sig)))

# ---- For reference: also run all 288 tests in Y2 (two-tailed) ---------------
# This is not part of the inferential pipeline, but useful for the supplement
# so readers can see Y2-only effects that did not survive Y1.
all_y2 = expand.grid(IV = ivs, DV = dvs, 
                     N = NA,
                     beta = NA, se = NA,
                     CI_low = NA, CI_high = NA,
                     t = NA, df_error = NA, p_two_tailed = NA,
                     stringsAsFactors = FALSE)

for (IV in ivs) {
  for (DV in dvs) {
    fit = fit_lm(dat, IV, DV, scale = TRUE)
    IV_params = fit$params %>% filter(Parameter == 'IV')
    sel = all_y2$IV == IV & all_y2$DV == DV
    all_y2[sel, 'N']            = IV_params$N
    all_y2[sel, 'beta']         = IV_params$Coefficient
    all_y2[sel, 'se']           = IV_params$SE
    all_y2[sel, 'CI_low']       = IV_params$CI_low
    all_y2[sel, 'CI_high']      = IV_params$CI_high
    all_y2[sel, 't']            = IV_params$t
    all_y2[sel, 'df_error']     = IV_params$df_error
    all_y2[sel, 'p_two_tailed'] = IV_params$p
  }
}

# ---- Confirmatory testing: one-tailed in the Y1 direction --------------------
conf = y1_sig %>%
  rename_with(~ paste0("y1_", .x), -c(IV, DV)) %>%
  mutate(y1_dir = sign(y1_beta))

# attach Y2 estimates for the same (IV, DV) pairs
conf = conf %>%
  left_join(all_y2 %>%
              select(IV, DV,
                     y2_N       = N,
                     y2_beta    = beta,
                     y2_se      = se,
                     y2_CI_low  = CI_low,
                     y2_CI_high = CI_high,
                     y2_t       = t,
                     y2_df      = df_error,
                     y2_p_two_tailed = p_two_tailed),
            by = c("IV", "DV"))

# one-tailed p in the Y1 direction
conf = conf %>%
  rowwise() %>%
  mutate(y2_p_one_tailed = make_one_tailed_p(y2_t, y2_df, y1_dir)) %>%
  ungroup()

# p-value correction
conf$y2_p_adj = p.adjust(conf$y2_p_one_tailed, method = 'BY')

# replication flag: same sign AND FDR-significant one-tailed p in Y2
conf$replicated = (sign(conf$y2_beta) == conf$y1_dir) & (conf$y2_p_adj < .05)

# nice column ordering
conf = conf %>%
  select(IV, DV, y1_N,
         y1_beta, y1_CI_low, y1_CI_high, y1_t, y1_df_error, y1_p, y1_p.adj, y1_dir,
         y2_N,
         y2_beta, y2_CI_low, y2_CI_high, y2_t, y2_df,
         y2_p_two_tailed, y2_p_one_tailed, y2_p_adj,
         replicated)

cat(sprintf("Pairs replicated (same sign AND Y2 p.adj < .05): %d / %d\n",
            sum(conf$replicated, na.rm = TRUE),
            nrow(conf)))

# ---- Save --------------------------------------------------------------------
write.csv(all_y2, 'output/y2_bivariate_all.csv', row.names = FALSE)
write.csv(conf, 'output/y2_bivariate_sig.csv', row.names = FALSE)
