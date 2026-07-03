###############################################################################
# y1_multivariate_exploratory.R
#
# Exploratory (Year 1) multivariate path analyses.
#
# Logic:
#   * Any driving IV with at least one significant Y1 bivariate DV on a specific health domain
#     enters the path analysis. 
#   * The IV simultaneously predicts all 11 DVs while controlling for
#     the standard demographic covariates.
#   * The CANDIDATE DV is identified from the Y1 SIGNIFICANT bivariate set
#     among decline outcomes (the sig decline DV with the largest |beta|; or
#     the single sig decline DV if only one). The OTHER DVs are the remaining
#     10 of the 11 decline outcomes, including DVs that were non-significant
#     in the bivariate analysis.
#   * Every candidate-vs-other contrast is computed in Y1. FDR
#     correction is applied across all such contrasts. 
#
# Missing data:
#   * lavaan is called with missing = "listwise", fixed.x = TRUE. Each path
#     model uses complete cases on the IV, all 11 decline DVs, and the
#     demographic covariates. Matches lm()'s row-wise complete-case behavior
#     in the bivariate step.
#   * Per-model N is captured and saved alongside each contrast.
###############################################################################

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 999)

library(tidyverse)
library(easystats)
library(lavaan)
library(fastDummies)
source('funcs.R')

# ---- Load Year 1 data, scale numerics (NA-safe), add dummy variables --------
dat = read.csv('../data/y1_subject_level.csv') %>%
  mutate(across(all_of(c(ivs, dvs, 'Age', 'EDUCATION', 'INCOME')),
                ~ safe_scale(.))) %>%
  add_dummies_for_pathmodel()

cat(sprintf("Y1 file rows = %d (per-model N reported below via listwise)\n",
            nrow(dat)))

# ---- Identify candidate / others per IV from Y1 significant bivariates ------
y1_sig = read.csv('output/y1_bivariate_sig.csv', stringsAsFactors = FALSE)

# Decline outcomes only: exclude LIFESATISFACTION because it is scored in the
# opposite direction (higher = better well-being) from the other 11 outcomes
# (higher = worse). Including it in the candidate-vs-other contrasts would
# inflate the contrasts purely by reversed polarity. LS is therefore excluded
# from the path model entirely; the LS-driving relationships are still tested
# at the bivariate level.
non_LS_dvs = setdiff(dvs, 'LIFESATISFACTION')

# An IV is eligible for the path model if it has >=1 Y1-sig bivariate effect
# on a DECLINE outcome (not just on LIFESATISFACTION).
y1_sig_decline = y1_sig %>% filter(DV %in% non_LS_dvs)

ivs_for_path = y1_sig_decline %>%
  count(IV) %>%
  filter(n >= 1) %>%
  pull(IV)

# Per-IV specification:
#   candidate  = the Y1-sig decline DV with the largest |beta|
#   other_DVs  = the remaining 10 of the 11 decline outcomes
pathspec = lapply(ivs_for_path, function(this_IV) {
  sig_for_iv = y1_sig_decline %>%
    filter(IV == this_IV) %>%
    mutate(abs_beta = abs(beta)) %>%
    arrange(desc(abs_beta))
  cand = sig_for_iv$DV[1]
  others = sort(setdiff(non_LS_dvs, cand))   # 10 decline outcomes
  tibble(
    IV = this_IV,
    candidate_DV = cand,
    candidate_beta_y1 = sig_for_iv$beta[1],
    n_sig_DVs_y1 = nrow(sig_for_iv),
    other_DVs = paste(others, collapse = "|")
  )
}) %>% bind_rows()

# ---- Fit Y1 path models and extract paths + contrasts -----------------------
all_paths = list()
all_contrasts = list()
n_used = integer(nrow(pathspec))

for (i in seq_len(nrow(pathspec))) {
  this_IV   = pathspec$IV[i]
  this_cand = pathspec$candidate_DV[i]
  others    = strsplit(pathspec$other_DVs[i], "\\|")[[1]]

  res = fit_pathmodel_oneIV(dat, this_IV, this_cand, others,
                            missing = "listwise", fixed_x = TRUE)

  this_n = lavaan::lavInspect(res$fit, "ntotal")
  n_used[i] = this_n

  cat(sprintf("[Y1 path] %s : candidate = %s (n_sig_y1 = %d) ; N = %d\n",
              this_IV, this_cand, pathspec$n_sig_DVs_y1[i], this_n))

  all_paths[[this_IV]]     = res$regression_paths %>% mutate(n_obs = this_n)
  all_contrasts[[this_IV]] = res$contrasts %>%
    mutate(candidate_DV = this_cand, n_obs = this_n)
}

# add N to the spec file as well so it travels with the frozen specification
pathspec$n_obs_y1 = n_used

# extract all regression paths
# we don't care about these as they should be very similar to bivariate results
# pending small differences in N due to listwise deletion
paths_df = bind_rows(all_paths) %>%
  mutate(DV = str_split(Label, "_") %>% map_chr(2))

# extract all contrasts
contrasts_df = bind_rows(all_contrasts) %>%
  mutate(
    against_DV = str_split(Label, "_vs_") %>% map_chr(2),
    y1_contrast_dir = sign(Coefficient)
  ) %>%
  rename(
    y1_Coefficient = Coefficient,
    y1_SE          = SE,
    y1_CI_low      = CI_low,
    y1_CI_high     = CI_high,
    y1_z           = z,
    y1_p           = p,
    y1_n_obs       = n_obs
  ) %>%
  select(IV, candidate_DV, against_DV,
         y1_n_obs,
         y1_Coefficient, y1_SE, y1_CI_low, y1_CI_high, y1_z, y1_p,
         y1_contrast_dir, Label)

# p-value correction
contrasts_df$y1_p_adj = p.adjust(contrasts_df$y1_p, method = 'BY')

# Filter to Y1-significant contrasts: this is the family that will be tested in Y2.
contrasts_sig = contrasts_df %>% filter(y1_p_adj < .05)

# ---- Save --------------------------------------------------------------------
write.csv(contrasts_sig,    'output/y1_multivariate_contrasts_sig.csv', row.names = FALSE)
write.csv(pathspec, 'output/y1_multivariate_modelspec.csv', row.names = FALSE)
