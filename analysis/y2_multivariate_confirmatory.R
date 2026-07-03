###############################################################################
# y2_multivariate_confirmatory.R
#
# Confirmation (Year 2) multivariate path analyses.
#
# Pre-specification (locked in by Y1):
#   * Each surviving contrast is tested ONE-TAILED in the direction of the Y1
#     contrast estimate.
#   * FDR correction is applied over the surviving Y1 family.
#
#
# Note on model fitting:
#   * For each IV in the frozen spec we fit the SAME 11-DV path model that
#     was used in Y1. This preserves the multivariate model context (and SEs)
#     for the contrasts. We then extract only the specific (candidate, against)
#     contrasts that survived Y1.
#
# Missing data:
#   * lavaan is called with missing = "listwise", fixed.x = TRUE. Each Y2
#     path model uses complete cases on the IV, all 11 decline DVs, and the
#     demographic covariates. Matches lm()'s behavior in the Y2 bivariate step.
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

# ---- Load Year 2 data and add dummy variables --------------------------------
dat = read.csv('../data/y2_subject_level.csv') %>%
  mutate(across(all_of(c(ivs, dvs, 'Age', 'EDUCATION', 'INCOME')),
                ~ safe_scale(.))) %>%
  add_dummies_for_pathmodel()

cat(sprintf("Y2 file rows = %d (per-model N reported below via listwise)\n",
            nrow(dat)))

# ---- Load frozen Y1 specifications (sig-only) --------------------------------
pathspec    = read.csv('output/y1_multivariate_modelspec.csv', stringsAsFactors = FALSE)
y1_sig      = read.csv('output/y1_multivariate_contrasts_sig.csv',  stringsAsFactors = FALSE)

cat(sprintf("Number of IVs to fit in Y2 path models:  %d\n", nrow(pathspec)))
cat(sprintf("Number of surviving Y1 contrasts to test: %d\n", nrow(y1_sig)))

if (nrow(y1_sig) == 0) {
  stop("No Y1-FDR-significant contrasts; nothing to confirm in Y2.")
}

# ---- Fit Y2 path models (same all-12-DV structure as Y1) ---------------------
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

  cat(sprintf("[Y2 path] %s : candidate = %s ; N = %d\n",
              this_IV, this_cand, this_n))

  all_paths[[this_IV]]     = res$regression_paths %>% mutate(n_obs = this_n)
  all_contrasts[[this_IV]] = res$contrasts %>%
    mutate(candidate_DV = this_cand, n_obs = this_n)
}

paths_df = bind_rows(all_paths) %>%
  mutate(DV = str_split(Label, "_") %>% map_chr(2))

y2_contrasts_df = bind_rows(all_contrasts) %>%
  mutate(against_DV = str_split(Label, "_vs_") %>% map_chr(2)) %>%
  rename(
    y2_n_obs       = n_obs,
    y2_Coefficient = Coefficient,
    y2_SE          = SE,
    y2_CI_low      = CI_low,
    y2_CI_high     = CI_high,
    y2_z           = z,
    y2_p_two_tailed = p
  ) %>%
  select(IV, candidate_DV, against_DV,
         y2_n_obs, y2_Coefficient, y2_SE, y2_CI_low, y2_CI_high,
         y2_z, y2_p_two_tailed, Label)

# ---- Merge surviving Y1 contrasts with Y2 estimates -------------------------
# We join on (IV, candidate_DV, against_DV) using the sig-only Y1 set, so the
# Y2 confirmation tests EXACTLY the contrasts that survived Y1 FDR and
# no others. The Y2 path model produced all 11 contrasts per IV (same as Y1),
# but only the surviving ones are inferentially tested.
conf = y1_sig %>%
  select(IV, candidate_DV, against_DV,
         y1_n_obs, y1_Coefficient, y1_z, y1_p, y1_p_adj, y1_contrast_dir) %>%
  left_join(y2_contrasts_df, by = c("IV", "candidate_DV", "against_DV"))

# one-tailed p in the direction implied by Y1
# (Wald z is asymptotically normal, so we just convert via pnorm)
conf = conf %>%
  mutate(y2_p_one_tailed = case_when(
    y1_contrast_dir > 0 ~ pnorm(y2_z, lower.tail = FALSE),
    y1_contrast_dir < 0 ~ pnorm(y2_z, lower.tail = TRUE),
    TRUE ~ NA_real_
  ))

# p-value correction
conf$y2_p_adj = p.adjust(conf$y2_p_one_tailed, method = 'BY')

# replication flag: same sign AND Y2 one-tailed FDR < .05
conf$replicated = (sign(conf$y2_Coefficient) == conf$y1_contrast_dir) &
                  (conf$y2_p_adj < .05)

cat(sprintf("Contrasts replicated (same sign AND Y2 p < .05): %d / %d\n",
            sum(conf$replicated, na.rm = TRUE), nrow(conf)))

# ---- Save --------------------------------------------------------------------
write.csv(conf, 'output/y2_multivariate_contrasts_sig.csv', row.names = FALSE)
