###############################################################################
# build_summary_stats.R
#
# Generates the SHAREABLE summary-statistics bundle (Option A) from the
# controlled-access subject-level data. Run this ONCE by someone with data
# access; the outputs in reproducibility/summary_stats/ contain no individual
# records and can be shared freely.
#
# Outputs (per year, Y1 and Y2):
#   summary_stats/y{1,2}_covariance.csv     - covariance matrix over all
#                                             analysis variables (driving,
#                                             outcomes, demographics as dummies)
#   summary_stats/y{1,2}_variable_summary.csv - per-variable mean, sd, n
#   summary_stats/y{1,2}_pairwise_n.csv     - pairwise complete-obs counts
#   summary_stats/items_baseline_cor/<scale>.csv - baseline item correlation
#                                             matrices (for Cronbach's alpha)
#
# All matrices use pairwise-complete observations, matching the analysis-level
# (per-model complete-case) missing-data policy as closely as a single shared
# matrix can.
###############################################################################

rm(list = ls())
# set working directory to this script's location (works under Rscript & RStudio)
get_script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", a, value = TRUE)
  if (length(fa)) return(dirname(normalizePath(sub("^--file=", "", fa))))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
}
setwd(get_script_dir())
options(scipen = 999)

suppressMessages({library(tidyverse); library(fastDummies)})
source('../analysis/funcs.R')   # ivs, dvs, add_dummies_for_pathmodel

OUT <- 'summary_stats'
dir.create(file.path(OUT, 'items_baseline_cor'), recursive = TRUE, showWarnings = FALSE)

# demographic pieces: 3 continuous + the exact dummy columns used by the
# path models (add_dummies_for_pathmodel drops the first level of each factor)
cont_demo <- c('Age', 'EDUCATION', 'INCOME')
dummy_cols_used <- c(
  'Site_JHSPH','Site_UCDENVER','Site_UCSD','Site_UMTRI',
  'GENDER_Female','RACE_Black','RACE_American_Indian','RACE_Asian',
  'RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander',
  'RACE_Other_Non_Hispanic','RACE_Hispanic','WORK_Working',
  'MARRIAGE_Living_with_a_partner','MARRIAGE_Separated','MARRIAGE_Divorced',
  'MARRIAGE_Widowed','MARRIAGE_Never_married')

build_one_year <- function(path, tag) {
  dat <- read.csv(path) %>% add_dummies_for_pathmodel()
  vars <- c(ivs, dvs, cont_demo, dummy_cols_used)
  X <- dat %>% select(all_of(vars)) %>% mutate(across(everything(), as.numeric))

  # LISTWISE-complete sample -> one internally-consistent, positive-definite
  # covariance matrix. (A single pairwise matrix blends different subsamples and
  # distorts multiple-regression / path coefficients; listwise keeps the sample
  # consistent, which matches the path models' own listwise policy.)
  Xcc    <- X[stats::complete.cases(X), ]
  ncc    <- nrow(Xcc)
  covmat <- cov(Xcc)
  vsum   <- tibble(variable = vars, mean = sapply(Xcc, mean),
                   sd = sapply(Xcc, sd), n = ncc)

  write.csv(round(covmat, 8), file.path(OUT, sprintf('%s_covariance.csv', tag)))
  write.csv(vsum, file.path(OUT, sprintf('%s_variable_summary.csv', tag)), row.names = FALSE)
  cat(sprintf('[%s] %d variables, listwise-complete N = %d\n', tag, length(vars), ncc))
}

build_one_year('../data/y1_subject_level.csv', 'y1')
build_one_year('../data/y2_subject_level.csv', 'y2')


# ---- baseline item correlation matrices (for Cronbach's alpha) ---------------
CSV <- '../data/Data Files/CSV'
recode_na <- function(df, cols){ for(c in cols) df[[c]] <- ifelse(df[[c]] %in% c(-1,-2,-3), NA, df[[c]]); df }
item_map <- list(
  cog        = list("DR.CH VALUES.csv", c('CH_2A','CH_2B','CH_2C','CH_2D')),
  phys       = list("DR.PH VALUES.csv", c('PH_1A','PH_1B','PH_1C','PH_1D')),
  fati       = list("DR.PH VALUES.csv", c('PH_4A','PH_4B','PH_4C','PH_4D')),
  dep        = list("DR.MH VALUES.csv", c('MH_1A','MH_1B','MH_1C','MH_1D')),
  anx        = list("DR.MH VALUES.csv", c('MH_2A','MH_2B','MH_2C','MH_2D')),
  ang        = list("DR.MH VALUES.csv", c('MH_3A','MH_3B','MH_3C','MH_3D','MH_3E')),
  socialrole = list("DR.SH VALUES.csv", c('SH_1A','SH_1B','SH_1C','SH_1D')),
  info       = list("DR.SH VALUES.csv", c('SH_2A','SH_2B','SH_2C','SH_2D')),
  emo        = list("DR.SH VALUES.csv", c('SH_3A','SH_3B','SH_3C','SH_3D')),
  iso        = list("DR.SH VALUES.csv", c('SH_4A','SH_4B','SH_4C','SH_4D')),
  ins        = list("DR.SH VALUES.csv", c('SH_5A','SH_5B','SH_5C','SH_5D')),
  lifesat    = list("DR.SH VALUES.csv", c('SH_7A','SH_7B','SH_7C','SH_7D','SH_7E')))

for (nm in names(item_map)) {
  f <- item_map[[nm]][[1]]; items <- item_map[[nm]][[2]]
  d <- read.csv(file.path(CSV, f))
  d <- d[d$INTERVAL == 0, items, drop = FALSE]
  d <- recode_na(d, items)
  R <- cor(d, use = 'pairwise.complete.obs')
  write.csv(round(R, 8), file.path(OUT, 'items_baseline_cor', sprintf('%s.csv', nm)))
}
cat('Baseline item correlation matrices written for', length(item_map), 'scales\n')
