###############################################################################
# reproduce_prediction.R
#
# Reproduces the out-of-time prediction results (Figure 5 / Figure S1) from the
# de-identified per-subject squared-error files in prediction_errors/ -- no
# individual predictor data required.
#
# For each outcome and each of the four model comparisons it computes the mean
# squared-error difference (dMSE) and a one-tailed sign-flip permutation test
# (10,000 permutations), then applies Benjamini-Yekutieli FDR across the 12
# outcomes within each comparison family -- exactly the paper's procedure.
###############################################################################

rm(list = ls())
get_script_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE); fa <- grep("^--file=", a, value = TRUE)
  if (length(fa)) return(dirname(normalizePath(sub("^--file=", "", fa))))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  getwd()
}
setwd(get_script_dir())
set.seed(1)                              # permutation reproducibility
options(scipen = 999)

# comparisons: first model expected to beat the second (dMSE < 0 = improvement)
comparisons <- list(
  driving_vs_baseline = c('sq_err_driving', 'sq_err_baseline'),
  demo_vs_baseline    = c('sq_err_demo',    'sq_err_baseline'),
  full_vs_demo        = c('sq_err_full',    'sq_err_demo'),
  full_vs_driving     = c('sq_err_full',    'sq_err_driving'))

sign_flip_p <- function(d, B = 10000) {   # one-tailed: H1 mean(d) < 0
  obs <- mean(d)
  perm <- replicate(B, mean(d * sample(c(-1, 1), length(d), replace = TRUE)))
  (sum(perm <= obs) + 1) / (B + 1)
}

reproduce_model <- function(mdl) {
  dir <- file.path('prediction_errors', mdl)
  files <- list.files(dir, pattern = '\\.csv$')
  outcomes <- sub('_per_subject\\.csv$', '', files)
  rows <- list()
  for (i in seq_along(files)) {
    e <- read.csv(file.path(dir, files[i]))
    r <- list(outcome = outcomes[i], n = nrow(e))
    for (m in c('full','driving','demo','baseline'))
      r[[paste0('mse_', m)]] <- mean(e[[paste0('sq_err_', m)]])
    for (cmp in names(comparisons)) {
      cc <- comparisons[[cmp]]; d <- e[[cc[1]]] - e[[cc[2]]]
      r[[paste0('dMSE_', cmp)]]   <- mean(d)
      r[[paste0('perm_p_', cmp)]] <- sign_flip_p(d)
    }
    rows[[i]] <- as.data.frame(r, stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, rows)
  # BY-FDR within each comparison family across the 12 outcomes
  for (cmp in names(comparisons))
    res[[paste0('perm_p_', cmp, '_fdr')]] <-
      p.adjust(res[[paste0('perm_p_', cmp)]], method = 'BY')
  write.csv(res, sprintf('reproduced/repro_prediction_%s.csv', mdl), row.names = FALSE)
  res
}

dir.create('reproduced', showWarnings = FALSE)
for (mdl in c('ridge', 'enet')) {
  res <- reproduce_model(mdl)
  sig_db <- res[res$perm_p_driving_vs_baseline_fdr < .05, 'outcome']
  sig_fd <- res[res$perm_p_full_vs_demo_fdr        < .05, 'outcome']
  cat(sprintf('\n[%s] driving > baseline (FDR<.05): %s\n', mdl, paste(sig_db, collapse = ', ')))
  cat(sprintf('[%s] full > demographics (FDR<.05): %s\n', mdl, paste(sig_fd, collapse = ', ')))
}
