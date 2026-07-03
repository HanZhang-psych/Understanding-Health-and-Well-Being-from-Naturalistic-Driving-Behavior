###############################################################################
# prepare_prediction_errors.R
#
# Copies the per-subject squared-error files (Option B) into the shareable
# bundle, DROPPING the subject identifier column. The four error columns and
# their row-pairing are all that the reproduction needs; no predictors or IDs
# are retained. Run once by someone with access to analysis/output/.
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

for (mdl in c('ridge', 'enet')) {
  src <- sprintf('../analysis/output/%s_errors', mdl)
  dst <- file.path('prediction_errors', mdl)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(src, pattern = '\\.csv$')) {
    d <- read.csv(file.path(src, f))
    d$subject <- NULL                     # drop the identifier
    write.csv(d, file.path(dst, f), row.names = FALSE)
  }
  cat(sprintf('%s: de-identified %d outcome files\n', mdl, length(list.files(dst))))
}
