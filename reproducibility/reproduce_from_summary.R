###############################################################################
# reproduce_from_summary.R
#
# Reproduces the paper's regression-based results using ONLY the shareable
# summary-statistics bundle in summary_stats/ (no individual-level data).
#
# Reproduces:
#   * Bivariate associations  -> Table 4, life-satisfaction text, Table S5
#   * Multivariate contrasts   -> Table S4 / Figure 4 (via lavaan on the cov matrix)
#   * Internal reliability      -> Cronbach's alpha (.76-.90), standardized
#
# Writes its results to reproduced/ so they can be compared to the paper.
# Requires only: lavaan (for the path models). Base R does the rest.
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
options(scipen = 999)
suppressMessages(library(lavaan))
dir.create('reproduced', showWarnings = FALSE)

SS <- 'summary_stats'
loadmat <- function(f) as.matrix(read.csv(file.path(SS, f), row.names = 1, check.names = FALSE))

# ---- variable sets (self-contained; must match the bundle's column names) ----
dvs11 <- c('cog','phys','fati','socialrole','iso','info','emo','ins','dep','anx','ang')
cont_demo <- c('Age','EDUCATION','INCOME')
dummies <- c('Site_JHSPH','Site_UCDENVER','Site_UCSD','Site_UMTRI',
  'GENDER_Female','RACE_Black','RACE_American_Indian','RACE_Asian',
  'RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander','RACE_Other_Non_Hispanic',
  'RACE_Hispanic','WORK_Working','MARRIAGE_Living_with_a_partner','MARRIAGE_Separated',
  'MARRIAGE_Divorced','MARRIAGE_Widowed','MARRIAGE_Never_married')
demo_terms <- paste(c(cont_demo, dummies), collapse = ' + ')

# =========================== 1. BIVARIATE ====================================
# Standardized multiple-regression coefficient for one driving IV on one
# outcome, controlling for demographics, recovered from the covariance matrix.
bivariate <- function(cov, n, IV, DV, extra = NULL) {
  preds <- c(IV, extra, cont_demo, dummies)
  vars  <- c(DV, preds)
  R     <- cov2cor(cov[vars, vars])
  Rxx_i <- solve(R[preds, preds]); rxy <- R[preds, DV]
  beta  <- as.numeric(Rxx_i %*% rxy); names(beta) <- preds
  R2    <- sum(rxy * beta)
  p <- length(preds); df <- n - p - 1
  se    <- sqrt((1 - R2) / df * diag(Rxx_i))
  b <- beta[IV]; s <- se[IV]; t <- b / s; tc <- qt(.975, df)
  data.frame(IV, DV, beta = b, ci_lo = b - tc * s, ci_hi = b + tc * s,
             t = t, df = df, n = n, row.names = NULL)
}

# reported (IV, DV) pairs: Table 4 + life-satisfaction text
reported <- rbind(
  data.frame(IV='Average_speed',            DV='LIFESATISFACTION'),
  data.frame(IV='PercentTripsAMPeak_n',     DV='LIFESATISFACTION'),
  data.frame(IV='PercentTripsPMPeak_n',     DV='LIFESATISFACTION'),
  data.frame(IV='DecelerationPer1000Miles', DV='LIFESATISFACTION'),
  data.frame(IV='DaysDriving',              DV='phys'),
  data.frame(IV='Trips',                    DV='phys'),
  data.frame(IV='MinutesPerTrip_n',         DV='phys'),
  data.frame(IV='Average_speed',            DV='phys'),
  data.frame(IV='TripsAMPeak',              DV='phys'),
  data.frame(IV='PercentTripsAMPeak_n',     DV='phys'),
  data.frame(IV='TripsLt15Miles',           DV='phys'),
  data.frame(IV='Average_speed',            DV='socialrole'),
  data.frame(IV='TripsAMPeak',              DV='socialrole'),
  data.frame(IV='PercentTripsAMPeak_n',     DV='socialrole'),
  data.frame(IV='PercentTripsPMPeak_n',     DV='socialrole'),
  data.frame(IV='Average_speed',            DV='ang'))

nfor <- function(tag) read.csv(file.path(SS, sprintf('%s_variable_summary.csv', tag)))$n[1]
run_biv_year <- function(tag) {
  cov <- loadmat(sprintf('%s_covariance.csv', tag)); n <- nfor(tag)
  res <- do.call(rbind, Map(function(iv, dv) bivariate(cov, n, iv, dv),
                            reported$IV, reported$DV))
  res$year <- tag; res
}
biv <- rbind(run_biv_year('y1'), run_biv_year('y2'))
write.csv(biv, 'reproduced/repro_bivariate.csv', row.names = FALSE)

# Table S5: minutes-per-trip -> phys, controlling for miles-per-trip
cov1 <- loadmat('y1_covariance.csv'); n1 <- nfor('y1')
s5 <- rbind(
  cbind(model = 'demo',       bivariate(cov1, n1, 'MinutesPerTrip_n', 'phys')),
  cbind(model = 'demo+miles', bivariate(cov1, n1, 'MinutesPerTrip_n', 'phys',
                                        extra = 'MilesPerTrip_n')))
write.csv(s5, 'reproduced/repro_tableS5_y1.csv', row.names = FALSE)

# =========================== 2. MULTIVARIATE (SEM) ===========================
# frozen candidate DV per driving IV (from the paper's Y1 specification)
spec <- c(Average_speed='phys', DaysDriving='phys', DecelerationPer1000Miles='iso',
          MilesPerChain_n='cog', MinutesPerTrip_n='phys', PercentTripsAMPeak_n='socialrole',
          PercentTripsAtNight_n='socialrole', PercentTripsPMPeak_n='socialrole',
          Trips='phys', TripsAMPeak='phys', TripsAtNight='socialrole', TripsLt15Miles='phys')

# rescale raw covariance to (z-scored continuous vars + raw dummies), matching
# the analysis, using the shared SDs: Cov_scaled = diag(1/s) Cov diag(1/s)
scale_cov <- function(cov, vsum) {
  s <- setNames(vsum$sd, vsum$variable)[rownames(cov)]  # sd for every var...
  s[dummies] <- 1                                        # ...but keep dummies raw
  D <- diag(1 / s); dimnames(D) <- dimnames(cov)
  D %*% cov %*% D
}

fit_contrasts <- function(covS, IV, cand, n) {
  others <- setdiff(dvs11, cand)
  regs <- paste(sapply(c(cand, others),
    function(dv) sprintf('%s ~ b_%s*%s + %s', dv, dv, IV, demo_terms)), collapse = '\n')
  cons <- paste(sapply(others,
    function(dv) sprintf('%s_vs_%s := b_%s - b_%s', cand, dv, cand, dv)), collapse = '\n')
  fit <- sem(paste(regs, cons, sep = '\n'), sample.cov = covS, sample.nobs = n)
  pe <- parameterEstimates(fit)
  pe <- pe[pe$op == ':=', c('lhs','est','se','z','pvalue')]
  data.frame(IV = IV, candidate = cand,
             against = sub('.*_vs_', '', pe$lhs),
             est = pe$est, se = pe$se, z = pe$z, p_two = pe$pvalue, row.names = NULL)
}

vs2  <- read.csv(file.path(SS, 'y2_variable_summary.csv'))
cov2 <- loadmat('y2_covariance.csv'); covS2 <- scale_cov(cov2, vs2); n2 <- vs2$n[1]
contr <- do.call(rbind, lapply(names(spec), function(iv)
  fit_contrasts(covS2, iv, spec[[iv]], n2)))
write.csv(contr, 'reproduced/repro_contrasts_y2.csv', row.names = FALSE)

# =========================== 3. CRONBACH'S ALPHA =============================
# Standardized alpha from an item correlation matrix: k*rbar / (1+(k-1)*rbar)
std_alpha <- function(R) { k <- nrow(R); rbar <- mean(R[lower.tri(R)]); k*rbar/(1+(k-1)*rbar) }
scales <- sub('\\.csv$', '', list.files(file.path(SS,'items_baseline_cor')))
alpha <- data.frame(scale = scales, std_alpha = sapply(scales, function(s)
  std_alpha(loadmat(file.path('items_baseline_cor', paste0(s, '.csv'))))), row.names = NULL)
write.csv(alpha, 'reproduced/repro_alpha.csv', row.names = FALSE)

cat('Reproduced:', nrow(biv), 'bivariate rows,', nrow(contr), 'contrasts,',
    nrow(alpha), 'reliabilities. Alpha range:',
    sprintf('%.3f-%.3f', min(alpha$std_alpha), max(alpha$std_alpha)), '\n')
