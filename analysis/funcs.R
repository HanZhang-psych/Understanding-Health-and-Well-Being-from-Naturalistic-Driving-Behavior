###############################################################################
# Shared variables and helper functions for the exploratory -> confirmatory
# pipeline.
#
# Key helpers:
#   * fit_lm()              - standardized bivariate regression (one IV, one DV,
#                             demographic covariates).
#   * fit_pathmodel_oneIV() - single-IV / multi-DV lavaan path model with
#                             candidate-vs-other slope contrasts.
#   * make_one_tailed_p()   - one-tailed p-value in a pre-specified direction,
#                             used in the Year 2 confirmatory tests.
#
# The full exploratory -> confirmatory design is implemented in the calling
# scripts; this file holds only shared definitions and utilities.
###############################################################################

library(Polychrome)
library(fastDummies)
library(parameters)
library(lavaan)

# ---- Variable definitions ---------------------------------------------------
ivs = c('DaysDriving',
        'Miles_n',
        "Trips",
        "TripMinutes_n",
        "TripChains",
        "MilesPerTrip_n",
        "MinutesPerTrip_n",
        "MilesPerChain_n",
        "MinutesPerChain_n",
        "Average_speed",
        "LeftTurnCount",
        "RightToLeftTurnRatio_n",
        "TripsAMPeak",
        "PercentTripsAMPeak_n",
        "TripsPMPeak",
        "PercentTripsPMPeak_n",
        "TripsAtNight",
        "PercentTripsAtNight_n",
        "TripsLt15Miles",
        "PercentDistLt15Miles_n",
        "TripsVgt60",
        "PercentTripsVgt60_n",
        "SpeedingPer1000Miles",
        "DecelerationPer1000Miles")

dvs = c('LIFESATISFACTION', 'cog', 'phys', 'fati', 'socialrole',
        'iso', 'info', 'emo', 'ins', 'dep', 'anx', 'ang')

demos = c("Site", "Age", "GENDER", "RACE_ETH", "EDUCATION", "INCOME",
          "WORK", "MARRIAGE")

driving_var_recode_list = c('DaysDriving' = 'Number of Days Driving',
                            'Miles_n' = 'Total Miles',
                            'Trips' = 'Total Trips',
                            'TripMinutes_n' = 'Total Trip Minutes',
                            'TripChains' = 'Total Trip Chains',
                            'MilesPerTrip_n' = 'Miles Per Trip',
                            'MinutesPerTrip_n' = 'Minutes Per Trip',
                            'MilesPerChain_n' = 'Miles Per Chain',
                            'MinutesPerChain_n' = 'Minutes Per Chain',
                            'Average_speed' = 'Vehicle Speed',
                            'LeftTurnCount' = 'Number of Left Turns',
                            'RightToLeftTurnRatio_n' = 'Right to Left Turn Ratio',
                            'TripsAMPeak' = 'Number of Trips AM Rush Hour',
                            'PercentTripsAMPeak_n' = 'Percentage of Trips AM Rush Hour',
                            'TripsPMPeak' = 'Number of Trips PM Rush Hour',
                            'PercentTripsPMPeak_n' = 'Percentage of Trips PM Rush Hour',
                            'TripsAtNight' = 'Number of Trips at Night',
                            'PercentTripsAtNight_n' = 'Percentage of Trips at Night',
                            'TripsLt15Miles' = 'Number of Trips < 15 Miles',
                            'PercentDistLt15Miles_n' = 'Percentage of Trips < 15 Miles',
                            'TripsVgt60' = 'Number of Trips > 60 MPH',
                            'PercentTripsVgt60_n' = 'Percentage of Trips > 60 MPH',
                            'SpeedingPer1000Miles' = 'Speeding Events Per 1000 Miles',
                            'DecelerationPer1000Miles' = 'Hard-Braking Events Per 1000 Miles')


health_var_recode_list = c('LIFESATISFACTION' = 'Life Satisfaction',
                           'cog' = 'Cognitive Decline',
                           'phys' = 'Physical Decline',
                           'fati' = 'Fatigue',
                           'socialrole' = 'Role Constraints',
                           'iso' = 'Social Isolation',
                           'info' = 'Low Informational Support',
                           'emo' = 'Low Emotional Support',
                           'ins' = 'Low Instrumental Support',
                           'dep' = 'Depression',
                           'anx' = 'Anxiety',
                           'ang' = 'Anger')

DV_colors = sky.colors(length(dvs))
names(DV_colors) = unlist(health_var_recode_list)


# ---- NA-safe scaling --------------------------------------------------------
# Base R scale() returns all-NA if any input is NA. With the no-listwise-drop
# policy we sometimes have missing values, so we use a safe version that
# computes mean and sd ignoring NA and preserves NAs in the output.
safe_scale = function(x) {
  m = mean(x, na.rm = TRUE)
  s = sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - m) / s
}


# ---- Linear model (per-year, raw-data) ---------------------------------------
# The IV/DV/covariates are scaled
# WITHIN the supplied data frame, so when this is called separately on Y1 vs Y2
# subject-level data the standardization is within-year (and therefore
# standardized betas are comparable across years).
#
# Missing-data policy: scaling uses safe_scale (na.rm = TRUE), and lm() itself
# performs row-wise complete-case deletion for the specific regression. The
# residual df reported by lm() reflects the actual N used for that test.
fit_lm = function(dat, IV_name, DV_name, COV_name = NULL, scale = TRUE) {

  df_lm = dat %>%
    mutate(IV = !!sym(IV_name),
           DV = !!sym(DV_name)) %>%
    select(XID, IV, DV, Site, Age, GENDER, RACE_ETH, EDUCATION,
           INCOME, WORK, MARRIAGE, all_of(COV_name)) %>%
    mutate(
      XID = as.factor(XID),
      IV = as.numeric(IV),
      DV = as.numeric(DV),
      Site = as.factor(Site),
      Age = as.numeric(Age),
      GENDER = as.factor(GENDER),
      RACE_ETH = as.factor(RACE_ETH),
      EDUCATION = as.numeric(EDUCATION),
      INCOME = as.numeric(INCOME),
      WORK = as.factor(WORK),
      MARRIAGE = as.factor(MARRIAGE)
    )

  if (scale) {
    df_lm$IV = safe_scale(df_lm$IV)
    df_lm$DV = safe_scale(df_lm$DV)
    df_lm$Age = safe_scale(df_lm$Age)
    df_lm$EDUCATION = safe_scale(df_lm$EDUCATION)
    df_lm$INCOME = safe_scale(df_lm$INCOME)
    if (!is.null(COV_name)) {
      for (cov in COV_name) {
        if (is.numeric(df_lm[[cov]])) {
          df_lm[[cov]] = safe_scale(df_lm[[cov]])
        }
      }
    }
  }

  base_covariates <- c("Age", "EDUCATION", "INCOME", "Site",
                       "GENDER", "RACE_ETH", "WORK", "MARRIAGE")
  all_covariates <- c(base_covariates, COV_name)

  formula_str <- paste("DV ~ IV +", paste(all_covariates, collapse = " + "))
  model_formula <- as.formula(formula_str)

  model = lm(model_formula, data = df_lm)
  params = as.data.frame(parameters(model))
  # add a sample size column
  params$N = nrow(model.frame(model))
  return(list(model = model, params = params))
}


# ---- Helper: pre-specified one-tailed p-value --------------------------------
# Given a t-statistic and df, return a one-tailed p-value in the direction
# specified by `dir` (+1 for upper-tail / positive effect, -1 for lower-tail).
# Used in the Y2 confirmation step where direction is locked in by Y1.
make_one_tailed_p = function(t_stat, df, dir) {
  if (dir > 0) {
    return(pt(t_stat, df = df, lower.tail = FALSE))
  } else if (dir < 0) {
    return(pt(t_stat, df = df, lower.tail = TRUE))
  } else {
    return(NA_real_)
  }
}


# ---- Lavaan path model: confirmatory, single-IV, multi-DV --------------------
# Build and fit a path model that simultaneously regresses several DVs on a
# single driving IV (plus standard demographic covariates), and defines
# slope-difference contrasts of a candidate DV against each "other" DV.
#
# Arguments:
#   dat            - subject-level data frame (Y1 or Y2)
#   IV_name        - name of the driving variable
#   candidate_DV   - the DV whose slope is treated as the reference
#   other_DVs      - vector of DV names to be contrasted against the candidate
#   missing        - lavaan missing-data option. Default "listwise" matches
#                    R's lm() behavior in the bivariate step: each path model
#                    uses subjects with complete data on the IV, all DVs in
#                    the model, and the demographic covariates. "fiml" is
#                    available for sensitivity checks (full-information ML),
#                    but listwise is the recommended default here for
#                    consistency with the bivariate analyses.
#   fixed_x        - lavaan fixed.x argument. Default TRUE (the lavaan
#                    default) treats exogenous variables as fixed regressors;
#                    rows with missing IV or covariate are dropped along with
#                    listwise. Set FALSE only if combined with missing = "fiml"
#                    to apply ML to exogenous variables too; note that this
#                    treats dummy 0/1 variables as if normally distributed,
#                    which is technically a violation of the FIML assumption.
#
# Returns a named list with:
#   fit            - the fitted lavaan object
#   regression_paths - data frame of IV->DV regression paths
#   contrasts      - data frame of (candidate - other) slope differences
fit_pathmodel_oneIV = function(dat, IV_name, candidate_DV, other_DVs,
                               missing = "listwise", fixed_x = TRUE) {

  if (length(other_DVs) == 0) {
    stop("other_DVs is empty; nothing to contrast against the candidate.")
  }

  demo_terms = paste(
    "Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD +",
    "Site_UMTRI + GENDER_Female + RACE_Black + RACE_American_Indian +",
    "RACE_Asian + RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander +",
    "RACE_Other_Non_Hispanic + RACE_Hispanic + WORK_Working +",
    "MARRIAGE_Living_with_a_partner + MARRIAGE_Separated +",
    "MARRIAGE_Divorced + MARRIAGE_Widowed + MARRIAGE_Never_married"
  )

  # build per-DV regression lines
  all_DVs = c(candidate_DV, other_DVs)
  reg_lines = sapply(all_DVs, function(DV) {
    sprintf("%s ~ b_%s*%s + %s", DV, DV, IV_name, demo_terms)
  })
  reg_block = paste(reg_lines, collapse = "\n  ")

  # build contrast lines
  contrast_lines = sapply(other_DVs, function(DV) {
    sprintf("%s_vs_%s := b_%s - b_%s", candidate_DV, DV, candidate_DV, DV)
  })
  contrast_block = paste(contrast_lines, collapse = "\n  ")

  model_text = paste0("  ", reg_block, "\n\n  ", contrast_block, "\n")
  
  # print model
  # cat(model_text)
  
  # fit lavaan model
  fit = lavaan::sem(model_text, data = dat, estimator = 'ML',
                    missing = missing, fixed.x = fixed_x)

  reg_params = parameters(fit) %>%
    as.data.frame() %>%
    filter(Component == 'Regression' & Label != '') %>%
    select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>%
    mutate(IV = IV_name)

  contrast_params = parameters(fit) %>%
    as.data.frame() %>%
    filter(Component == 'Defined') %>%
    select(Coefficient, SE, CI_low, CI_high, z, p, Label) %>%
    mutate(IV = IV_name)

  list(fit = fit,
       regression_paths = reg_params,
       contrasts = contrast_params)
}


# ---- Helper: add dummy variables matching the lavaan model spec --------------
# Lavaan does not auto-dummify factors. This helper produces the dummy
# variables with the exact names used in fit_pathmodel_oneIV().
add_dummies_for_pathmodel = function(dat) {
  dat = dat %>%
    mutate(Site = as.factor(Site),
           GENDER = as.factor(GENDER),
           RACE_ETH = as.factor(RACE_ETH),
           WORK = as.factor(WORK),
           MARRIAGE = as.factor(MARRIAGE))

  dat = dummy_cols(dat,
                   select_columns = c('Site', 'GENDER', 'RACE_ETH',
                                      'WORK', 'MARRIAGE'),
                   remove_first_dummy = TRUE)

  dat = dat %>% rename(
    GENDER_Female = GENDER_2,
    RACE_Black = RACE_ETH_2,
    RACE_American_Indian = RACE_ETH_3,
    RACE_Asian = RACE_ETH_4,
    RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander = RACE_ETH_5,
    RACE_Other_Non_Hispanic = RACE_ETH_6,
    RACE_Hispanic = RACE_ETH_7,
    WORK_Working = WORK_1,
    MARRIAGE_Living_with_a_partner = MARRIAGE_2,
    MARRIAGE_Separated = MARRIAGE_3,
    MARRIAGE_Divorced = MARRIAGE_4,
    MARRIAGE_Widowed = MARRIAGE_5,
    MARRIAGE_Never_married = MARRIAGE_6
  )
  dat
}
