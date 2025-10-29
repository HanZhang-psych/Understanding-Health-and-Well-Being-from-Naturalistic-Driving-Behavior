library(Polychrome)
library(fastDummies)

# define variables
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
dvs = c( 'LIFESATISFACTION', 'cog','phys','fati','socialrole','iso','info','emo','ins','dep','anx','ang')
demos = c("Site", "Age","GENDER" ,"RACE_ETH", "EDUCATION" ,"INCOME", "WORK", "MARRIAGE")

driving_var_recode_list = c('DaysDriving'='Number of Days Driving',
                            'Miles_n'='Total Miles',
                            'Trips' = 'Total Trips',
                            'TripMinutes_n'='Total Trip Minutes',
                            'TripChains'='Total Trip Chains',
                            'MilesPerTrip_n'= 'Miles Per Trip', 
                            'MinutesPerTrip_n'='Minutes Per Trip',
                            'MilesPerChain_n'='Miles Per Chain', 
                            'MinutesPerChain_n' = 'Minutes Per Chain',  
                            'Average_speed'='Travel Speed (MPH)', 
                            'LeftTurnCount'='Number of Left Turns',  
                            'RightToLeftTurnRatio_n'='Right to Left Turn Ratio', 
                            'TripsAMPeak'='Number of Trips AM Rush Hour',
                            'PercentTripsAMPeak_n'='Percentage of Trips AM Rush Hour',
                            'TripsPMPeak' = 'Number of Trips PM Rush Hour',
                            'PercentTripsPMPeak_n'='Percentage of Trips PM Rush Hour',
                            'TripsAtNight'='Number of Trips at Night',
                            'PercentTripsAtNight_n'='Percentage of Trips at Night',
                            'TripsLt15Miles'='Number of Trips < 15 Miles',
                            'PercentDistLt15Miles_n'='Percentage of Trips < 15 Miles',
                            'TripsVgt60'= 'Number of Trips > 60 MPH',
                            'PercentTripsVgt60_n'='Percentage of Trips > 60 MPH', 
                            'SpeedingPer1000Miles'='Speeding Events Per 1000 Miles', 
                            'DecelerationPer1000Miles' = 'Hard-Braking Events Per 1000 Miles')


health_var_recode_list = c('LIFESATISFACTION'='Life Satisfaction',
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
                      
fit_lm = function(dat, IV_name, DV_name, COV_name = NULL, scale = TRUE) {

  # Create IV, DV, and optional covariate columns
  df_lm = dat %>%
    mutate(IV = !!sym(IV_name),
           DV = !!sym(DV_name)) %>%
    select(XID, IV, DV, Site, Age, GENDER, RACE_ETH, EDUCATION, INCOME, WORK, MARRIAGE, all_of(COV_name)) %>%
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
  
  # Optional: scale numeric variables
  if (scale) {
    df_lm$IV = scale(df_lm$IV, center = TRUE, scale = TRUE)[,1]
    df_lm$DV = scale(df_lm$DV, center = TRUE, scale = TRUE)[,1]
    df_lm$Age = scale(df_lm$Age, center = TRUE, scale = TRUE)[,1]
    df_lm$EDUCATION = scale(df_lm$EDUCATION, center = TRUE, scale = TRUE)[,1]
    df_lm$INCOME = scale(df_lm$INCOME, center = TRUE, scale = TRUE)[,1]
    
    # Scale numeric covariates automatically
    if (!is.null(COV_name)) {
      for (cov in COV_name) {
        if (is.numeric(df_lm[[cov]])) {
          df_lm[[cov]] = scale(df_lm[[cov]], center = TRUE, scale = TRUE)[,1]
        }
      }
    }
  }
  
  # Build regression formula dynamically
  base_covariates <- c("Age", "EDUCATION", "INCOME", "Site", "GENDER", "RACE_ETH", "WORK", "MARRIAGE")
  all_covariates <- c(base_covariates, COV_name)  # combine default + user-specified
  
  formula_str <- paste("DV ~ IV +", paste(all_covariates, collapse = " + "))
  model_formula <- as.formula(formula_str)
  
  # Fit the model
  model = lm(model_formula, data = df_lm)
  
  # Extract parameters
  params = as.data.frame(parameters(model))
  
  return(list(model = model, params = params))
}


fit_lm_cor = function(dat, IV_name, DV_name, cormat, COV_name=NULL) {
  demo_vars = c('Age',
                'Site_JHSPH', 'Site_UCDENVER', 'Site_UCSD', 'Site_UMTRI',
                'EDUCATION', 'INCOME', 'GENDER_Female', 
                'RACE_Black', 'RACE_American_Indian', 'RACE_Asian',
                'RACE_Alaska_Native_Native_Hawaiian_Pacific_Islander',
                'RACE_Other_Non_Hispanic',
                'RACE_Hispanic',
                'WORK_Working',
                'MARRIAGE_Living_with_a_partner',
                'MARRIAGE_Separated',
                'MARRIAGE_Divorced',
                'MARRIAGE_Widowed',
                'MARRIAGE_Never_married')
  
  # subsetting cormat
  cormat_sub = cormat[c(IV_name, COV_name, demo_vars, DV_name), c(IV_name, COV_name, demo_vars, DV_name)]
  
  # number of vars
  nvars=nrow(cormat_sub)
  n = 2658
  k = nvars - 1
  
  Rxx = cormat_sub[1:(nvars-1), 1:(nvars-1)]
  Rxy = cormat_sub[1:(nvars-1), nvars]
  
  beta = solve(Rxx) %*% Rxy
  R2 = as.numeric(t(Rxy) %*% beta)
  se2 = 1-R2
  cov_beta = se2 * solve(Rxx) / (n - k - 1)
  se <- sqrt(diag(cov_beta))
  t_values = beta / se
  df_error = n - k - 1
  p_values = 2 * pt(-abs(t_values), df = df_error)
  ci_low = beta - qt(0.975, df = df_error) * se
  ci_high = beta + qt(0.975, df = df_error) * se
  results = data.frame(
    Parameter = rownames(Rxx),
    beta = as.numeric(beta),
    se = as.numeric(se),
    CI_low = as.numeric(ci_low),
    CI_high = as.numeric(ci_high),
    t = as.numeric(t_values),
    df_error = df_error,
    p = as.numeric(p_values)
  )
  return(results)
}

# not used
fit_lavaan = function(dat, IV_name, DV_name, scale=T) {
  
  # mutate to create IV and DV columns
  df_lavaan = dat %>%
    mutate(IV = !!sym(IV_name),
           DV = !!sym(DV_name)) %>%
    select(XID, IV, DV, Site, Age, GENDER, RACE_ETH, EDUCATION, INCOME, WORK, MARRIAGE) %>%
    mutate(XID = as.factor(XID),
           IV = as.numeric(IV),
           DV = as.numeric(DV),
           Site = as.factor(Site),
           Age = as.numeric(Age),
           GENDER = as.factor(GENDER),
           RACE_ETH = as.factor(RACE_ETH),
           EDUCATION = as.numeric(EDUCATION),
           INCOME = as.numeric(INCOME),
           WORK = as.factor(WORK),
           MARRIAGE = as.factor(MARRIAGE))
  
  if (scale) {
    # z-score the IV and DV
    df_lavaan$IV = scale(df_lavaan$IV, center = T, scale = T)[,1] # z-score the IV
    df_lavaan$DV = scale(df_lavaan$DV, center = T, scale = T)[,1] # z-score the DV
    df_lavaan$Age = scale(df_lavaan$Age, center = T, scale = T)[,1] # z-score Age
    df_lavaan$EDUCATION = scale(df_lavaan$EDUCATION, center = T, scale = T)[,1] # z-score EDUCATION
    df_lavaan$INCOME = scale(df_lavaan$INCOME, center = T, scale = T)[,1] # z-score INCOME
  }
  
  # create dummy variables
  df_lavaan = df_lavaan %>% 
    dummy_cols(select_columns = c('Site','GENDER','RACE_ETH','WORK','MARRIAGE'), remove_first_dummy = TRUE)

  # fit the model
  lavaan.bi = 'DV ~ IV + Age + EDUCATION + INCOME + Site_JHSPH + Site_UCDENVER + Site_UCSD + Site_UMTRI + GENDER_2 + RACE_ETH_2 + RACE_ETH_3 + RACE_ETH_4 + RACE_ETH_5 + RACE_ETH_6 + RACE_ETH_7 + WORK_1 + MARRIAGE_2 + MARRIAGE_3 + MARRIAGE_4 + MARRIAGE_5 + MARRIAGE_6'
  model = lavaan::sem(lavaan.bi, data = df_lavaan, estimator='MLM')

  # get params
  params = as.data.frame(parameters(model)) %>% filter(Operator=='~')
  
  return(list(model = model, params = params))
}


