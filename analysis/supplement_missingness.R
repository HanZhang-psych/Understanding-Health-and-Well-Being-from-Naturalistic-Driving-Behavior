# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(tidyverse)
library(table1)
source('funcs.R')

# ---- Load and prepare -------------------------------------------------------
Y1_PATH <- '../data/y1_subject_level.csv'
Y2_PATH <- '../data/y2_subject_level.csv'

y1 <- read.csv(Y1_PATH, stringsAsFactors = FALSE)
y2 <- read.csv(Y2_PATH, stringsAsFactors = FALSE)

y1$Year <- "Year 1"
y2$Year <- "Year 2"

cat_demos <- c('Site', 'GENDER', 'RACE_ETH', 'WORK', 'MARRIAGE')
num_demos <- c('Age', 'EDUCATION', 'INCOME')

prepare <- function(df) {
  for (v in c(ivs, dvs, num_demos)) df[[v]] <- as.numeric(df[[v]])
  for (v in cat_demos)              df[[v]] <- factor(df[[v]])
  for (v in ivs) label(df[[v]]) <- unname(driving_var_recode_list[v])
  for (v in dvs) label(df[[v]]) <- unname(health_var_recode_list[v])
  label(df$Age)       <- "Age (years)"
  label(df$EDUCATION) <- "Education level (ordinal)"
  label(df$INCOME)    <- "Household income level (ordinal)"
  label(df$Site)      <- "Site"
  label(df$GENDER)    <- "Gender"
  label(df$RACE_ETH)  <- "Race/Ethnicity"
  label(df$WORK)      <- "Currently working"
  label(df$MARRIAGE)  <- "Marital status"
  levels(df$Site) = c('Cooperstown, New York', 'Baltimore, Maryland', 'Denver, Colorado', 'La Jolla, California', 'Ann Arbor, Michigan')
  levels(df$GENDER) = c('Male','Female')
  levels(df$RACE_ETH) = c('White, Non-Hispanic', 'Black, Non-Hispanic','American Indian','Asian','Alaska Native, Native Hawaiian, Pacific Islander','Other, Non-Hispanic','Hispanic')
  levels(df$WORK) = c('No', 'Yes')
  levels(df$MARRIAGE) = c('Married', 'Living with a partner', 'Separated', 'Divorced', 'Widowed', 'Never married')
  df
}
y1 <- prepare(y1)
y2 <- prepare(y2)

stacked <- bind_rows(y1, y2) %>% prepare()

# ---- Table S1: per-variable descriptives + missingness ----------------
f1 <- as.formula(paste0(
  "~ ", paste(c(ivs, dvs), collapse = " + "),
  " | Year"
))

table1(f1, data = stacked, overall = FALSE, render.continuous=c(.="Mean (SD)"))


# ---- Table S2: complete vs any-missing -------------------------------
# "Complete" subjects have no missing values on any of the 24 driving variables, 12 health outcomes, or 8 demographic covariates. 
# "Any missing" subjects have at least one missing value on that set. 
# Numeric variables (Age, Education, Income): Welch's t-test. 
# Categorical variables (Site, Gender, Race/Ethnicity, Currently working, Marital status): chi-square test of independence.

flag_complete <- function(df) {
  all_vars <- c(ivs, dvs, num_demos, cat_demos)
  miss_mat <- sapply(all_vars, function(v) {
    x <- df[[v]]
    if (is.factor(x) || is.character(x)) is.na(x) | as.character(x) == ""
    else is.na(x)
  })
  rowSums(miss_mat) == 0
}

y1$Group <- factor(ifelse(flag_complete(y1), "Complete", "Any missing"),
                   levels = c("Complete", "Any missing"))
y2$Group <- factor(ifelse(flag_complete(y2), "Complete", "Any missing"),
                   levels = c("Complete", "Any missing"))

format_p_short <- function(p) {
  if (is.na(p)) return(intToUtf8(0x2014))
  if (p < .001) return("&lt; .001")
  sprintf("%.3f", p)
}
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(seq_along(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- tryCatch(t.test(y ~ g)$p.value, error = function(e) NA)
  } else {
    p <- tryCatch(suppressWarnings(chisq.test(table(y, g))$p.value),
                  error = function(e) NA)
  }
  format_p_short(p)
}

f2 <- ~ Age + EDUCATION + INCOME + Site + GENDER + RACE_ETH + WORK + MARRIAGE | Group

# year 1
table1(f2, data = y1, overall = FALSE, extra.col = list(`p-value` = pvalue), render.continuous=c(.="Mean (SD)"), render.missing = NULL)

# year 2
table1(f2, data = y2, overall = FALSE, extra.col = list(`p-value` = pvalue), render.continuous=c(.="Mean (SD)"), render.missing = NULL)