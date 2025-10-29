# Clear environment
rm(list = ls())

# Load libraries
library(pwrss)
library(dplyr)
library(ggplot2)

######## Power Analysis for Multiple Regression ######## 
# https://cran.r-project.org/web/packages/pwrss/vignettes/examples.html#3_Linear_Regression_(F_and_t_Tests)

# effect size for N = 2658 to reach 80% power
# partial r2 = 1/(1+1/f2), assuming no correlation between beta1 and predictors, so essentially beta1 = sqrt(partial r2)
pwrss.t.reg(beta1 = 0.05428048, beta0 = 0, k = 26, alpha = 0.05, power= .8, alternative = "not equal")

# get a list of betas and their corresponding power
betas = seq(0.03, 0.1, by=0.001)
powers = sapply(betas, function(b) pwrss.t.reg(beta1 = b, beta0 = 0, k = 26, alpha = 0.05, n = 2658, alternative = "not equal")$power)

# draw plot
cbind(betas, powers) %>%
  as.data.frame() %>%
  ggplot(aes(x=betas, y=powers)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype="dashed", color = "red") +
  geom_vline(xintercept = 0.05428048, linetype="dashed", color = "blue") +
  annotate("text", x = 0.07, y = 0.82, label = "80% Power", color = "red") +
  annotate("text", x = 0.056, y = 0.6, label = "Effect size: 0.054", color = "blue", angle=90) +
  labs(title="Power Analysis for Multiple Regression",
       subtitle="26 Predictors (1 Key + 25 Controls), N = 2658, Alpha = 0.05",
       x="Standardized Regression Coefficient",
       y="Power") +
  theme_classic()

ggsave('../figs/power_bivariate.png', width=6, height=4, dpi=300)

######## Power Analysis for Path Model ######## 
#https://www.regorz-statistik.de/blog/power_path_analysis_sempower.html
library(semPower)

# critical effect size for N = 2658 to reach 80% power
# the effect of interest is the difference in slopes
b1 = 0.01
critical_eff = 0.077 # critical effect
b2 = b1 + critical_eff

Beta <- matrix(c(
  #   IV   DV1  DV2   <- predictors (columns)
  c( 0,   0,   0),   # IV  = 0*IV + 0*DV1 + 0*DV2
  c(b1,   0,   0 ),   # DV1 = b1*IV  + 0*DV1 + 0*DV2
  c(b2,   0,   0 )    # DV2 = b2*IV  + 0*DV1 + 0*DV2
), byrow = TRUE, ncol = 3)

powerPath <- semPower.powerPath(
  type = 'posthoc',
  N = 2658,
  alpha = .05, 
  Beta = Beta,
  nullEffect = 'betaX = betaZ', # testing for equality of paths
  nullWhich = list(c(2,1), c(3,1)),  # equate IV→DV1 and IV→DV2
  Lambda = diag(3) # number of variables
)

summary(powerPath)

# inspect models to make sure they are correctly specified
p_calc <- powerPath

h1_model <- lavaan::sem(p_calc$modelH1, sample.cov = p_calc$Sigma,
                        sample.nobs = 2658, sample.cov.rescale = FALSE)

h0_model <- lavaan::sem(p_calc$modelH0, sample.cov = p_calc$Sigma,
                        sample.nobs = 2658, sample.cov.rescale = FALSE)

summary(h1_model, standardized = T)
summary(h0_model, standardized = T)

# vary effect size and obtain power
eff = seq(0.03, 0.15, by=0.001)
power = c()
for (e in eff) {
  b1 = 0.01
  b2 = b1 + e
  Beta <- matrix(c(
    #   IV   DV1  DV2   <- predictors (columns)
    c( 0,   0,   0),   # IV  = 0*IV + 0*DV1 + 0*DV2
    c(b1,   0,   0 ),   # DV1 = b1*IV  + 0*DV1 + 0*DV2
    c(b2,   0,   0 )    # DV2 = b2*IV  + 0*DV1 + 0*DV2
  ), byrow = TRUE, ncol = 3)
  
  powerPath <- semPower.powerPath(
    type = 'posthoc',
    N = 2669,
    alpha = .05, 
    Beta = Beta,
    nullEffect = 'betaX = betaZ',
    nullWhich = list(c(2,1), c(3,1)),  # equate IV→DV1 and IV→DV2
    Lambda = diag(3) # number of variables
  )
  
  power = c(power, powerPath$power)
}

# plot
cbind(eff, power) %>%
  as.data.frame() %>%
  ggplot(aes(x=eff, y=power)) +
  geom_line() +
  geom_hline(yintercept = 0.8, linetype="dashed", color = "red") +
  geom_vline(xintercept = critical_eff, linetype="dashed", color = "blue") +
  annotate("text", x = 0.12, y = 0.83, label = "80% Power", color = "red") +
  annotate("text", x = critical_eff+0.005, y = 0.6, label = paste0("Effect size: ", toString(critical_eff)), color = "blue", angle=90) +
  labs(title="Power Analysis for Path Model",
       subtitle="1 Predictor, 2 DVs, N = 2658, Alpha = 0.05",
       x="Difference in Standardized Regression Coefficient",
       y="Power") +
  theme_classic()
ggsave('../figs/power_multivariate.png', width=6, height=4, dpi=300)


# one-sample t-test
stats::power.t.test(n = 2658, sd=1, sig.level = 0.05, power = .8, type = 'one.sample', alternative = "one.sided")

