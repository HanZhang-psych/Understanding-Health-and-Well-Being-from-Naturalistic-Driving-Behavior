# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries
library(tidyverse)
library(ggcorrplot)
library(viridis)
source('funcs.R')

# read data
dat = read.csv('../data/subject_level_data_for_analysis.csv')

################## Correlation Matrix ################## 
# correlation matrix of driving_data
cormat = dat %>% 
  select(all_of(ivs)) %>% 
  cor()

unlist(driving_var_recode_list)

rownames(cormat) = driving_var_recode_list[rownames(cormat)]
colnames(cormat) = driving_var_recode_list[colnames(cormat)]

corplot = ggcorrplot(cormat, lab=T, type = "upper", lab_size=3.5, legend.title='Correlation') + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.1, "cm"))
corplot

# save correlation plot
ggsave('../figs/Driving_Correlation_Matrix.png', corplot, width = 12, height = 12, bg = 'white')


# correlation matrix of health measures
# rearrange order
cormat = dat %>% 
  select(all_of(dvs)) %>% 
  cor()

rownames(cormat) = health_var_recode_list[rownames(cormat)]
colnames(cormat) = health_var_recode_list[colnames(cormat)]

corplot = ggcorrplot(cormat, lab=T, type = "upper", lab_size=3.5, legend.title='Correlation') + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        axis.ticks.x = element_line(color = "black", linewidth = 0.5),
        axis.ticks.y = element_line(color = "black", linewidth = 0.5),
        axis.ticks.length = unit(0.1, "cm"))
corplot
# save correlation plot
ggsave('../figs/Health_Correlation_Matrix.png', corplot, width = 8, height = 8, bg = 'white')
