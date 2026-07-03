###############################################################################
# supplement_plot_enet.R
# same as ridge version but with the elastic net CV errors instead of ridge. 
###############################################################################

# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- libraries --------------------------------------------------------------
library(tidyverse)
source('funcs.R')   # dvs

# ---- run the primary plotter with the staged inputs -------------------------
# Override the three knobs and the seed for reproducibility.
PER_SUBJECT_DIR <- 'output/enet_errors/'
SUMMARY_FILE    <- 'output/enet_summary.csv'
OUT_FILE        <- '../figs/figS_enet.png'

source('plot_fig4_ridge.R')
