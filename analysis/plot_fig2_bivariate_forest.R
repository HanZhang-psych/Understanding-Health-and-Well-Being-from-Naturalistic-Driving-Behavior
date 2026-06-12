###############################################################################
# plot_fig2_bivariate_forest.R
#
# forest plot of the bivariate driving -> health effects
# that were significant in Year 1 (exploratory), with Year 2 (confirmation) shown
# alongside.

###############################################################################

# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- libraries --------------------------------------------------------------
library(tidyverse)
source('funcs.R')     # ivs, dvs, recode lists

# ---- load discovery -> replication bivariate table --------------------------
b <- read.csv('output/y2_bivariate_sig.csv')

# Tidy labels using the project recode lists
b <- b %>%
  mutate(IV_label = recode(IV, !!!driving_var_recode_list),
         DV_label = recode(DV, !!!health_var_recode_list))

# reorder factor levels
b$IV_label <- factor(b$IV_label, levels = unname(driving_var_recode_list[ivs]))
b$DV_label <- factor(b$DV_label, levels = unname(health_var_recode_list[dvs]))

# ---- long-format for two points per row -------------------------------------
long <- bind_rows(
  b %>% transmute(IV_label, DV_label,
                  year = 'Year 1',
                  beta = y1_beta, lo = y1_CI_low,  hi = y1_CI_high,
                  replicated),
  b %>% transmute(IV_label, DV_label,
                  year = 'Year 2',
                  beta = y2_beta, lo = y2_CI_low,  hi = y2_CI_high,
                  replicated)
) %>%
  mutate(year = factor(year, levels = c('Year 1', 'Year 2')),
         fill_year = factor(ifelse(replicated, as.character(year), NA),
                            levels = c('Year 1', 'Year 2'))) %>%
  filter(DV_label != 'Life Satisfaction') # since we are intetested in specific health domains here

# ---- color and shape choices ------------------------------------------------
year_colors <- c('Year 1' = '#377eb8',   # blue
                 'Year 2' = '#e41a1c')   # red
year_shapes <- c('Year 1' = 21,           # filled circle (when replicated)
                 'Year 2' = 24)           # filled triangle (when replicated)

# ---- plot -------------------------------------------------------------------
dodge <- position_dodge(width = 0.55)

p <- ggplot(long, aes(x = beta, y = IV_label, color = year)) +
  geom_vline(xintercept = 0, color = 'grey60', linetype = 'dashed') +
  geom_linerange(aes(xmin = lo, xmax = hi), position = dodge, linewidth = 0.7) +
  geom_point(aes(shape = year, fill = fill_year),
             position = dodge, size = 2.6, stroke = 0.7) +
  scale_y_discrete(limits = rev) + 
  scale_color_manual(values = year_colors, name = NULL) +
  scale_fill_manual(values  = year_colors, na.value = 'white',
                    guide  = 'none') +
  scale_shape_manual(values = year_shapes, name = NULL) +
  facet_wrap(~ DV_label, scales = 'free_x', ncol = 4) +
  coord_cartesian(xlim = c(-0.15, 0.15)) +
  labs(x = 'Standardized regression coefficient (\u03b2)',
       y = '') +
  theme_classic(base_size = 15) +
  theme(strip.background = element_rect(fill = 'grey92', color = NA),
        strip.text       = element_text(face = 'bold'),
        legend.position  = 'top',
        panel.grid.major.y = element_line(color = 'grey95'),
        plot.caption     = element_text(hjust = 0, size = 9, color = 'grey30'))

print(p)

# ---- save -------------------------------------------------------------------
ggsave('../figs/fig2_bivariate_forest.png', p,
       width = 12, height = 8, dpi = 800, bg = 'white')
