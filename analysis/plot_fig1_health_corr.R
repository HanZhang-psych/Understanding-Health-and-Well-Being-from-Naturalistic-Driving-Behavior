###############################################################################
# plot_fig1_health_corr.R
#
# Figure 1 (revision): correlation matrix of health and well-being measures.
#
# Upper triangle = Year 1 (driving Interval 0 -> health Interval 1)
# Lower triangle = Year 2 (driving Interval 1 -> health Interval 2)
###############################################################################

# Clear environment
rm(list = ls())

# set working directory to file location (RStudio convention used elsewhere)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- libraries --------------------------------------------------------------
library(tidyverse)
library(scico)        # diverging palette ('vik' or 'roma') — matches Figs 2-3
source('funcs.R')     # dvs, health_var_recode_list

# ---- load year-specific subject-level data ----------------------------------
y1 <- read.csv('../data/y1_subject_level.csv')
y2 <- read.csv('../data/y2_subject_level.csv')

# DV order: match the order
dv_levels <- dvs
dv_labels <- unname(health_var_recode_list[dv_levels])

# ---- correlation matrices ---------------------------------------------------
# pairwise.complete.obs mirrors the row-wise complete-case rule of lm()
cor_y1 <- cor(y1[, dv_levels], use = 'pairwise.complete.obs')
cor_y2 <- cor(y2[, dv_levels], use = 'pairwise.complete.obs')

# ---- assemble long-format tibble for tiled plot -----------------------------
# upper.tri / lower.tri are taken with respect to the matrix as displayed.
# In ggplot's default y axis the matrix is flipped vs. matrix indexing, so we
# build the long form explicitly with explicit row/col labels.
build_long <- function(m, source_label) {
  m %>%
    as.data.frame() %>%
    rownames_to_column('row') %>%
    pivot_longer(-row, names_to = 'col', values_to = 'r') %>%
    mutate(source = source_label)
}

long_y1 <- build_long(cor_y1, 'Y1')
long_y2 <- build_long(cor_y2, 'Y2')

# index DVs so we can pick triangle by integer comparison
dv_idx <- setNames(seq_along(dv_levels), dv_levels)

combined <- bind_rows(long_y1, long_y2) %>%
  mutate(row_i = dv_idx[row],
         col_i = dv_idx[col]) %>%
  # keep Y1 only in upper triangle (col > row), Y2 only in lower triangle
  # (col < row); diagonal handled separately
  filter((source == 'Y1' & col_i >  row_i) |
         (source == 'Y2' & col_i <  row_i))

plot_df <- combined %>%
  mutate(row_label = factor(health_var_recode_list[row], levels = dv_labels),
         col_label = factor(health_var_recode_list[col], levels = dv_labels),
         r_text    = sprintf('%.2f', r))

# ---- plot -------------------------------------------------------------------
p <- ggplot(plot_df, aes(x = col_label, y = row_label, fill = r)) +
  geom_tile(color = 'white', linewidth = 0.5) +
  geom_text(aes(label = r_text), size = 3.0, fontface='bold') +
  scale_fill_scico(palette = 'vik', limits = c(-1, 1), midpoint = 0,
                   direction = 1, name = 'Correlation') +
  # invert y so the diagonal runs top-left to bottom-right (matrix convention)
  scale_y_discrete(limits = rev(dv_labels)) +
  scale_x_discrete(position = 'top') +
  # corner annotations to make the triangle assignment obvious to readers
  annotate('text', x = length(dv_labels)+2, y = length(dv_labels),
           label = 'Year 1', fontface = 'italic', size = 4.5, hjust = 1) +
  annotate('text', x = 0.5, y = 0,
           label = 'Year 2', fontface = 'italic', size = 4.5, hjust = 0) +
  coord_fixed(clip = "off")+
  theme_minimal(base_size = 15) +
  theme(axis.text.x      = element_text(angle = 45, hjust = 0, vjust = 0),
        axis.title       = element_blank(),
        panel.grid       = element_blank(),
        legend.position  = c(1, 0.5),
        legend.key.height = unit(1.2, 'cm'),
        plot.margin = margin(0.1, 2, 0.1, 0.1, 'cm'))

print(p)

# ---- save -------------------------------------------------------------------
ggsave('../figs/fig1_health_corr.png', p,
       width = 9, height = 9, dpi = 800, bg = 'white')
