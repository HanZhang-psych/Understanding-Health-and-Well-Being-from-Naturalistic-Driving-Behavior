###############################################################################
# plot_fig3_dominance.R
#
#   * Show only IVs that entered a multivariate (path) model in the Y1
#     exploratory step (i.e., IVs in y1_multivariate_modelspec.csv).
#   * Tile fill = Year 2 standardized bivariate beta (matches the path model's
#     regression coefficient up to listwise-vs-rowwise differences in the
#     sample; using the bivariate beta here keeps fill consistent with Fig 2).
#   * Outlined tile = the candidate DV chosen in Y1 for that IV.
#   * Crossed-out tile = the candidate DV's relationship dominated the
#     relationship of that DV in YEAR 2 (i.e., replicated == TRUE in the
#     y2 multivariate confirmatory step).
#   * Specificity score (Panel B) = % of "against" DVs significantly dominated
#     in Y2, out of the 11 possible non-candidate DVs.

###############################################################################

# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ---- libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scico)
source('funcs.R')   # ivs, dvs, recode lists

# ---- load year-2 confirmatory results ---------------------------------------
spec      <- read.csv('output/y1_multivariate_modelspec.csv')   # IV -> candidate
contrasts <- read.csv('output/y2_multivariate_contrasts_sig.csv') # Y2 sig + replicated
y2_biv    <- read.csv('output/y2_bivariate_all.csv')            # tile fill

# IVs included in the multivariate model (from the Y1 spec)
iv_used <- unique(spec$IV)
cat(sprintf('IVs entering multivariate path model in Y1: %d\n', length(iv_used)))

# ---- assemble tile data frame (one row per IV x DV) -------------------------
# Tile fill comes from Y2 bivariate beta. We pull only the IVs that were in
# the multivariate model.
tile_df <- y2_biv %>%
  filter(IV %in% iv_used) %>%
  select(IV, DV, beta) %>%
  mutate(IV_label = recode(IV, !!!driving_var_recode_list),
         DV_label = recode(DV, !!!health_var_recode_list))

# Candidate flag: TRUE for the candidate DV for that IV (one per IV)
tile_df <- tile_df %>%
  mutate(is_candidate = mapply(
    function(iv, dv) any(spec$IV == iv & spec$candidate_DV == dv),
    IV, DV))

# Dominated flag: TRUE iff (IV, candidate_DV) replicated dominance over (IV, DV)
# in Year 2. The candidate cell itself is never "dominated by itself".
dom_y2 <- contrasts %>%
  filter(replicated) %>%
  select(IV, against_DV)

tile_df <- tile_df %>%
  mutate(dominated_y2 = mapply(
    function(iv, dv) any(dom_y2$IV == iv & dom_y2$against_DV == dv),
    IV, DV))

# ---- ordering ---------------------------------------------------------------
# Rows: order IVs by their Y2 specificity score (Panel B) so the most specific
# IVs sit at the top of Panel A.
spec_score <- tile_df %>%
  group_by(IV, IV_label) %>%
  summarise(n_dom_y2 = sum(dominated_y2),
            specificity = n_dom_y2 / 10,   # 10 = total possible "against" DVs
            .groups = 'drop') %>%
  arrange(specificity)
iv_order <- spec_score$IV_label
tile_df$IV_label <- factor(tile_df$IV_label, levels = iv_order)
spec_score$IV_label <- factor(spec_score$IV_label, levels = iv_order)

# Columns: keep the canonical health-variable order from funcs.R
dv_order <- unname(health_var_recode_list[dvs])
tile_df$DV_label <- factor(tile_df$DV_label, levels = dv_order)

# remove life satisfaction
tile_df <- tile_df %>% filter(DV_label != 'Life Satisfaction')

# ---- Panel A: dominance matrix ----------------------------------------------
fill_range <- max(abs(tile_df$beta), na.rm = TRUE)

panelA <- ggplot(tile_df, aes(x = DV_label, y = IV_label)) +
  geom_tile(aes(fill = beta), linewidth = 0.5) +
  # Y2-replicated dominance: cross-out (two diagonals)
  ggpattern::geom_tile_pattern(data = filter(tile_df, dominated_y2),
                               aes(
                                 x=DV_label, y=IV_label,
                                 pattern='crosshatch'), 
                               width = 0.8,
                               height = 0.8,
                               pattern_color='darkgrey',
                               pattern_fill = 'darkgrey',
                               pattern_angle = 45,
                               pattern_density = 0.1,
                               pattern_size = 0.6,
                               pattern_spacing = 0.02,
                               fill=NA,
                               color=NA) +
  # candidate outline
  geom_tile(data = filter(tile_df, is_candidate),
            color = 'black', fill = NA, linewidth = 1.5) +
  scale_fill_scico(palette = 'vik', limits = c(-fill_range, fill_range),
                   midpoint = 0, direction = 1,
                   name =  'Standardized regression coefficient (\u03b2)') +
  scale_x_discrete(position = 'top') +
  labs(x = NULL, y = NULL, title = 'A. Dominance matrix') +
  theme_minimal(base_size = 15) +
  theme(axis.text.x      = element_text(angle = 45, hjust = 0, vjust = 0),
        panel.grid       = element_blank(),
        plot.title       = element_text(face = 'bold'),
        legend.position  = c(0.5, -0.06),
        legend.direction ='horizontal',
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width = unit(1.0, 'cm'))

# ---- Panel B: Y2 specificity score ------------------------------------------
panelB <- ggplot(spec_score, aes(x = specificity, y = IV_label)) +
  geom_col(fill = 'grey45', width = 0.7) +
  geom_text(aes(label = sprintf('%.0f%%', 100 * specificity)),
            hjust = -0.2, size = 4.0) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1.15), expand = c(0, 0)) +
  labs(x = 'Specificity score', y = NULL,
       title = 'B. Specificity score') +
  theme_classic(base_size = 15) +
  theme(plot.title  = element_text(face = 'bold'),
        axis.text.y = element_blank(),     # share y axis with Panel A
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_line(color = 'grey92'))

# ---- combine and save -------------------------------------------------------
fig3 <- panelA + panelB + plot_layout(widths = c(3.0, 1.0))
print(fig3)

ggsave('../figs/fig3_dominance.png', fig3,
       width = 12, height = 7, dpi = 800, bg = 'white')
