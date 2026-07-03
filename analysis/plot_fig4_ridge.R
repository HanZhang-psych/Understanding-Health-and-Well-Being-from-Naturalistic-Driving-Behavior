###############################################################################
# plot_fig4_ridge.R
#
# Panel A: Per-outcome MSE for four models (baseline, demo, driving, full).
#          Point estimate = mean per-subject squared error; error bars = 95%
#          bootstrap CI. 
#
# Panel B: Paired-contrast forest. Point estimate = mean paired difference in
#          per-subject squared error (sq_err_a - sq_err_b); 
#          Significance markers come from the FDR-BY-adjusted permutation p
#          (sign-flip permutation test of paired differences against zero,
#          one-tailed in the direction of better prediction by the first model).
###############################################################################

# Clear environment ONLY if no caller has pre-set the knobs below. This lets
# supplement_plot_enet.R (and any other reuser) override
# PER_SUBJECT_DIR / SUMMARY_FILE / OUT_FILE before sourcing this script.
if (!exists('PER_SUBJECT_DIR')) {
  rm(list = ls())
}

# set working directory to file location.
# Skip when invoked by another R script that has already set up the cwd.
if (!exists('PER_SUBJECT_DIR')) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# ---- libraries --------------------------------------------------------------
library(tidyverse)
library(patchwork)
source('funcs.R')   # dvs, health_var_recode_list

# ---- knobs the elastic-net supplement script overrides ----------------------
if (!exists('PER_SUBJECT_DIR')) PER_SUBJECT_DIR <- 'output/ridge_errors/'
if (!exists('SUMMARY_FILE'))    SUMMARY_FILE    <- 'output/ridge_summary.csv'
if (!exists('OUT_FILE'))        OUT_FILE        <- '../figs/fig4_ridge.png'
if (!exists('B_BOOT'))          B_BOOT          <- 1000   # bootstrap replicates
if (!exists('SEED'))            SEED            <- 123

set.seed(SEED)

# ---- load per-subject squared errors ----------------------------------------
models <- c('baseline', 'demo', 'driving', 'full')
model_labels <- c(baseline = 'Baseline',
                  demo     = 'Demographics only',
                  driving  = 'Driving only',
                  full     = 'Full (driving + demo)')
model_colors <- c('Baseline'              = '#999999',
                  'Demographics only'     = '#1f78b4',
                  'Driving only'          = '#33a02c',
                  'Full (driving + demo)' = '#e31a1c')
model_order  <- unname(model_labels)

per_sub_list <- list()
for (dv in dvs) {
  fp <- file.path(PER_SUBJECT_DIR, sprintf('%s_per_subject.csv', dv))
  if (!file.exists(fp)) {
    stop(sprintf('Missing per-subject file %s. Re-run prediction_model.ipynb to regenerate.', fp))
  }
  d <- read.csv(fp)
  d$dv <- dv
  per_sub_list[[dv]] <- d
}
per_sub_wide <- bind_rows(per_sub_list)

# ---- long format for Panel A: one row per (subject, dv, model) --------------
panelA_long <- per_sub_wide %>%
  select(dv, subject,
         sq_err_baseline, sq_err_demo, sq_err_driving, sq_err_full) %>%
  pivot_longer(starts_with('sq_err_'),
               names_to  = 'model_code',
               values_to = 'sq_err',
               names_prefix = 'sq_err_') %>%
  mutate(model    = factor(model_labels[model_code], levels = model_order),
         DV_label = unname(health_var_recode_list[dv]))

# DV order: by raw baseline MSE descending so "hard" outcomes sit at the top.
# Computed analytically (not bootstrapped) since this is just for axis order.
dv_order <- panelA_long %>%
  filter(model_code == 'baseline') %>%
  group_by(DV_label) %>%
  summarise(m = mean(sq_err, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(m)) %>%
  pull(DV_label)
panelA_long$DV_label <- factor(panelA_long$DV_label, levels = dv_order)

# ---- Panel A: pointranges via stat_summary ----------------------------------
dodge <- position_dodge(width = 0.6)

panelA <- ggplot(panelA_long,
                 aes(x = sq_err, y = DV_label, color = model, shape = model)) +
  stat_summary(fun.data = 'mean_cl_boot',
               fun.args = list(B = B_BOOT, conf.int = 0.95),
               geom = 'linerange',
               position = dodge, linewidth = 0.7,
               show.legend = FALSE) +
  stat_summary(fun = mean, geom = 'point',
               position = dodge, size = 2.4) +
  scale_color_manual(values = model_colors, name = NULL) +
  scale_shape_manual(values = c(15, 16, 17, 18), name = NULL) +
  scale_y_discrete(limits = rev(dv_order)) +
  labs(x = 'Mean squared error', y = NULL,
       title = 'A. Model prediction errors') +
  theme_classic(base_size = 13) +
  theme(plot.title         = element_text(face = 'bold'),
        plot.subtitle      = element_text(size = 9, color = 'grey30'),
        legend.position    = 'top',
        panel.grid.major.y = element_line(color = 'grey95'))

# ---- Panel B: paired-difference forest --------------------------------------
contrast_specs <- tibble(
  contrast = c('driving_vs_baseline',
               'demo_vs_baseline',
               'full_vs_demo',
               'full_vs_driving'),
  label    = c('Driving \u2013 Baseline',
               'Demographics \u2013 Baseline',
               'Full \u2013 Demographics',
               'Full \u2013 Driving')
)

# Compute per-subject paired differences (first model - second model) for each
# of the four contrasts, then go long.
panelB_long <- per_sub_wide %>%
  mutate(
    driving_vs_baseline = sq_err_driving - sq_err_baseline,
    demo_vs_baseline    = sq_err_demo    - sq_err_baseline,
    full_vs_demo        = sq_err_full    - sq_err_demo,
    full_vs_driving     = sq_err_full    - sq_err_driving
  ) %>%
  select(dv, subject,
         driving_vs_baseline, demo_vs_baseline,
         full_vs_demo,        full_vs_driving) %>%
  pivot_longer(c(driving_vs_baseline, demo_vs_baseline,
                 full_vs_demo,        full_vs_driving),
               names_to  = 'contrast',
               values_to = 'diff') %>%
  left_join(contrast_specs, by = 'contrast') %>%
  mutate(DV_label = unname(health_var_recode_list[dv]),
         DV_label = factor(DV_label, levels = dv_order),
         label    = factor(label,    levels = contrast_specs$label))

# Significance markers from FDR-BY-adjusted permutation p.
summary_df <- read.csv(SUMMARY_FILE)
annot_df <- expand_grid(contrast = contrast_specs$contrast,
                        dv = summary_df$dv) %>%
  left_join(contrast_specs, by = 'contrast') %>%
  rowwise() %>%
  mutate(
    p_perm_fdr = summary_df[summary_df$dv == dv,
                            paste0('perm_p_', contrast, '_fdr')],
    mean_diff  = summary_df[summary_df$dv == dv,
                            paste0('mean_diff_', contrast)]
  ) %>%
  ungroup() %>%
  mutate(
    is_sig = p_perm_fdr < 0.05,
    sig_label = case_when(p_perm_fdr < .001 ~ '***',
                          p_perm_fdr < .01  ~ '**',
                          p_perm_fdr < .05  ~ '*',
                          TRUE              ~ ''),
    DV_label  = unname(health_var_recode_list[dv]),
    DV_label  = factor(DV_label, levels = dv_order),
    label     = factor(label,    levels = contrast_specs$label)
  )

# merge annot_df with panelB_long to get is_sig into the main data frame for plotting.
panelB_long <- panelB_long %>%
  left_join(annot_df %>% select(dv, contrast, is_sig),
            by = c('dv', 'contrast'))

panelB <- ggplot(panelB_long, aes(x = diff, y = DV_label, color=is_sig)) +
  geom_vline(xintercept = 0, color = 'grey60', linetype = 'dashed') +
  stat_summary(fun.data = 'mean_cl_boot',
               fun.args = list(B = B_BOOT, conf.int = 0.95),
               geom = 'linerange', linewidth = 0.6) +
  stat_summary(fun = mean, geom = 'point',
               size = 2.0) +
  scale_color_manual(values = c('TRUE' = '#e31a1c', 'FALSE' = 'grey15'), guide = 'none') +
  geom_text(data = annot_df,
            aes(x = mean_diff, y = DV_label, label = sig_label),
            hjust = 0.5, size = 7.0, vjust = 0.3,
            inherit.aes = FALSE) +
  scale_y_discrete(limits = rev(dv_order)) +
  facet_wrap(~ label, nrow = 1, scales = 'free_x') +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x = 'Difference in squared errors',
       y = NULL,
       title = 'B. Model comparisons') +
  theme_classic(base_size = 13) +
  theme(plot.title         = element_text(face = 'bold'),
        plot.subtitle      = element_text(size = 9,  color = 'grey30'),
        plot.caption       = element_text(size = 8.5, color = 'grey30', hjust = 0),
        strip.background   = element_rect(fill = 'grey92', color = NA),
        strip.text         = element_text(face = 'bold'),
        panel.grid.major.y = element_line(color = 'grey95'))

# ---- combine and save -------------------------------------------------------
fig4 <- panelA / panelB + plot_layout(heights = c(1.0, 1.2))
print(fig4)

ggsave(OUT_FILE, fig4, width = 10, height = 10, dpi = 800, bg = 'white')
