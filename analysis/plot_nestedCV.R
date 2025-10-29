# Clear environment
rm(list = ls())

# set working directory to file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# disable scientific notation
options(scipen = 999)

# load libraries
library(tidyverse)
library(effectsize)
library(ggdist)
source('funcs.R')

USE_RAW = F

if (USE_RAW){
  path_prefix = 'cv_errors/'
} else {
  path_prefix = 'cv_errors/simulated/'
}

# Life satisfaction
life = read.csv(paste0(path_prefix, 'LIFESATISFACTION.csv')) %>% mutate(DV='Life Satisfaction')
t.life = t.test(life$diff, alternative = 'less')
t.life
d.life = cohens_d(life$diff)
d.life

# cognitve decline
cog = read.csv(paste0(path_prefix, 'cog.csv')) %>% mutate(DV='Cognitive Decline')
t.cog = t.test(cog$diff, alternative = 'less')
t.cog
d.cog = cohens_d(cog$diff)
d.cog

# physical decline
phys = read.csv(paste0(path_prefix, 'phys.csv')) %>% mutate(DV='Physical Decline')
t.phys = t.test(phys$diff, alternative = 'less')
t.phys
d.phys = cohens_d(phys$diff)
d.phys

# fatigue
fati = read.csv(paste0(path_prefix, 'fati.csv')) %>% mutate(DV='Fatigue')
t.fati = t.test(fati$diff, alternative = 'less')
t.fati
d.fati = cohens_d(fati$diff)
d.fati

# role constraints
socialrole = read.csv(paste0(path_prefix, 'socialrole.csv')) %>% mutate(DV='Role Constraints')
t.socialrole = t.test(socialrole$diff, alternative = 'less')
t.socialrole
d.socialrole = cohens_d(socialrole$diff)
d.socialrole

# social isolation
iso = read.csv(paste0(path_prefix, 'iso.csv')) %>% mutate(DV='Social Isolation')
t.iso = t.test(iso$diff, alternative = 'less')
t.iso
d.iso = cohens_d(iso$diff)
d.iso

# informational support
info = read.csv(paste0(path_prefix, 'info.csv')) %>% mutate(DV='Low Informational Support')
t.info = t.test(info$diff, alternative = 'less')
t.info
d.info = cohens_d(info$diff)
d.info

# emotional support
emo = read.csv(paste0(path_prefix, 'emo.csv')) %>% mutate(DV='Low Emotional Support')
t.emo = t.test(emo$diff, alternative = 'less')
t.emo
d.emo = cohens_d(emo$diff)
d.emo

# instrumental support
ins = read.csv(paste0(path_prefix, 'ins.csv')) %>% mutate(DV='Low Instrumental Support')
t.ins = t.test(ins$diff, alternative = 'less')
t.ins
d.ins = cohens_d(ins$diff)
d.ins

# depression
dep = read.csv(paste0(path_prefix, 'dep.csv')) %>% mutate(DV='Depression')
t.dep = t.test(dep$diff, alternative = 'less')
t.dep
d.dep = cohens_d(dep$diff)
d.dep

# anxiety
anx = read.csv(paste0(path_prefix, 'anx.csv')) %>% mutate(DV='Anxiety')
t.anx = t.test(anx$diff, alternative = 'less')
t.anx
d.anx = cohens_d(anx$diff)
d.anx

# anger
ang = read.csv(paste0(path_prefix, 'ang.csv')) %>% mutate(DV='Anger')
t.ang = t.test(ang$diff, alternative = 'less')
t.ang
d.ang = cohens_d(ang$diff)
d.ang

# combine results
res = tibble(
  DV = dvs,
  estimate = c(t.life$estimate, t.cog$estimate, t.phys$estimate, t.fati$estimate, t.socialrole$estimate, t.iso$estimate, t.info$estimate, t.emo$estimate, t.ins$estimate, t.dep$estimate, t.anx$estimate, t.ang$estimate),
  t = c(t.life$statistic, t.cog$statistic, t.phys$statistic, t.fati$statistic, t.socialrole$statistic, t.iso$statistic, t.info$statistic, t.emo$statistic, t.ins$statistic, t.dep$statistic, t.anx$statistic, t.ang$statistic),
  df = c(t.life$parameter, t.cog$parameter, t.phys$parameter, t.fati$parameter, t.socialrole$parameter, t.iso$parameter, t.info$parameter, t.emo$parameter, t.ins$parameter, t.dep$parameter, t.anx$parameter, t.ang$parameter),
  p = c(t.life$p.value, t.cog$p.value, t.phys$p.value, t.fati$p.value, t.socialrole$p.value, t.iso$p.value, t.info$p.value, t.emo$p.value, t.ins$p.value, t.dep$p.value, t.anx$p.value, t.ang$p.value),
  d = c(d.life$Cohens_d, d.cog$Cohens_d, d.phys$Cohens_d, d.fati$Cohens_d, d.socialrole$Cohens_d, d.iso$Cohens_d, d.info$Cohens_d, d.emo$Cohens_d, d.ins$Cohens_d, d.dep$Cohens_d, d.anx$Cohens_d, d.ang$Cohens_d),
) 

# recode DV name
res$DV = recode(res$DV, !!!health_var_recode_list)

# adjust p value
res$p.adj = p.adjust(res$p, method='bonferroni')

# sort
res = res %>% arrange(estimate)
res

# save
library(flextable)
flextable(res) %>% colformat_double(digits=3) %>% save_as_docx(path='../Table_S2.docx')

# plot
plot_df = rbind(life, cog, phys, fati, socialrole, iso, info, emo, ins, dep, anx, ang)

plot_df %>% 
  #gather(key, value, error_reduced, error_full) %>%
  ggplot(aes(x=DV, y=diff, color=DV)) +
  geom_hline(yintercept = 0, color='grey', linetype='dashed') +
  stat_summary(fun = 'mean', geom='point', size=3)+
  stat_summary(fun.data = 'mean_cl_boot', geom='errorbar', linewidth=1, width=0.5) +
  scale_x_discrete(limits=rev(res$DV))+
  scale_color_manual(values=DV_colors)+
  coord_flip() +
  theme_classic(base_size = 12)+
  theme(legend.position = 'none',
        panel.grid = element_blank()) +
  labs(x=NULL, y='Reduction in Prediction Error (Full - Reduced)')

ggsave("../figs/cv_results_sim.png",width=8, height=3, dpi=1200)

# mean squared error
plot_df %>% 
  summarise(mse_full = mean(error_full), mse_reduced = mean(error_reduced), diff_mse = mse_full - mse_reduced, .by=DV)
