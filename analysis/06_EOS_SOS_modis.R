# This script analyses the relationship of spring and autumn phenological dates 
# remote-sensing observations (MODIS C6 MCD12Q2 data). Outputs include ED Fig. 8.
.
# load packages
library(dplyr)
library(tidyverse)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)
library(jtools)

# load functions for plots
source("~/phenoEOS/analysis/00_load_functions_data.R")

# read phenology dates from MODIS
modis_pheno_sites <- readRDS("~/phenoEOS/data/modis_pheno_sites.rds")

# Select the pheno band
df_modis <- modis_pheno_sites %>% rename(on = SOS_2_doy, off = EOS_2_doy) %>% filter(off>on)

# Interannual variation (IAV) ####
# EOS ~ SOS
fit_iav_modis_off_vs_on = lmer(off ~ scale(on) + (1|sitename), data = df_modis, na.action = "na.exclude")
summary(fit_iav_modis_off_vs_on)
out <- summary(fit_iav_modis_off_vs_on)
out$coefficients
r.squaredGLMM(fit_iav_modis_off_vs_on)
plot(allEffects(fit_iav_modis_off_vs_on))
parres17 <- partialize(fit_iav_modis_off_vs_on,"on") # calculate partial residuals
out_iav_modis_off_vs_on <- allEffects(fit_iav_modis_off_vs_on)
gg_iav_modis_off_vs_on <- ggplot_on_modis(out_iav_modis_off_vs_on)
gg_iav_modis_off_vs_on

# Long-term trends ####
# Long-term separating mean across years 2001-2018 from interannual anomaly.
# EOS ~ Mean SOS + Anomalies SOS
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_off = mean(off, na.rm = TRUE), 
              mean_on = mean(on, na.rm = TRUE))
  df %>% 
    mutate(mean_on = df_mean$mean_on,
           anom_on = on - df_mean$mean_on)
}

df_modis <- df_modis %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_lt_modis_anom_on = lmer(off ~ scale(mean_on) + scale(anom_on) + (1|sitename) + (1|year), data = df_modis, na.action = "na.exclude")
summary(fit_lt_modis_anom_on)
out <- summary(fit_lt_modis_anom_on)
out$coefficients
r.squaredGLMM(fit_lt_modis_anom_on)
plot(allEffects(fit_lt_modis_anom_on))
parres18 <- partialize(fit_lt_modis_anom_on,"mean_on") # calculate partial residuals
parres19 <- partialize(fit_lt_modis_anom_on,"anom_on") # calculate partial residuals
out_lt_modis_anom_on <- allEffects(fit_lt_modis_anom_on)
gg_lt_modis_mean_on <- ggplot_mean_on(out_lt_modis_anom_on)
gg_lt_modis_anom_on <- ggplot_anom_on(out_lt_modis_anom_on)
gg_lt_modis_mean_on + gg_lt_modis_anom_on

# ED Fig. 8 ####
ff_lt_modis_mean_on <- gg_lt_modis_mean_on +
  labs(title = expression(paste("EOS ~ ", bold("Mean SOS"), " + Anomalies SOS")), subtitle = "MODIS data") +
  theme(legend.position = "none",
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))

ff_lt_modis_anom_on <- gg_lt_modis_anom_on +
  labs(title = expression(paste("EOS ~ Mean SOS + ", bold("Anomalies SOS"))), subtitle = "") +
  theme(legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .25),
        legend.direction="vertical",
        legend.margin = margin(.1, .1, .1, .1),
        legend.key.size = unit(.45, 'lines'),
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        legend.text = element_text(size=6))  

figED8 <- ff_lt_modis_mean_on + ff_lt_modis_anom_on + 
  plot_annotation(tag_levels = 'A',tag_suffix = ')') & theme(plot.tag = element_text(size = 7))
figED8 
ggsave("~/phenoEOS/manuscript/figures/ED_Fig8.jpg", width = 130, height = 70, units="mm",dpi=300)
ggsave("~/phenoEOS/manuscript/figures/ED_Fig8.eps", device=cairo_ps, width = 130, height = 70, units="mm", dpi=300)
