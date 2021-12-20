# This script analyses the relationship of spring and autumn phenological dates 
# remote-sensing observations (MODIS C6 MCD12Q2 data). Outputs include Figure S4
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

# read phenology dates from MODIS
modis_pheno_sites <- readRDS("~/pheno/data/modis_pheno_sites.rds")

# read p-model outputs
modis_pmodel <- readRDS("~/pheno/data//modis_pmodel_outputs.rds")
modis_pmodel <- modis_pmodel %>% 
  mutate(gpp_net = gpp - rd, 
         lue = gpp / apar)

# join datasets
df_modis <- modis_pheno_sites %>% 
  left_join(modis_pmodel[,2:10], by = c("lon","lat","year"))

# Select the pheno band
df_modis <- df_modis %>% rename(on = SOS_2_doy, off = EOS_2_doy) %>% filter(off>on)

# Interannual variation (IAV)
# EOS ~ SOS
fit_iav_modis_off_vs_on = lmer(off ~ scale(on) + (1|sitename), data = df_modis, na.action = "na.exclude")
summary(fit_iav_modis_off_vs_on)
r.squaredGLMM(fit_iav_modis_off_vs_on)
plot(allEffects(fit_iav_modis_off_vs_on))
out_iav_modis_off_vs_on <- allEffects(fit_iav_modis_off_vs_on)
gg_iav_modis_off_vs_on <- ggplot_on(out_iav_modis_off_vs_on)
gg_iav_modis_off_vs_on

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
plot(allEffects(fit_lt_modis_anom_on))
r.squaredGLMM(fit_lt_modis_anom_on)
out_lt_modis_anom_on <- allEffects(fit_lt_modis_anom_on)
gg_lt_modis_mean_on <- ggplot_mean_on(out_lt_modis_anom_on)
gg_lt_modis_anom_on <- ggplot_anom_on(out_lt_modis_anom_on)
gg_lt_modis_mean_on + gg_lt_modis_anom_on

## Supplementary Fig. S4
ff_lt_modis_mean_on <- gg_lt_modis_mean_on +
  labs(title = expression(paste("EOS ~ ", bold("Mean SOS"), " + Anomalies SOS")), subtitle = "MODIS data") 

ff_lt_modis_anom_on <- gg_lt_modis_anom_on +
  labs(title = expression(paste("EOS ~ Mean SOS + ", bold("Anomalies SOS"))), subtitle = "MODIS data") 

ss4 <- ff_lt_modis_mean_on + ff_lt_modis_anom_on
ss4 + plot_annotation(tag_levels = 'A')
ggsave("~/pheno/manuscript/figures/fig_S4.png", width = 7.5, height = 4, dpi=300)
