# This script analyses the relationship of CO2 assimilation (flux data)
# and phenological dates from remote observations (MODIS data). Outputs include Figure 2.

# load packages
library(dplyr)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)
library(jtools)

# read flux data and phenology dates from MODIS
data_flux_pheno <- readRDS("~/phenoEOS/data/data_flux_pheno.rds")

# Select the pheno band
data_flux_pheno <- data_flux_pheno %>% rename(on = SOS_2_doy, off = EOS_2_doy) %>% filter(off>on)

# Interannual variation (IAV)
# EOS ~ Anet flux data
fit_iav_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + (1|sitename) , data = data_flux_pheno, na.action = "na.exclude")
summary(fit_iav_flux_off_vs_gppnet)
r.squaredGLMM(fit_iav_flux_off_vs_gppnet)
plot(allEffects(fit_iav_flux_off_vs_gppnet))
parres4 <- partialize(fit_iav_flux_off_vs_gppnet,"gpp_net") # calculate partial residuals
out_iav_modis_off_vs_gppnet <- allEffects(fit_iav_flux_off_vs_gppnet)
str(out_iav_modis_off_vs_gppnet)
gg_iav_modis_off_vs_gppnet <- ggplot_gppnet_modis(out_iav_modis_off_vs_gppnet)
gg_iav_modis_off_vs_gppnet

# Long-term trends
# EOS ~ Anet flux data + Year 
fit_lt_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + scale(year) + (1|sitename), data = data_flux_pheno, na.action = "na.exclude")
summary(fit_lt_flux_off_vs_gppnet)
r.squaredGLMM(fit_lt_flux_off_vs_gppnet)
plot(allEffects(fit_lt_flux_off_vs_gppnet))
parres2 <- partialize(fit_lt_flux_off_vs_gppnet,"cA_tot")
parres3 <- partialize(fit_lt_flux_off_vs_gppnet,"year")
out_lt_pep_off_vs_cAtot_year <- allEffects(fit_lt_flux_off_vs_gppnet,partial.residuals = TRUE)
str(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot <- ggplot_lt_off_catot(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_year <- ggplot_lt_off_catot_year(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot + gg_lt_pep_off_vs_year + plot_layout(guides = "collect") & theme(legend.position = 'right')

# Long-term separating mean across years 2001-2018 from interannual anomaly.
# EOS ~ Mean Anet + Anomalies Anet
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_off = mean(off, na.rm = TRUE), 
              mean_gpp_net = mean(gpp, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_net = df_mean$mean_gpp_net,
           anom_gpp_net = gpp - df_mean$mean_gpp_net)
}

data_flux_pheno <- data_flux_pheno %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_flux_anom_gppnet = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename), data = data_flux_pheno, na.action = "na.exclude")
summary(fit_flux_anom_gppnet)
r.squaredGLMM(fit_flux_anom_gppnet)
plot(allEffects(fit_flux_anom_gppnet))
parres5 <- partialize(fit_flux_anom_gppnet,"mean_gpp_net") # calculate partial residuals
parres6 <- partialize(fit_flux_anom_gppnet,"anom_gpp_net") # calculate partial residuals
out_modis_anom_gppnet <- allEffects(fit_flux_anom_gppnet)
gg_modis_mean_gppnet <- ggplot_mean_gppnet(out_modis_anom_gppnet)
gg_modis_anom_gppnet <- ggplot_anom_gppnet(out_modis_anom_gppnet)
gg_modis_mean_gppnet + gg_modis_anom_gppnet + plot_layout(guides = "collect") & theme(legend.position = 'right')


