# This script analyses the relationship of CO2 assimilation (flux data)
# and phenological dates from remote observations (MODIS data). Outputs include Figure 2.

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
library(sjPlot)

# read flux data and phenology dates from MODIS
data_flux_modis_pmodel <- readRDS("~/phenoEOS/data/data_flux_modis_pmodel.rds")
data_flux_modis_pmodel <- data_flux_modis_pmodel %>% rename(on = SOS_2_doy, off = EOS_2_doy,gpp=gpp_flux)
length(unique(data_flux_modis_pmodel$sitename))
ggplot(data_flux_modis_pmodel) + geom_point(aes(gpp,off))

# Interannual variation (IAV)
# EOS ~ Anet flux data
fit_iav_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + (1|sitename) , data = data_flux_modis_pmodel, na.action = "na.exclude")
summary(fit_iav_flux_off_vs_gppnet)
r.squaredGLMM(fit_iav_flux_off_vs_gppnet)
plot(allEffects(fit_iav_flux_off_vs_gppnet))

# Long-term trends
# EOS ~ Anet flux data + Year 
fit_lt_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + scale(year) + (1|sitename), data = data_flux_modis_pmodel, na.action = "na.exclude")
summary(fit_lt_flux_off_vs_gppnet)
r.squaredGLMM(fit_lt_flux_off_vs_gppnet)
plot(allEffects(fit_lt_flux_off_vs_gppnet))

# Spatial variation
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

data_flux_modis_pmodel <- data_flux_modis_pmodel %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_flux_anom_gppnet = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename) + (1|year), data = data_flux_modis_pmodel, na.action = "na.exclude")
summary(fit_flux_anom_gppnet)
r.squaredGLMM(fit_flux_anom_gppnet)
plot(allEffects(fit_flux_anom_gppnet))

gg_flux_mean_gppnet <- plot_model(fit_flux_anom_gppnet, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean "), bolditalic("A")[bold(net)], 
                                " + Anomalies ", italic("A")[net])), subtitle = "FLUXNET observations",
       x = expression(paste("Mean " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 580, y = 300, label = "italic(P) == 0.075",parse = TRUE,size=3)
gg_flux_mean_gppnet

gg_flux_anom_gppnet <- plot_model(fit_flux_anom_gppnet, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean ", italic("A")[net], " + " ,
                                bold("Anomalies "), bolditalic("A")[bold(net)])), subtitle = "FLUXNET observations",
       x = expression(paste("Anomalies " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -750, y = 280, label = "italic(P) == 0.762",parse = TRUE,size=3)
gg_flux_anom_gppnet
