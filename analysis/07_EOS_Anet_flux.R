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

# read flux data and phenology dates from MODIS
data_flux_pheno_modis <- readRDS("~/phenoEOS/data/flux_sites/data_flux_pheno_modis.rds")

# Select the pheno band
data_flux_pheno_modis <- data_flux_pheno_modis %>% rename(on = SOS_2_doy, off = EOS_2_doy)
length(unique(data_flux_pheno_modis$sitename))

# Select the sites with longer # years
ggplot(data_flux_pheno_modis) + geom_point(aes(gpp_21Jun,off))
ggplot(data_flux_pheno_modis) + geom_point(aes(gpp_11h,off))
data_site <- data_flux_pheno_modis %>%  group_by(sitename) %>% mutate(n_years=n())

# Chose gpp = to gpp_21Jun or gpp_11h
data_flux_pheno_modis <- data_flux_pheno_modis %>% rename(gpp=gpp_11h)

# One-site analysis
#data_site <- data_flux_pheno_modis %>% filter(sitename==unique(data_flux_pheno_modis$sitename)[2])
data_site <- data_site %>% filter(n_years==max(data_site$n_years)) # BE-Bra and BE-Vie
#data_site1 <- data_site %>% filter(sitename == "BE-Bra")
fit_iav_site = lm(off ~ scale(gpp_21Jun) + scale(year), data = data_site, na.action = "na.exclude")
summary(fit_iav_site)
plot(allEffects(fit_iav_site))

# Interannual variation (IAV)
# EOS ~ Anet flux data
fit_iav_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + (1|sitename) , data = data_flux_pheno_modis, na.action = "na.exclude")
summary(fit_iav_flux_off_vs_gppnet)
r.squaredGLMM(fit_iav_flux_off_vs_gppnet)
plot(allEffects(fit_iav_flux_off_vs_gppnet))

# Long-term trends
# EOS ~ Anet flux data + Year 
fit_lt_flux_off_vs_gppnet = lmer(off ~ scale(gpp) + scale(year) + (1|sitename), data = data_flux_pheno_modis, na.action = "na.exclude")
summary(fit_lt_flux_off_vs_gppnet)
r.squaredGLMM(fit_lt_flux_off_vs_gppnet)
plot(allEffects(fit_lt_flux_off_vs_gppnet))

library(sjPlot) # plot_model fc
figboth1 <- plot_model(fit_lt_flux_off_vs_gppnet, type = "pred", terms = c("year")) +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), 
       subtitle = "FLUXNET gpp 11.2h") 
figboth2 <- plot_model(fit_lt_flux_off_vs_gppnet, type = "pred", terms = c("gpp")) +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), 
       subtitle = "FLUXNET gpp 11.2h") 
figgpp <- plot_model(fit_iav_flux_off_vs_gppnet, type = "pred", terms = c("gpp")) +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), 
       subtitle = "FLUXNET gpp 11.2h") 

figboth1 + figboth2 + figgpp

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

data_flux_pheno_modis <- data_flux_pheno_modis %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_flux_anom_gppnet = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename), data = data_flux_pheno_modis, na.action = "na.exclude")
summary(fit_flux_anom_gppnet)
r.squaredGLMM(fit_flux_anom_gppnet)
plot(allEffects(fit_flux_anom_gppnet))

figboth1 <- plot_model(fit_flux_anom_gppnet, type = "pred", terms = c("mean_gpp_net")) +
  labs(title = expression(paste("EOS ~ ", bold("mean_gpp_net"), " + anom_gpp_net", )), 
       subtitle = "FLUXNET gpp 11.2h") 
figboth2 <- plot_model(fit_flux_anom_gppnet, type = "pred", terms = c("anom_gpp_net")) +
  labs(title = expression(paste("EOS ~ mean_gpp_net + ", bold("anom_gpp_net"))), 
       subtitle = "FLUXNET gpp 11.2h") 

figboth1 + figboth2 
