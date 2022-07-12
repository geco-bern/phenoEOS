# This script analyses the relationship of CO2 assimilation (from both flux observations and p-model simulations) and phenological 
# dates from remote observations (MODIS data).Outputs include Figure S6 (observed and modelled GPP) and Figure S7 (modelled GPP net).

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

# GPP from Fluxnet observations ####
fluxnet_pmodel_pheno <- readRDS("~/phenoEOS/data/fluxnet_pmodel_pheno.rds")
fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  rename(on = SOS_2_doy, off = EOS_2_doy, gpp=Anet_flux)
length(unique(fluxnet_pmodel_pheno$sitename))

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

fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_flux_anom_gpp = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename) + (1|year), data = fluxnet_pmodel_pheno, na.action = "na.exclude")
summary(fit_flux_anom_gpp)
r.squaredGLMM(fit_flux_anom_gpp)
plot(allEffects(fit_flux_anom_gpp))
out <- summary(fit_flux_anom_gpp)
# Unscaled
trend_unscaled <- out$coefficients["scale(mean_gpp_net)","Estimate"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net)
error_unscaled <- out$coefficients["scale(mean_gpp_net)","Std. Error"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net)

gg_flux_mean_gpp <- plot_model(fit_flux_anom_gpp, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean GPP"),
                                " + Anomalies GPP")), subtitle = "FLUXNET observations",
       x = expression(paste("Mean GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 900, y = 300, label = "italic(P) == 0.191",parse = TRUE,size=3) +
  scale_y_continuous(limits = c(240,300),breaks = seq(240,300,20)) + 
  scale_x_continuous(limits = c(750,1800),breaks = seq(750,1800,250)) 
gg_flux_mean_gpp

gg_flux_anom_gpp <- plot_model(fit_flux_anom_gpp, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean GPP", " + " ,
                                bold("Anomalies GPP"))), subtitle = "FLUXNET observations",
       x = expression(paste("Anomalies GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -300, y = 285, label = "italic(P) == 0.971",parse = TRUE,size=3) +
  scale_y_continuous(limits = c(260,285),breaks = seq(260,280,10)) + 
  scale_x_continuous(limits = c(-500,800),breaks = seq(-400,800,400)) 
gg_flux_anom_gpp

# GPP from P-model simulations ####
fluxnet_pmodel_pheno <- readRDS("~/phenoEOS/data/fluxnet_pmodel_pheno.rds")
fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  rename(on = SOS_2_doy, off = EOS_2_doy, gpp=Anet_pmodel)
length(unique(fluxnet_pmodel_pheno$sitename))

# Long-term separating mean across years from interannual anomaly.
# EOS ~ Mean Anet + Anomalies Anet
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_off = mean(off, na.rm = TRUE), 
              mean_gpp_net = mean(gpp, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_net = df_mean$mean_gpp_net,
           anom_gpp_net = gpp - df_mean$mean_gpp_net)
}

fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_pmodel_anom_gpp = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename) + (1|year), data = fluxnet_pmodel_pheno, na.action = "na.exclude")
summary(fit_pmodel_anom_gpp)
r.squaredGLMM(fit_pmodel_anom_gpp)
plot(allEffects(fit_pmodel_anom_gpp))
out <- summary(fit_pmodel_anom_gpp)
# Unscaled
trend_unscaled <- out$coefficients["scale(mean_gpp_net)","Estimate"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)
error_unscaled <- out$coefficients["scale(mean_gpp_net)","Std. Error"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)

gg_pmodel_mean_gpp <- plot_model(fit_pmodel_anom_gpp, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean GPP"),
                                " + Anomalies GPP")), subtitle = "P-model simulations",
       x = expression(paste("Mean GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 990, y = 300, label = "italic(P) == 0.831",parse = TRUE,size=3) +
  scale_y_continuous(limits = c(240,300),breaks = seq(240,300,20)) + 
  scale_x_continuous(limits = c(890,1620),breaks = seq(750,1800,250)) 
gg_pmodel_mean_gpp

gg_pmodel_anom_gpp <- plot_model(fit_pmodel_anom_gpp, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean GPP", " + " ,
                                bold("Anomalies GPP"))), subtitle = "P-model simulations",
       x = expression(paste("Anomalies GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -350, y = 290, label = paste0("italic(P) == ", deparse("0.810")),parse = TRUE,size=3) +
  scale_y_continuous(limits = c(260,290),breaks = seq(260,290,10)) + 
  scale_x_continuous(limits = c(-450,300),breaks = seq(-400,400,200)) 
gg_pmodel_anom_gpp

# join plots from Fluxnet observations and P-model simulations analyses
ppS6 <- gg_flux_mean_gpp + gg_flux_anom_gpp + gg_pmodel_mean_gpp + gg_pmodel_anom_gpp + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') #+ plot_layout(guides = "collect") & theme(legend.position = 'left')
ppS6
ggsave("~/phenoEOS/manuscript/figures/fig_S6_rev.png", width = 7.5, height = 7.5, dpi=300)

# GPP net from P-model simulations ####
fluxnet_pmodel_pheno <- readRDS("~/phenoEOS/data/fluxnet_pmodel_pheno.rds")
fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>% 
  rename(on = SOS_2_doy, off = EOS_2_doy, gpp = gpp_net)
length(unique(fluxnet_pmodel_pheno$sitename))

# Long-term separating mean across years from interannual anomaly.
# EOS ~ Mean Anet + Anomalies Anet
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_off = mean(off, na.rm = TRUE), 
              mean_gpp_net = mean(gpp, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_net = df_mean$mean_gpp_net,
           anom_gpp_net = gpp - df_mean$mean_gpp_net)
}

fluxnet_pmodel_pheno <- fluxnet_pmodel_pheno %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_pmodel_anom_gppnet = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename) + (1|year), data = fluxnet_pmodel_pheno, na.action = "na.exclude")
summary(fit_pmodel_anom_gppnet)
r.squaredGLMM(fit_pmodel_anom_gppnet)
plot(allEffects(fit_pmodel_anom_gppnet))
out <- summary(fit_pmodel_anom_gppnet)
# Unscaled
trend_unscaled <- out$coefficients["scale(mean_gpp_net)","Estimate"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)
error_unscaled <- out$coefficients["scale(mean_gpp_net)","Std. Error"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)

gg_pmodel_mean_gppnet <- plot_model(fit_pmodel_anom_gppnet, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean "), bold("GPP")[bold(net)], 
                                " + Anomalies ", "GPP"[net])), subtitle = "P-model simulations",
       x = expression(paste("Mean " ,"GPP"[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 940, y = 300, label = "italic(P) == 0.741",parse = TRUE,size=3) +
  scale_y_continuous(limits = c(240,300),breaks = seq(240,300,20)) + 
  scale_x_continuous(limits = c(850,1520),breaks = seq(1000,1500,250)) +
  theme(plot.title = element_text(size=11),plot.subtitle = element_text(size=10))
gg_pmodel_mean_gppnet

gg_pmodel_anom_gppnet <- plot_model(fit_pmodel_anom_gppnet, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean ", "GPP"[net], " + " ,
                                bold("Anomalies "), bold("GPP")[bold(net)])), subtitle = "P-model simulations",
       x = expression(paste("Anomalies " ,"GPP"[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -350, y = 290, label = paste0("italic(P) == ", deparse("0.800")),parse = TRUE,size=3) +
  scale_y_continuous(limits = c(260,290),breaks = seq(260,290,10)) + 
  scale_x_continuous(limits = c(-450,300),breaks = seq(-400,400,200))  +
  theme(plot.title = element_text(size=11),plot.subtitle = element_text(size=10))
gg_pmodel_anom_gppnet

# join plots
ppS7 <- gg_pmodel_mean_gppnet + gg_pmodel_anom_gppnet + plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') #+ plot_layout(guides = "collect") & theme(legend.position = 'left')
ppS7
ggsave("~/phenoEOS/manuscript/figures/fig_S7_rev.png", width = 7.5, height = 4, dpi=300)
