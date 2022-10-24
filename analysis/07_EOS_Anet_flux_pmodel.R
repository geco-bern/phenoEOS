# This script analyses the relationship of CO2 assimilation (from both flux observations and p-model simulations) and phenological 
# dates from remote observations (MODIS data).Outputs include ED Fig. 5 (observed and modelled GPP) and ED Fig. 6 (modelled GPP net).

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
trend_unscaled <- out$coefficients["scale(mean_gpp_net)","Estimate"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)
error_unscaled <- out$coefficients["scale(mean_gpp_net)","Std. Error"]/ sd(fluxnet_pmodel_pheno$mean_gpp_net,na.rm=T)
trend_unscaled
error_unscaled

gg_flux_mean_gpp <- plot_model(fit_flux_anom_gpp, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean GPP"),
                                " + Anomalies GPP")), subtitle = "FLUXNET observations",
       x = expression(paste("Mean GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 450, y = 310, label = "italic(P) == 0.095",parse = TRUE,size=2) +
  scale_y_continuous(limits = c(200,310),breaks = seq(220,310,40)) + 
  scale_x_continuous(limits = c(320,1850),breaks = seq(400,1800,400)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
gg_flux_mean_gpp

gg_flux_anom_gpp <- plot_model(fit_flux_anom_gpp, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean GPP", " + " ,
                                bold("Anomalies GPP"))), subtitle = "",
       x = expression(paste("Anomalies GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -850, y = 285, label = "italic(P) == 0.646",parse = TRUE,size=2) +
  scale_y_continuous(limits = c(246,285),breaks = seq(250,280,10)) + 
  scale_x_continuous(limits = c(-1050,1200),breaks = seq(-1000,1200,500)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
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
trend_unscaled
error_unscaled

gg_pmodel_mean_gpp <- plot_model(fit_pmodel_anom_gpp, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean GPP"),
                                " + Anomalies GPP")), subtitle = "P-model simulations",
       x = expression(paste("Mean GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 830, y = 310, label = "italic(P) == 0.115",parse = TRUE,size=2) +
  scale_y_continuous(limits = c(200,310),breaks = seq(220,310,40)) + 
  scale_x_continuous(limits = c(750,1650),breaks = seq(750,1620,250)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
gg_pmodel_mean_gpp

gg_pmodel_anom_gpp <- plot_model(fit_pmodel_anom_gpp, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean GPP", " + " ,
                                bold("Anomalies GPP"))), subtitle = "",
       x = expression(paste("Anomalies GPP" , " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -420, y = 285, label = paste0("italic(P) == ", deparse("0.860")),parse = TRUE,size=2) +
  scale_y_continuous(limits = c(246,285),breaks = seq(250,280,10)) + 
  scale_x_continuous(limits = c(-500,400),breaks = seq(-500,500,250)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
gg_pmodel_anom_gpp

# ED Fig. 5 ####
# join plots from Fluxnet observations and P-model simulations analyses
figED5 <- gg_flux_mean_gpp + gg_flux_anom_gpp + gg_pmodel_mean_gpp + gg_pmodel_anom_gpp + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A',tag_suffix = ')') & theme(plot.tag = element_text(size = 7))
figED5
ggsave("~/phenoEOS/manuscript/figures/ED_Fig5.jpg", width = 150, height = 150, units="mm",dpi=300)
ggsave("~/phenoEOS/manuscript/figures/ED_Fig5.eps", device=cairo_ps, width = 150, height = 150, units="mm", dpi=300)

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
trend_unscaled
error_unscaled

gg_pmodel_mean_gppnet <- plot_model(fit_pmodel_anom_gppnet, type = "pred", terms = c("mean_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ ", bold("Mean "), bold("GPP")[bold(net)], 
                                " + Anomalies ", "GPP"[net])), subtitle = "P-model simulations",
       x = expression(paste("Mean " ,"GPP"[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = 800, y = 320, label = "italic(P) == 0.095",parse = TRUE,size=2) +
  scale_y_continuous(limits = c(200,320),breaks = seq(220,310,40)) + 
  scale_x_continuous(limits = c(700,1550),breaks = seq(750,1620,250)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
gg_pmodel_mean_gppnet

gg_pmodel_anom_gppnet <- plot_model(fit_pmodel_anom_gppnet, type = "pred", terms = c("anom_gpp_net")) +
  theme_classic() +
  labs(title = expression(paste("EOS ~ Mean ", "GPP"[net], " + " ,
                                bold("Anomalies "), bold("GPP")[bold(net)])), subtitle = "",
       x = expression(paste("Anomalies " ,"GPP"[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)") +
  annotate("text", x = -400, y = 285, label = paste0("italic(P) == ", deparse("0.867")),parse = TRUE,size=2) +
  scale_y_continuous(limits = c(246,285),breaks = seq(250,280,10)) + 
  scale_x_continuous(limits = c(-500,400),breaks = seq(-500,500,250)) +
  theme(plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))
gg_pmodel_anom_gppnet

# ED Fig. 6 ####
figED6 <- gg_pmodel_mean_gppnet + gg_pmodel_anom_gppnet + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'A',tag_suffix = ')') & theme(plot.tag = element_text(size = 7))
figED6
ggsave("~/phenoEOS/manuscript/figures/ED_Fig6.jpg", width = 150, height = 75, units="mm",dpi=300)
ggsave("~/phenoEOS/manuscript/figures/ED_Fig6.eps", device=cairo_ps, width = 150, height = 75, units="mm", dpi=300)

