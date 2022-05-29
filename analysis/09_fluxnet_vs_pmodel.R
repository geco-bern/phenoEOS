# This script compares gpp from fluxnet sites with gpp simulations from p-model

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
library(ggpubr)

# read flux data and phenology dates from MODIS
data_flux_modis_pmodel <- readRDS("~/phenoEOS/data/data_flux_modis_pmodel.rds")
data_flux_modis_pmodel <- data_flux_modis_pmodel %>% rename(on = SOS_2_doy, off = EOS_2_doy)
length(unique(data_flux_modis_pmodel$sitename))
ggplot(data_flux_modis_pmodel) + geom_point(aes(gpp_flux,off))
ggplot(data_flux_modis_pmodel) + geom_point(aes(gpp_pmodel,off))

# Filter to select the 23 sites
#data_flux_modis_pmodel <- data_flux_modis_pmodel %>% filter(year>=2001&year<=2014) %>% drop_na(gpp_pmodel)
#length(unique(data_flux_modis_pmodel$sitename))

# Comparison from GPP data (before aggregating)
flux_sites_data
df_from_flux <- flux_sites_data %>% select(sitename,year, doy, gpp) %>% rename(gpp_flux=gpp)
model_data
df_from_pmodel <- model_data %>% select(sitename,year, doy, gpp) %>% rename(gpp_pmodel=gpp)

df_from_both <- df_from_flux %>% left_join(df_from_pmodel)
df_from_both <- df_from_both %>% filter(gpp_pmodel>0&gpp_flux>0)
length(unique(df_from_both$sitename))

data_flux_modis_pmodel <- df_from_both
  
fit <- lm(gpp_flux ~ gpp_pmodel , data = data_flux_modis_pmodel, na.action = "na.exclude")
summary(fit)

gg_scatter <- ggplot(data = data_flux_modis_pmodel, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="red",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=650, label.y=2200, digits = 3) +
  theme_classic() +
  scale_y_continuous(limits = c(650,2200),breaks = seq(500,2500,500)) + 
  scale_x_continuous(limits = c(650,2200),breaks = seq(500,2500,500)) + 
  #coord_cartesian(xlim = c(650,2200), ylim = c(650,2200), expand = expansion(mult = 0)) +
  labs(x = "Simulated", y = "Observed") 
gg_scatter

# plot site-level means
df_site_means <- data_flux_modis_pmodel %>% group_by(sitename) %>% summarise(gpp_flux=mean(gpp_flux,na.rm=T),
                                                                             gpp_pmodel=mean(gpp_pmodel,na.rm=T))
gg_site_means <- ggplot(data = df_site_means, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="red",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=900, label.y=2000, digits = 3) +
  theme_classic() +
  scale_y_continuous(limits = c(900,2000),breaks = seq(500,2500,500)) + 
  scale_x_continuous(limits = c(900,2000),breaks = seq(500,2500,500)) + 
  labs(x = "Simulated", y = "Observed")
gg_site_means

# plot trends
df_year_trend <- data_flux_modis_pmodel %>% group_by(year) %>% summarise(gpp_flux=mean(gpp_flux,na.rm=T),
                                                                         gpp_pmodel=mean(gpp_pmodel,na.rm=T))
gg_year_trend <- ggplot(data = df_year_trend, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="red",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=1300, label.y=1520, digits = 3) +
  theme_classic() +
  scale_y_continuous(limits = c(1300,1520),breaks = seq(1300,1550,100)) + 
  scale_x_continuous(limits = c(1300,1520),breaks = seq(1300,1550,100)) + 
  labs(x = "Simulated", y = "Observed ")
gg_year_trend

# plot IA anomalies
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_gpp_flux = mean(gpp_flux, na.rm = TRUE), 
              mean_gpp_pmodel = mean(gpp_pmodel, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_flux = df_mean$mean_gpp_flux,
           mean_gpp_pmodel = df_mean$mean_gpp_pmodel,
           anom_gpp_flux = gpp_flux - df_mean$mean_gpp_flux,
           anom_gpp_pmodel = gpp_pmodel - df_mean$mean_gpp_pmodel)
}

data_flux_modis_pmodel <- data_flux_modis_pmodel %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

gg_anomalies <- ggplot(data = data_flux_modis_pmodel, aes(x = anom_gpp_pmodel, y = anom_gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="red",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=-400, label.y=1000, digits = 2) +
  theme_classic() +
  scale_y_continuous(limits = c(-1000,1000),breaks = seq(-1000,1000,500)) + 
  scale_x_continuous(limits = c(-400,400),breaks = seq(-400,400,200)) + 
  labs(x = "Simulated", y = "Observed")
gg_anomalies

# Figure
ff_scatter <- gg_scatter +
  labs(title = expression(paste("Observed vs. simulated ",italic("A")[net])),
       subtitle = "Values for each site and year") +
  theme(legend.position = "none") 
ff_scatter

ff_site_means <- gg_site_means +
  labs(title = "", 
       subtitle = "Site-level means") +
  theme(legend.position = "none") 
ff_site_means

ff_year_trend <- gg_year_trend +
  labs(title = "", 
       subtitle = "Temporal trends") +
  theme(legend.position = "none") 
ff_year_trend

ff_anomalies <- gg_anomalies +
  labs(title = "", 
       subtitle = "Interannual anomalies") +
  theme(legend.position = "none") 
ff_anomalies

ppS6 <- ff_scatter + ff_site_means + ff_year_trend + ff_anomalies + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') #+ plot_layout(guides = "collect") & theme(legend.position = 'left')
ppS6
ggsave("~/phenoEOS/manuscript/figures/fig_S6_rev.png", width = 7.5, height = 7.5, dpi=300)

# Comparison from GPP data (before aggregating)
flux_sites_data <- readRDS("~/phenoEOS/data/fluxnet_sites/flux_sites_data.rds")
df_from_flux <- flux_sites_data %>% select(sitename,year, doy, gpp) %>% rename(gpp_flux=gpp)
model_data <- readRDS("~/phenoEOS/data/fluxnet_sites/p_model/model_data.rds")
df_from_pmodel <- model_data %>% select(sitename,year, doy, gpp) %>% rename(gpp_pmodel=gpp)
df_from_both <- df_from_flux %>% left_join(df_from_pmodel) %>% filter(gpp_pmodel>0&gpp_flux>0) %>%
  group_by(sitename,year) %>% mutate(n=n())
length(unique(df_from_both$sitename))

fit <- lm(gpp_flux ~ gpp_pmodel , ldata = df_from_both, na.action = "na.exclude")
summary(fit)

gg_scatterGPP <- ggplot(data = df_from_both, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_hex(bins = 100) +     
  scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.9),
                       trans = "log",limits=c(1,1200),breaks=c(5,50,400)) +
  geom_smooth(method = "lm", se = F,col="black",fullrange=TRUE) +
  stat_cor(aes(label=..rr.label..), label.x=0, label.y=25, digits = 3) +
  theme_classic() +
  #geom_abline(intercept = 0, slope = 1,col="black",linetype="dashed") +
  scale_y_continuous(limits = c(0,25),breaks = seq(0,25,5)) + 
  scale_x_continuous(limits = c(0,25),breaks = seq(0,25,5)) + 
  scale_color_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                       trans = "log") +
  labs(x = "Simulated", y = "Observed") 
gg_scatterGPP

# plot site-level means
df_site_means <- df_from_both %>% group_by(sitename) %>% summarise(gpp_flux=mean(gpp_flux,na.rm=T),
                                                                   gpp_pmodel=mean(gpp_pmodel,na.rm=T))
gg_site_meansGPP <- ggplot(data = df_site_means, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="black",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=3, label.y=7, digits = 3) +
  theme_classic() +
  scale_y_continuous(limits = c(3,7),breaks = seq(3,7,1)) + 
  scale_x_continuous(limits = c(3,7),breaks = seq(3,7,1)) + 
  labs(x = "Simulated", y = "Observed")
gg_site_meansGPP

# plot trends
df_year_trend <- df_from_both %>% group_by(year) %>% summarise(gpp_flux=mean(gpp_flux,na.rm=T),
                                                               gpp_pmodel=mean(gpp_pmodel,na.rm=T))
gg_year_trendGPP <- ggplot(data = df_year_trend, aes(x = gpp_pmodel, y = gpp_flux)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = F,col="black",fullrange=TRUE) +
  #stat_regline_equation(label.x=1750, label.y=2400) +
  stat_cor(aes(label=..rr.label..), label.x=4.5, label.y=5.5, digits = 3) +
  theme_classic() +
  scale_y_continuous(limits = c(4.5,5.5),breaks = seq(4.5,5.5,0.5)) + 
  scale_x_continuous(limits = c(4.5,5.5),breaks = seq(4.5,5.5,0.5)) + 
  labs(x = "Simulated", y = "Observed ")
gg_year_trendGPP

# plot IA anomalies
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_gpp_flux = mean(gpp_flux, na.rm = TRUE), 
              mean_gpp_pmodel = mean(gpp_pmodel, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_flux = df_mean$mean_gpp_flux,
           mean_gpp_pmodel = df_mean$mean_gpp_pmodel,
           anom_gpp_flux = gpp_flux - df_mean$mean_gpp_flux,
           anom_gpp_pmodel = gpp_pmodel - df_mean$mean_gpp_pmodel)
}

df_from_both <- df_from_both %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

gg_anomaliesGPP <- ggplot(data = df_from_both, aes(x = anom_gpp_pmodel, y = anom_gpp_flux)) + 
  geom_hex(bins = 100) +     
  scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.9),
                       trans = "log",limits=c(1,1200),breaks=c(5,50,400)) +
  geom_smooth(method = "lm", se = F,col="black",fullrange=TRUE) +
  stat_cor(aes(label=..rr.label..), label.x=-7, label.y=20, digits = 3) +
  theme_classic() +
  #geom_abline(intercept = 0, slope = 1,col="black",linetype="dashed") +
  scale_y_continuous(limits = c(-7,20),breaks = seq(-10,20,5)) + 
  scale_x_continuous(limits = c(-7,20),breaks = seq(-10,20,5)) + 
  labs(x = "Simulated", y = "Observed") 
gg_anomaliesGPP

# Figure
ff_scatterGPP <- gg_scatterGPP +
  labs(title = "Observed vs. simulated GPP",
       subtitle = "Values for each site and day") +
  theme(legend.position = "right") 
ff_scatterGPP

ff_site_meansGPP <- gg_site_meansGPP +
  labs(title = "", 
       subtitle = "Site-level means") +
  theme(legend.position = "none") 
ff_site_meansGPP

ff_year_trendGPP <- gg_year_trendGPP +
  labs(title = "", 
       subtitle = "Temporal trends") +
  theme(legend.position = "none") 
ff_year_trendGPP

ff_anomaliesGPP <- gg_anomaliesGPP +
  labs(title = "", 
       subtitle = "Interannual anomalies") +
  theme(legend.position = "right") 
ff_anomaliesGPP

ppS6_GPP <- ff_scatterGPP + ff_site_meansGPP + ff_year_trendGPP + ff_anomaliesGPP + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') + plot_layout(guides = "collect") & theme(legend.position = 'none')
ppS6_GPP
ggsave("~/phenoEOS/manuscript/figures/ppS6_GPP_rev.png", width = 7.5, height = 7.5, dpi=300)

# join plots from Fluxnet observations and P-model simulations analyses
ppS7 <- gg_flux_mean_gppnet + gg_flux_anom_gppnet + gg_pmodel_mean_gppnet + gg_pmodel_anom_gppnet + 
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A') #+ plot_layout(guides = "collect") & theme(legend.position = 'left')
ppS7
ggsave("~/phenoEOS/manuscript/figures/fig_S7_rev.png", width = 7.5, height = 7.5, dpi=300)

