# This script analyses the relationship of CO2 assimilation (simulated using the p-model, 
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
library(maps)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# load functions for plots
source("~/phenoEOS/analysis/00_load_functions_data.R")

# read phenology dates from MODIS
modis_pheno_sites <- readRDS("~/phenoEOS/data/modis_pheno_sites.rds")

# read p-model outputs
modis_pmodel <- readRDS("~/phenoEOS/data/modis_pmodel_Anet.rds") #11.2h
modis_pmodel <- modis_pmodel %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>%
  mutate(gpp_net=ifelse(gpp_net==0, NA, gpp_net))

# join datasets
df_modis <- modis_pheno_sites %>% 
  left_join(modis_pmodel)

# Select the pheno band
df_modis <- df_modis %>% rename(on = SOS_2_doy, off = EOS_2_doy) %>% filter(off>on)
length(unique(df_modis$sitename)) #4879

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_modis_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|sitename) , data = df_modis, na.action = "na.exclude")
summary(fit_iav_modis_off_vs_gppnet)
out <- summary(fit_iav_modis_off_vs_gppnet)
out$coefficients[,"Estimate"]
out$coefficients[,"Std. Error"]
r.squaredGLMM(fit_iav_modis_off_vs_gppnet)
plot(allEffects(fit_iav_modis_off_vs_gppnet))
parres4 <- partialize(fit_iav_modis_off_vs_gppnet,"gpp_net") # calculate partial residuals
out_iav_modis_off_vs_gppnet <- allEffects(fit_iav_modis_off_vs_gppnet)
str(out_iav_modis_off_vs_gppnet)
gg_iav_modis_off_vs_gppnet <- ggplot_gppnet_modis(out_iav_modis_off_vs_gppnet)
gg_iav_modis_off_vs_gppnet

# Long-term separating mean across years 2001-2018 from interannual anomaly.
# EOS ~ Mean Anet + Anomalies Anet
separate_anom <- function(df){
  df_mean <- df %>% 
    summarise(mean_off = mean(off, na.rm = TRUE), 
              mean_gpp_net = mean(gpp_net, na.rm = TRUE))
  df %>% 
    mutate(mean_gpp_net = df_mean$mean_gpp_net,
           anom_gpp_net = gpp_net - df_mean$mean_gpp_net)
}

df_modis <- df_modis %>% 
  group_by(sitename) %>% 
  nest() %>% 
  mutate(data = purrr::map(data, ~separate_anom(.))) %>% 
  unnest(data)

fit_modis_anom_gppnet = lmer(off ~ scale(mean_gpp_net) + scale(anom_gpp_net) + (1|sitename) + (1|year), data = df_modis, na.action = "na.exclude")
summary(fit_modis_anom_gppnet)
out <- summary(fit_modis_anom_gppnet)
out$coefficients[,"Estimate"]
out$coefficient[,"Std. Error"]*1.96
r.squaredGLMM(fit_modis_anom_gppnet)
plot(allEffects(fit_modis_anom_gppnet))
parres5 <- partialize(fit_modis_anom_gppnet,"mean_gpp_net") # calculate partial residuals
parres6 <- partialize(fit_modis_anom_gppnet,"anom_gpp_net") # calculate partial residuals
out_modis_anom_gppnet <- allEffects(fit_modis_anom_gppnet)
gg_modis_mean_gppnet <- ggplot_mean_gppnet(out_modis_anom_gppnet)
gg_modis_anom_gppnet <- ggplot_anom_gppnet(out_modis_anom_gppnet)
gg_modis_mean_gppnet + gg_modis_anom_gppnet + plot_layout(guides = "collect") & theme(legend.position = 'right')
# Unscaled
trend_unscaled <- out$coefficients["scale(mean_gpp_net)","Estimate"]/ sd(df_modis$mean_gpp_net)
error_unscaled <- out$coefficients["scale(mean_gpp_net)","Std. Error"]/ sd(df_modis$mean_gpp_net)

trend_unscaled <- out$coefficients["scale(anom_gpp_net)","Estimate"]/ sd(df_modis$anom_gpp_net)
error_unscaled <- out$coefficients["scale(anom_gpp_net)","Std. Error"]/ sd(df_modis$anom_gpp_net)

# Figure 2
ff_modis_mean_gppnet <- gg_modis_mean_gppnet +
  labs(title = expression(paste("EOS ~ ", bold("Mean "), bolditalic("A")[bold(net)], 
                                " + Anomalies ", italic("A")[net])), subtitle = "MODIS data and P-model") +
  theme(legend.position = "none") 

ff_modis_anom_gppnet <- gg_modis_anom_gppnet +
  labs(title = expression(paste("EOS ~ Mean ", italic("A")[net], " + " ,
                                bold("Anomalies "), bolditalic("A")[bold(net)])), subtitle = "MODIS data and P-model") +
  theme(legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .25),
        legend.direction="vertical",
        legend.margin = margin(.2, .2, .2, .2),
        legend.key.size = unit(.6, 'lines')) 

# Maps
lon_breaks <- seq(from = floor(min(df_modis$lon)), to = ceiling(max(df_modis$lon)), by = 0.1)
lat_breaks <- seq(from = floor(min(df_modis$lat)), to = ceiling(max(df_modis$lat)), by = 0.1)

df_modis <- df_modis %>%
  ungroup() %>%
  mutate(ilon = cut(lon,
                    breaks = lon_breaks),
         ilat = cut(lat,
                    breaks = lat_breaks)) %>%
  mutate(lon_lower = as.numeric( sub("\\((.+),.*", "\\1", ilon)),
         lon_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilon) ),
         lat_lower = as.numeric( sub("\\((.+),.*", "\\1", ilat) ),
         lat_upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", ilat) )) %>%
  mutate(lon_mid = (lon_lower + lon_upper)/2,
         lat_mid = (lat_lower + lat_upper)/2) %>%
  
# create cell name to associate with climate input
  dplyr::select(-ilon, -ilat, -lon_lower, -lon_upper, -lat_lower, -lat_upper)

df_modis_agg <- df_modis %>% 
  group_by(lon_mid, lat_mid) %>% 
  summarise(mean_gpp = mean(gpp_net, na.rm = TRUE),mean_eos = mean(off)) %>% 
  rename(lon = lon_mid, lat = lat_mid)

# Generate map and zoom
world <- ne_countries(scale = "medium", returnclass = "sf")
map_eos <- ggplot(data = world) + 
  geom_sf(fill= "grey",size=.3) + 
  coord_sf(xlim = c(-180, 180), ylim = c(15, 75), expand = F) +
  geom_point(data = df_modis_agg, aes(x = lon, y = lat, color = mean_eos),size=.3) +
  labs(color="EOS (DOY)        ") +
  scale_color_viridis(option="viridis",limits=c(235,342), breaks= seq(240,340,20)) +
  theme(legend.position="right", panel.background = element_rect(fill = "aliceblue"),axis.title=element_blank(),plot.title = element_text(size = 10)) 

map_gpp <- ggplot(data = world) + 
  geom_sf(fill= "grey",size=.3) + 
  coord_sf(xlim = c(-180, 180), ylim = c(15, 75), expand = F) +
  geom_point(data = df_modis_agg, aes(x = lon, y = lat, color = mean_gpp),size=.3) +
  labs(color=expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")"))) +
  #scale_color_viridis(option="magma",limits=c(900,3500), breaks= seq(1000,3500,500)) +
  scale_color_viridis(option="magma",limits=c(650,2550), breaks= seq(1000,2500,500)) +
  theme(legend.position="right", panel.background = element_rect(fill = "aliceblue"),axis.title=element_blank(),plot.title = element_text(size = 10))

pp2 <- (ff_modis_mean_gppnet + ff_modis_anom_gppnet)/ map_eos / map_gpp +
 plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1.5, 1, 1),widths = c(2, 1, 1))
pp2
ggsave("~/phenoEOS/manuscript/figures/fig_2_rev.png", width = 9, height = 8, dpi=300)
