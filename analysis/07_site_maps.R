# This script plot the maps showing geographical distributions and densities of the data. Outputs include Figure S5.

# load packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(maps)
library(viridis)
library(rgdal)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Maps for PEP725 sites ####

# read phenology dates from PEP725
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# create map
lon_breaks <- seq(from = floor(min(df_pep$lon)), to = ceiling(max(df_pep$lon)), by = 0.1)
lat_breaks <- seq(from = floor(min(df_pep$lat)), to = ceiling(max(df_pep$lat)), by = 0.1)

df_pep <- df_pep %>%
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

df_pep_agg <- df_pep %>% 
  group_by(lon_mid, lat_mid) %>% 
  summarise(mean_catot = mean(cA_tot, na.rm = TRUE),mean_eos = mean(off)) %>% 
  rename(lon = lon_mid, lat = lat_mid)

# generate map and zoom
world <- ne_countries(scale = "medium", returnclass = "sf")
map_pep_sites <- ggplot(data = world) + 
  geom_sf(fill= "grey",size=.3) + 
  coord_sf(xlim = c(-30, 60), ylim = c(30, 75), expand = F) +
  geom_point(data = df_pep_agg, aes(x = lon, y = lat),size=.3,color="darkred",alpha=0.2) +
  theme(legend.position="right", panel.background = element_rect(fill = "aliceblue"),
        axis.title=element_blank(),plot.title = element_text(size = 10)) 
map_pep_sites

# Maps for MODIS sites ####

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
df_modis <- df_modis %>% rename(on = SOS_2_doy, off = EOS_2_doy) %>% filter(off>on)

# create map
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

# generate map and zoom
world <- ne_countries(scale = "medium", returnclass = "sf")
map_modis_sites <- ggplot(data = world) + 
  geom_sf(fill= "grey",size=.3) + 
  coord_sf(xlim = c(-180, 180), ylim = c(15, 75), expand = F) +
  geom_point(data = df_modis_agg, aes(x = lon, y = lat),size=.3,color="darkred",alpha=0.3) +
  theme(legend.position="right", panel.background = element_rect(fill = "aliceblue"),
        axis.title=element_blank(),plot.title = element_text(size = 10))

ffs <- map_pep_sites / map_modis_sites
ffs + plot_annotation(tag_levels = 'A') + plot_layout(heights = c(1, 1),widths = c(1,0.7))
ggsave("~/phenoEOS/manuscript/figures/fig_S5_rev.png", width = 9, height = 8, dpi=300)
