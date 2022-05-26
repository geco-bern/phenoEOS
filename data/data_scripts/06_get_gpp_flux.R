# This script reads the gpp data from the flux data kit sites and filter the data for
# deciduous forests.

# load packages
library(dplyr)
library(tidyverse)
library(amerifluxr)
library(geosphere)
library(bigleaf)
library(rnaturalearth)

# read all sites from the FLUXNET2015
fluxnet2015 <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_metadata.csv")

# filter those sites from the Northern Hemisphere and those classified as deciduous (DBF, DNF) and mixed forests (MF)
fluxnet2015 <- fluxnet2015 %>% filter(LOCATION_LAT>=10,IGBP=="DBF"|IGBP=="DNF"|IGBP=="MF") %>% 
  rename(sitename=SITE_ID,lat=LOCATION_LAT,lon=LOCATION_LONG,ele=LOCATION_ELEV)
fluxnet2015 %>% n_distinct() #32

# read files for the selected sites at daily (DD) resolution
setwd("~/phenoEOS/data/fluxnet_sites/FLUXNET2015")
flux_sites <- list.files(pattern = "*.csv") %>%
  purrr::map(read.csv) %>% 
  bind_rows()

# join with metadata from sites
flux_sites_data <- flux_sites %>% rename(sitename=site) %>% left_join(fluxnet2015[c(1,5,6,7,8)]) %>% 
  rename(gpp=GPP_NT_VUT_REF)

# prepare variables 
flux_sites_data <-  flux_sites_data %>% select(c(1:3,359,360,361,362)) %>%
  mutate(gpp = na_if(gpp, -9999),
         date=lubridate::ymd(TIMESTAMP),
         year=lubridate::year(date),
         doy=lubridate::yday(date),
         daylength=daylength(lat, doy))

length(unique(flux_sites_data$sitename))
unique(flux_sites_data$IGBP)
min(flux_sites_data$lat)

# check gaps in the data
quality_control <- flux_sites_data %>% group_by(sitename, year) %>% summarise(gpp_mean=mean(gpp))
quality_control %>% filter(is.na(gpp_mean)) %>% summarise(sitename=unique(sitename))
  
# aggregate gpp data summing up to the cutoff
# keeping those gap years with NAs
# doy_cutoff <- lubridate::yday("2001-06-21") 
daylength_cutoff <- 11.2

flux_sites_data <- flux_sites_data %>%
  #mutate(gpp_21Jun = ifelse(!is.na(gpp) & doy >= doy_cutoff, 0, gpp)) %>%
  mutate(gpp_11h = ifelse(!is.na(gpp) & daylength <= daylength_cutoff, 0, gpp)) # alternative cutoff

df_flux_agg <- flux_sites_data %>% 
  group_by(sitename, year) %>% 
  summarise(#gpp_21Jun = sum(gpp_21Jun,na.rm = F),
            gpp_11h = sum(gpp_11h,na.rm = F)) %>%
  mutate(#gpp_21Jun=ifelse(gpp_21Jun<=0,NA,gpp_21Jun),
         gpp_11h=ifelse(gpp_11h<=0,NA,gpp_11h))
length(unique(df_flux_agg$sitename))

quality_control2 <- df_flux_agg %>% group_by(sitename, year) %>% summarise(gpp_mean=mean(gpp_11h))
quality_control2 %>% filter(is.na(gpp_mean)) %>% summarise(sitename=unique(sitename))

saveRDS(df_flux_agg, "~/phenoEOS/data/fluxnet_sites/df_fluxnet_agg.rds")

