# This script reads the gpp data from flux observations sites and select the data for deciduous forests.

# load packages
library(dplyr)
library(tidyverse)
library(amerifluxr)
library(geosphere)
library(bigleaf)
library(rnaturalearth)

# read all sites from the FLUXNET2015
fluxnet_metadata <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_metadata.csv")
fluxnet_citations <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_citations.csv")
fluxnet <- fluxnet_metadata %>% left_join(fluxnet_citations[,c(1,8)])

# filter those sites from the Northern Hemisphere and those classified as deciduous (DBF, DNF) and mixed forests (MF)
fluxnet_refs <- fluxnet %>% filter(LOCATION_LAT>=10,IGBP=="DBF"|IGBP=="DNF"|IGBP=="MF") %>% 
  rename(sitename=SITE_ID,lat=LOCATION_LAT,lon=LOCATION_LONG,ele=LOCATION_ELEV)
fluxnet_refs %>% n_distinct() #32
write.csv(fluxnet_refs,"~/phenoEOS/data/fluxnet_sites/fluxnet_refs.csv")

# read files for the selected sites at daily (DD) resolution
setwd("~/phenoEOS/data/fluxnet_sites/FLUXNET2015")
fluxnet_sites <- list.files(pattern = "*.csv") %>%
  purrr::map(read.csv) %>% 
  bind_rows()

# join with metadata from sites
fluxnet_data <- fluxnet_sites %>% rename(sitename=site) %>% left_join(fluxnet_refs[c(1,5,6,7,8)]) %>% 
  rename(gpp=GPP_NT_VUT_REF)

# prepare variables 
fluxnet_data <-  fluxnet_data %>% select(c(1:3,359,360,361,362)) %>%
  mutate(gpp = na_if(gpp, -9999),
         date=lubridate::ymd(TIMESTAMP),
         year=lubridate::year(date),
         doy=lubridate::yday(date),
         daylength=daylength(lat, doy))

length(unique(fluxnet_data$sitename))
unique(fluxnet_data$IGBP)
saveRDS(fluxnet_data, "~/phenoEOS/data/fluxnet_sites/fluxnet_data.rds")
