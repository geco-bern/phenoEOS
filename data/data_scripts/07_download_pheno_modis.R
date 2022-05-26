# This script downloads the phenology dates from the MODIS product MCD12Q2 and for the 
# sites given by the flux measurements.

# load packages
library(MODISTools)
library(tidyverse)
library(lubridate)

## List available MODIS products
products <- mt_products()

## List available bands for a product
bands <- mt_bands(product = "MCD12Q2")

# read flux sites
#df_flux_sites <- readRDS("~/phenoEOS/data/flux_sites/df_flux_sites.rds")

fluxnet2015 <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_metadata.csv")
fluxnet2015 <- fluxnet2015 %>% filter(LOCATION_LAT>=10,IGBP=="DBF"|IGBP=="DNF"|IGBP=="MF") %>% 
  rename(sitename=SITE_ID,lat=LOCATION_LAT,lon=LOCATION_LONG,ele=LOCATION_ELEV)
length(unique(fluxnet2015$sitename))
df_flux_sites <- fluxnet2015

# run mt_subset fc for more than one site 
# See that the result of phenological dates is recorded in the variable value, express as POSIXt, i.e., # days since 01/01/1970
df_modistools <- data.frame() 

for(i in 1:nrow(df_flux_sites)) {
  
  df_modistools_sub <- mt_subset(
    product = "MCD12Q2",
    lat = df_flux_sites$lat[i],
    lon = df_flux_sites$lon[i],
    band = c("Greenup.Num_Modes_01","Dormancy.Num_Modes_01",
             "MidGreenup.Num_Modes_01","MidGreendown.Num_Modes_01"),
    start = "2001-01-01",
    end = "2018-01-01",
    km_lr = 0,
    km_ab = 0,
    site_name = df_flux_sites$sitename[i],
    internal = TRUE,
    progress = FALSE)
  
  df_modistools <- rbind(df_modistools, df_modistools_sub)
 
} 

# save data
saveRDS(df_modistools, "~/phenoEOS/data/fluxnet_sites/df_modistools_fluxnet.rds")

# read data
df_modistools <- readRDS("~/phenoEOS/data/fluxnet_sites/df_modistools_fluxnet.rds")

# Convert from long to wide format
df_modistools_wide <- df_modistools %>%
  pivot_wider(names_from = band, values_from = value)
length(unique(df_modistools_wide$site))

# Convert value output to DOY
df_pheno_modis <- df_modistools_wide %>%
  mutate(year=year(calendar_date)) %>%
  # drop fill values (not relevant for Northern Hemisphere)
  dplyr::filter(Greenup.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(Dormancy.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(MidGreenup.Num_Modes_01 <= 32766) %>% 
  dplyr::filter(MidGreendown.Num_Modes_01 <= 32766) %>% 
  mutate(
    SOS_1_date = as_date(Greenup.Num_Modes_01), 
    SOS_1_doy = yday(SOS_1_date)
  ) %>%
  mutate(
    SOS_2_date = as_date(MidGreenup.Num_Modes_01), 
    SOS_2_doy = yday(SOS_2_date)
  ) %>% 
  mutate(
    EOS_1_date = as_date(Dormancy.Num_Modes_01), 
    EOS_1_doy = yday(EOS_1_date)
  ) %>%
  mutate(
    EOS_2_date = as_date(MidGreendown.Num_Modes_01), 
    EOS_2_doy = yday(EOS_2_date)
  ) %>%
  relocate(site) %>%
  rename(sitename=site)

length(unique(df_pheno_modis$sitename))

# save data
saveRDS(df_pheno_modis, "~/phenoEOS/data/fluxnet_sites/df_modis_pheno_flux.rds")
