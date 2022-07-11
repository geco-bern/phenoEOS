
library(tidyverse)
library(dplyr)
library(lubridate)

setwd("/cluster/work/climate/bestocke/data/pep_pmodel_output/pep725")
mod <- list.files(pattern = "*.rds") %>%
  map(readRDS) %>%
  bind_rows() %>% 
  as_tibble() %>% 
  dplyr::select(sitename,lat,lon,year,doy,daylength,gpp,rd)

# read data pep LPJ-GUESS
df_pep <- read.csv("/cluster/work/climate/bestocke/data/pep/processed/DataMeta_3_Drivers_20_11_10.csv",sep=";")
df_pep <- df_pep %>% 
  as_tibble() %>% 
  dplyr::select(timeseries,LON,LAT,YEAR,DoY_off,DoY_out) %>%
  dplyr::rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out,
                sitename = timeseries)

# join both datasets
df_pep <- df_pep %>% 
  left_join(mod)

# Option 2 cutoff
daylength_cutoff <- 11.2

df_out <- df_pep %>%
  mutate(Anet_pmodel = ifelse(!is.na(gpp) & (doy < on | daylength <= daylength_cutoff), 0, gpp)) %>%
  mutate(rd_pmodel = ifelse(!is.na(gpp) & (doy < on | daylength <= daylength_cutoff), 0, rd)) 

pep_check <- df_out[1:365,]
saveRDS(pep_check, "/cluster/work/climate/bestocke/data/pep_pmodel_output/outputs/pep_check_112h.rds")

df_pmodel_Anet <- df_out %>% 
  group_by(sitename, lat, lon, year) %>% 
  summarise(
    Anet_pmodel = sum(Anet_pmodel),
    rd_pmodel = sum(rd_pmodel)
    ) 

saveRDS(df_pmodel_Anet, "/cluster/work/climate/bestocke/data/pep_pmodel_output/outputs/pep_pmodel_Anet.rds")
