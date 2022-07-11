# This script runs the p-model for the selected sites given by the flux measurements.

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(knitr)
devtools::install_github("stineb/LSD")
devtools::install_github("computationales/rsofun")
install.packages("rpmodel")
library(rsofun)

# load data
# from sofunCalVal repository (https://github.com/computationales/sofunCalVal/blob/main/data/df_drivers_fluxnet2015_allsites.rda)
load("~/phenoEOS/data/fluxnet_sites/p_model/df_drivers_fluxnet2015.rda")
df_drivers_fluxnet2015 %>% n_distinct() #129

# select sites within the FLUXNET2015
fluxnet_refs <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_refs.csv")
fluxnet_refs %>% n_distinct() #32

# sites for which there are drivers
df_drivers_fluxnet2015 %>% dplyr::filter(sitename %in% fluxnet_refs$sitename) %>% n_distinct() #23

# convert data to adhere to new p-model naming conventions
drivers <- df_drivers_fluxnet2015 %>%
  dplyr::select(sitename, forcing) %>% 
  unnest(forcing) %>% 
  dplyr::filter(!(month(date)==2 & mday(date)==29)) %>% 
  
  ## model requires flux per seconds now
  mutate(
    prec = prec / (60*60*24),
    ppfd = ppfd / (60*60*24),
    rain = prec,
    snow = 0,
    tmin = temp,
    tmax = temp
  ) %>% 
  
  group_by(sitename) %>% 
  nest() %>%
  rename(forcing = data) %>% 
  right_join(
    df_drivers_fluxnet2015 %>% 
      dplyr::select(-forcing),
    by = "sitename"
  ) %>% 
  ungroup() %>% 
  rename(
    site_info = siteinfo,
    params_soil = df_soiltexture
  )

# filter sites selected
drivers <- drivers %>%
  dplyr::filter(sitename %in% fluxnet_refs$sitename) 

# set p-model settings
params_modl <- list(
  kphio           = 0.09423773,
  soilm_par_a     = 0.33349283,
  soilm_par_b     = 1.45602286,
  tau_acclim_tempstress = 10,
  par_shape_tempstress  = 0.0
)

# run the model
output <- rsofun::runread_pmodel_f(
  drivers,
  par = params_modl
)

# check results
output %>%
  tidyr::unnest(c(data))

model_output <- output %>%
  tidyr::unnest(c(site_info, data))

# prepare variables 
model_output <-  model_output %>% 
  mutate(year=lubridate::year(date),
         doy=lubridate::yday(date),
         daylength=daylength(lat, doy))
length(unique(model_output$sitename))
saveRDS(model_output, "~/phenoEOS/data/fluxnet_sites/p_model/model_output.rds")
