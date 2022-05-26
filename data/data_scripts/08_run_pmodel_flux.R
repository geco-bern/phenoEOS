
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
fluxnet2015 <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_metadata.csv")
fluxnet2015 <- fluxnet2015 %>% filter(LOCATION_LAT>=10,IGBP=="DBF"|IGBP=="DNF"|IGBP=="MF") %>% 
  rename(sitename=SITE_ID,lat=LOCATION_LAT,lon=LOCATION_LONG,ele=LOCATION_ELEV)
fluxnet2015 %>% n_distinct() #32

# sites for which there are drivers
df_drivers_fluxnet2015 %>% dplyr::filter(sitename %in% fluxnet2015$sitename) %>% n_distinct() #23

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
  dplyr::filter(sitename %in% fluxnet2015$sitename) 

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

model_data <- output %>%
  tidyr::unnest(c(site_info, data))
str(model_data)

ggplot() + geom_line(data = model_data, aes(date, gpp),colour = "red") 

# prepare variables 
model_data <-  model_data %>% 
  mutate(year=lubridate::year(date),
         doy=lubridate::yday(date),
         daylength=daylength(lat, doy))
length(unique(model_data$sitename))

# aggregate gpp data summing up to the cutoff
# Option 1 cutoff
daylength_cutoff <- 11.2

df_out_11h <- model_data %>%
  mutate(gpp   = ifelse(daylength <= daylength_cutoff, 0, gpp))

df_out_11h <- df_out_11h %>% 
  group_by(sitename, lat, lon, year) %>% 
  summarise(
    gpp = sum(gpp)
  )
saveRDS(df_out_11h, "~/phenoEOS/data/fluxnet_sites/p_model/flux_pmodel_11h_output.rds")

# Option 2 cutoff
#doy_cutoff <- lubridate::yday("2001-06-21") 

#df_out_21J <- model_data %>%
#  mutate(gpp   = ifelse(doy >= doy_cutoff, 0, gpp))

#df_out_21J <- df_out_21J %>% 
#  group_by(sitename, lat, lon, year) %>% 
#  summarise(
#    gpp = sum(gpp)
#  )
#saveRDS(df_out_21J, "~/phenoEOS/data/flux_sites/p-model/p_model_drivers/flux_pmodel_21J_output.rds")

