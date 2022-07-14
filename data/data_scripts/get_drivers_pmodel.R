
# Libraries, check for Euler access ----
library(dplyr)
library(tidyverse)
library(rsofun)
library(ingestr)

# . prepare sites that are not in the siteinfo_fluxnet2015
# fluxnet_sites <- ingestr::siteinfo_fluxnet2015 
# 
# fluxnet_refs <- read.csv("~/phenoEOS/data/fluxnet_sites/fluxnet_refs.csv")
# fluxnet_refs %>% n_distinct() #32
# fluxnet_refs$sitename
# # fluxnet_sub <-fluxnet_refs %>% filter(sitename=="CA-Gro"|sitename=="CA-Oas"|sitename=="CA-TPD"|sitename=="DE-Lnf"|sitename=="IT-Ro1"|sitename=="RU-SkP"|
# #                                         sitename=="US-Oho"|sitename=="US-Wi1"|sitename=="US-Wi8") %>%
# #   select(sitename,lon,lat,ele) %>% rename(elv=ele)
# fluxnet_refs <- fluxnet_refs %>% select(sitename,lon,lat,ele) %>% rename(elv=ele)
# 
# fluxnet_sites <- fluxnet_sites %>%
#   dplyr::filter(sitename %in% fluxnet_refs$sitename) 
# fluxnet_refs <- fluxnet_refs %>%
#   dplyr::filter(!sitename %in% fluxnet_sites$sitename) 
# 
# fluxnet_sites <- fluxnet_sites %>% bind_rows(fluxnet_refs) %>% mutate(year_start=2000, year_end=2014)
# 
# saveRDS(fluxnet_sites, "~/phenoEOS/data/fluxnet_sites/p_model/fluxnet_sites.rds")

# load selected sites info
#fluxnet_sites <- readRDS("~/phenoEOS/data/fluxnet_sites/p_model/fluxnet_sites.rds")
# If run in cluster:
fluxnet_sites <- readRDS("/cluster/home/lmarques/fluxnet_sites.rds")

# grab fluxnet data
df_fluxnet <-
  suppressWarnings(
    suppressMessages(
      ingestr::ingest(
        siteinfo  = fluxnet_sites,
        source    = "fluxnet",
        getvars   = list(
          temp = "TA_F_DAY",
          prec = "P_F",
          vpd  = "VPD_F_DAY",
          ppfd = "SW_IN_F",
          patm = "PA_F"),
          #dir       = "~/data/FLUXNET-2015_Tier1/20191024/pheno_sites/",
          dir       = "/cluster/home/lmarques/pheno_sites/",
          settings  = list(
          #dir_hh = "~/data/FLUXNET-2015_Tier1/20191024/pheno_sites/", getswc = FALSE),
          dir_hh = "/cluster/home/lmarques/pheno_sites/", getswc = FALSE),
         timescale = "d"
      )
    )
  )

# get CRU data to complement fluxnet data 
df_cru <- ingestr::ingest(
  siteinfo  = fluxnet_sites,
  source    = "cru",
  getvars   = "ccov",
  #dir       = "~/data/cru/ts_4.01/"
  dir       = "/cluster/work/climate/bestocke/data/cru/ts_4.01/"
)

# merge data into one "meteo" data frame 
df_meteo <- df_fluxnet %>%
  tidyr::unnest(data) %>%
  left_join(
    df_cru %>%
      tidyr::unnest(data),
    by = c("sitename", "date")
  ) %>%
  group_by(sitename) %>%
  tidyr::nest()

# grab MODIS FPAR data 
settings_modis <- get_settings_modis(
  bundle            = "modis_fpar",
  data_path         = "~/data/modis_fapar/",
  method_interpol   = "loess",
  keep              = TRUE,
  overwrite_raw     = FALSE,
  overwrite_interpol= TRUE,
  network           = "FLUXNET"
)

df_modis_fpar <- ingest(
  fluxnet_sites, 
  source = "modis",
  settings = settings_modis, 
  parallel = FALSE
)

# grab CO2 data 
df_co2 <- ingestr::ingest(
  fluxnet_sites,
  source  = "co2_mlo",
  verbose = FALSE
)

# set soil parameters
df_soiltexture <- bind_rows(
  top    = tibble(
    layer = "top",
    fsand = 0.4,
    fclay = 0.3,
    forg = 0.1,
    fgravel = 0.1
  ),
  bottom = tibble(
    layer = "bottom",
    fsand = 0.4,
    fclay = 0.3,
    forg = 0.1,
    fgravel = 0.1)
)

# set simulation parameters
params_siml <- list(
  spinup             = TRUE,
  spinupyears        = 10,
  recycle            = 1,
  soilmstress        = TRUE,
  tempstress         = TRUE,
  calc_aet_fapar_vpd = FALSE,
  in_ppfd            = TRUE,
  in_netrad          = FALSE,
  outdt              = 1,
  ltre               = FALSE,
  ltne               = FALSE,
  ltrd               = FALSE,
  ltnd               = FALSE,
  lgr3               = TRUE,
  lgn3               = FALSE,
  lgr4               = FALSE
)

# combine all data into the rsofun driver data format
p_model_fluxnet_drivers <- rsofun::collect_drivers_sofun(
  site_info       = fluxnet_sites,
  params_siml    = params_siml,
  meteo          = df_meteo,
  fapar          = df_modis_fpar,
  co2            = df_co2,
  params_soil    = df_soiltexture
)

saveRDS(p_model_fluxnet_drivers, "/cluster/home/lmarques/p_model_fluxnet_drivers.rds")
