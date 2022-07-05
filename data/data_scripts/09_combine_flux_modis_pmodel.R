# This script join gpp flux measurements, phenology dates from the MODIS and gpp p-model simulations

# load packages
library(dplyr)
library(lubridate)
library(geosphere)

# read flux data
df_fluxnet_agg <- readRDS("~/phenoEOS/data/fluxnet_sites/df_fluxnet_agg.rds")
df_fluxnet_agg <- df_fluxnet_agg %>% rename(gpp_flux=gpp_11h)
length(unique(df_fluxnet_agg$sitename)) 

# read pheno modis data for the selected flux sites
# this data has been downloaded using MODISTools (can be also done using ingestr) (from the product MCD12Q2)
df_modis_pheno_flux <- readRDS("~/phenoEOS/data/fluxnet_sites/df_modis_pheno_flux.rds")
length(unique(df_modis_pheno_flux$sitename)) 
df_modis_pheno_flux <- df_modis_pheno_flux %>% select(sitename,year,SOS_1_date,SOS_1_doy,SOS_2_date,
                                            SOS_2_doy,EOS_1_date,EOS_1_doy,EOS_2_date,EOS_2_doy)

# read p-model simulations
df_out_11h <- readRDS("~/phenoEOS/data/fluxnet_sites/p_model/flux_pmodel_11h_output.rds")
df_out_11h <- df_out_11h %>% rename(gpp_pmodel=gpp)

# join the three datasets
data_flux_modis_pmodel <- df_fluxnet_agg %>% left_join(df_modis_pheno_flux) %>% left_join(df_out_11h)
length(unique(data_flux_modis_pmodel$sitename))
saveRDS(data_flux_modis_pmodel, "~/phenoEOS/data/data_flux_modis_pmodel.rds")
saveRDS(data_flux_modis_pmodel, "~/phenoEOS/data/data_flux_modis_pmodel2.rds")

table(df_fluxnet_agg$year)
table(df_modis_pheno_flux$year)
table(df_out_11h$year)
table(df_fluxnet_agg$sitename)
table(df_modis_pheno_flux$sitename)
table(df_out_11h$sitename)
