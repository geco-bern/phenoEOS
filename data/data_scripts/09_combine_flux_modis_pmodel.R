# This script join gpp flux measurements, phenology dates from the MODIS and gpp p-model simulations

# load packages
library(dplyr)
library(lubridate)

# Aggregate Anet from flux measurements ####

# read flux data
fluxnet_data <- readRDS("~/phenoEOS/data/fluxnet_sites/fluxnet_data.rds")
str(fluxnet_data)

# read pheno modis data
df_pheno_modis_fluxnet <- readRDS("~/phenoEOS/data/fluxnet_sites/df_pheno_modis_fluxnet.rds")
df_pheno_modis_fluxnet <- df_pheno_modis_fluxnet %>% select(sitename,year,SOS_1_date,SOS_1_doy,SOS_2_date,
                                                      SOS_2_doy,EOS_1_date,EOS_1_doy,EOS_2_date,EOS_2_doy)
# join datasets
fluxnet_pheno_join <- fluxnet_data %>% left_join(df_pheno_modis_fluxnet[,c(1,2,6)])

# aggregate gpp data summing from SOS up to the cutoff
daylength_cutoff <- 11.2

fluxnet_pheno_join <- fluxnet_pheno_join %>%
  mutate(Anet_flux = ifelse(!is.na(gpp) & (doy < SOS_2_doy | daylength <= daylength_cutoff), 0, gpp)) 

df_fluxnet_Anet <- fluxnet_pheno_join %>% 
  group_by(sitename, year) %>% 
  summarise(Anet_flux = sum(Anet_flux,na.rm = F)) %>%
  mutate(Anet_flux=ifelse(Anet_flux<=0,NA,Anet_flux))
length(unique(df_fluxnet_Anet$sitename))
table(df_fluxnet_Anet$year)
sort(table(df_fluxnet_Anet$sitename))

# Aggregate Anet from p-model simulations ####

# read p-model outputs
model_output <- readRDS("~/phenoEOS/data/fluxnet_sites/p_model/pmodel_output.rds")
model_output <- model_output %>% select(sitename,lon,lat,year,doy,daylength,fapar,iwue,gpp,rd)

# read pheno modis data
df_pheno_modis_fluxnet <- readRDS("~/phenoEOS/data/fluxnet_sites/df_pheno_modis_fluxnet.rds")
df_pheno_modis_fluxnet <- df_pheno_modis_fluxnet %>% select(sitename,year,SOS_1_date,SOS_1_doy,SOS_2_date,
                                                            SOS_2_doy,EOS_1_date,EOS_1_doy,EOS_2_date,EOS_2_doy)
# join datasets
pmodel_pheno_join <- model_output %>% left_join(df_pheno_modis_fluxnet[,c(1,2,6)])

# aggregate gpp data summing up to the cutoff
daylength_cutoff <- 11.2

pmodel_pheno_join <- pmodel_pheno_join %>%
  mutate(Anet_pmodel = ifelse(!is.na(gpp) & (doy < SOS_2_doy | daylength <= daylength_cutoff), 0, gpp)) %>%
  mutate(rd_pmodel = ifelse(!is.na(gpp) & (doy < SOS_2_doy | daylength <= daylength_cutoff), 0, rd)) 

df_pmodel_Anet <- pmodel_pheno_join %>% 
  group_by(sitename, lat, lon, year) %>% 
  summarise(Anet_pmodel = sum(Anet_pmodel,na.rm = F),
            rd_pmodel = sum(rd_pmodel,na.rm = F)) %>%
  mutate(Anet_pmodel=ifelse(Anet_pmodel<=0,NA,Anet_pmodel))
length(unique(df_pmodel_Anet$sitename))
table(df_pmodel_Anet$year)
sort(table(df_pmodel_Anet$sitename))

# Join all data fluxnet_pmodel_pheno ####

fluxnet_pmodel_pheno <- df_fluxnet_Anet %>% left_join(df_pmodel_Anet, by = c("sitename", "year")) %>% 
  left_join(df_pheno_modis_fluxnet, by = c("sitename", "year"))
length(unique(fluxnet_pmodel_pheno$sitename))
saveRDS(fluxnet_pmodel_pheno, "~/phenoEOS/data/fluxnet_pmodel_pheno.rds")
table(fluxnet_pmodel_pheno$year)
sort(table(fluxnet_pmodel_pheno$sitename))
