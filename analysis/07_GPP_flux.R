# This script analyses the relationship of CO2 assimilation (GPP flux data)
# and phenological dates from 

# load packages
library(dplyr)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)
library(sjPlot)
library(jtools)

# read site info
flux_data_kit_site <- readRDS("~/phenoEOS/data/flux_data_kit_site-info.rds")
str(flux_data_kit_site)
unique(flux_data_kit_site$igbp_land_use)
length(unique(flux_data_kit_site$sitename))

# read gpp flux data
site_based_drivers_HH <- readRDS("~/phenoEOS/data/site_based_drivers_HH.rds")

# Select sites for deciduous forest
deciduous_sites <- flux_data_kit_site %>% filter(igbp_land_use=="Deciduous Broadleaf Forest")
length(unique(deciduous_sites$sitename)) #48

# Select gpp flux data for the filtered deciduous sites
deciduous_flux <- site_based_drivers_HH %>% filter(sitename %in% deciduous_sites$sitename)
length(unique(deciduous_flux$sitename)) #14
deciduous_flux_sites <- deciduous_sites %>% filter(sitename %in% deciduous_flux$sitename)
saveRDS(deciduous_flux_sites, "~/phenoEOS/data/deciduous_flux_sites.rds")

# Read pheno modis data for the selected flux sites
# This data has been downloaded using ingestr (from the product MCD12Q2)
modis_flux_pheno_sites <- readRDS("~/phenoEOS/data/modis_flux_pheno_sites.rds")



