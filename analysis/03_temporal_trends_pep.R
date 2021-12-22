# This script analyses the temporal trends of CO2 assimilation (estimations from both LPJ-GUESS and P-model) 
# and phenological dates from local observations (PEP725 data). Outputs include Figure S1.

# load packages
library(dplyr)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)

# read data pep LPJ-GUESS
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries)

# read data pep P-model
pep_pmodel <- readRDS("~/phenoEOS/data/pep_pmodel_outputs.rds")
pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = gpp - rd, 
         lue = gpp / apar)

# join both datasets
df_pep <- df_pep %>% 
  left_join(pep_pmodel)

# EOS ~ Year
fit_lt_pep_off_vs_year <- lmer(off ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_year)
r.squaredGLMM(fit_lt_pep_off_vs_year)
plot(allEffects(fit_lt_pep_off_vs_year))
out_lt_pep_off_vs_year <- allEffects(fit_lt_pep_off_vs_year)
gg_lt_pep_off_vs_year <- ggplot_year(out_lt_pep_off_vs_year)
gg_lt_pep_off_vs_year

# SOS ~ Year
fit_lt_pep_on_vs_year <- lmer(on ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_on_vs_year)
r.squaredGLMM(fit_lt_pep_on_vs_year)
plot(allEffects(fit_lt_pep_on_vs_year))
out_lt_pep_on_vs_year <- allEffects(fit_lt_pep_on_vs_year)
gg_lt_pep_on_vs_year <- ggplot_year(out_lt_pep_on_vs_year)
gg_lt_pep_on_vs_year

# Anet LPJ-GUESS ~ Year
fit_lt_pep_cAtot_vs_year <- lmer(cA_tot ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_cAtot_vs_year)
r.squaredGLMM(fit_lt_pep_cAtot_vs_year)
plot(allEffects(fit_lt_pep_cAtot_vs_year))
out_lt_pep_cAtot_vs_year <- allEffects(fit_lt_pep_cAtot_vs_year)
gg_lt_pep_cAtot_vs_year <- ggplot_year(out_lt_pep_cAtot_vs_year)
gg_lt_pep_cAtot_vs_year

# Anet P-model ~ Year
fit_lt_pep_gppnet_vs_year <- lmer(gpp_net ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_gppnet_vs_year)
r.squaredGLMM(fit_lt_pep_gppnet_vs_year)
plot(allEffects(fit_lt_pep_gppnet_vs_year))
out_lt_pep_gppnet_vs_year <- allEffects(fit_lt_pep_gppnet_vs_year)
gg_lt_pep_gppnet_vs_year <- ggplot_year(out_lt_pep_gppnet_vs_year)
gg_lt_pep_gppnet_vs_year

# Supplementary Fig. S1
ff_lt_pep_off_vs_year <- gg_lt_pep_off_vs_year +
  labs(title = "EOS ~ Year", subtitle = "PEP data")

ff_lt_pep_cAtot_vs_year <- gg_lt_pep_cAtot_vs_year +
  labs(title = expression(paste(italic("A")[net], " ~ Year")), subtitle = "PEP data and LPJ",
       y = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), x = "Year")

ff_lt_pep_gppnet_vs_year <- gg_lt_pep_gppnet_vs_year +
  labs(title = expression(paste(italic("A")[net], " ~ Year")), subtitle = "PEP data and P-model",
       y = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), x = "Year")

ff_lt_pep_on_vs_year <- gg_lt_pep_on_vs_year +
  labs(title = "SOS ~ Year", subtitle = "PEP data",
       x = "Year", y = "SOS (DOY)")

ss1 <- (ff_lt_pep_off_vs_year + ff_lt_pep_cAtot_vs_year)/(ff_lt_pep_gppnet_vs_year + ff_lt_pep_on_vs_year)
ss1 + plot_annotation(tag_levels = 'A')
ggsave("~/phenoEOS/manuscript/figures/fig_S1.png", width = 7.5, height = 7.5, dpi=300)

