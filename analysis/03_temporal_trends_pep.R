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
library(jtools)

# load functions for plots
source("~/phenoEOS/analysis/00_load_functions_data.R")

# read data pep LPJ-GUESS
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# read data pep P-model
pep_pmodel <- readRDS("~/phenoEOS/data/pep_pmodel_Anet.rds") #11.2h
pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>%
  mutate(gpp_net=ifelse(gpp_net==0, NA, gpp_net))

df_pep <- df_pep %>% 
  left_join(pep_pmodel)

# EOS ~ Year
fit_lt_pep_off_vs_year <- lmer(off ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_year)
out <- summary(fit_lt_pep_off_vs_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_off_vs_year)
plot(allEffects(fit_lt_pep_off_vs_year))
parres7 <- partialize(fit_lt_pep_off_vs_year,"year")
out_lt_pep_off_vs_year <- allEffects(fit_lt_pep_off_vs_year)
gg_lt_pep_off_vs_year <- ggplot_off_year(out_lt_pep_off_vs_year)
gg_lt_pep_off_vs_year
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)

# Anet P-model ~ Year
fit_lt_pep_gppnet_vs_year <- lmer(gpp_net ~ scale(year) + (1|id_site) + (1|species), data = df_pep, REML = FALSE, na.action = "na.exclude")
summary(fit_lt_pep_gppnet_vs_year)
out <- summary(fit_lt_pep_gppnet_vs_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_gppnet_vs_year)
plot(allEffects(fit_lt_pep_gppnet_vs_year))
parres10 <- partialize(fit_lt_pep_gppnet_vs_year,"year")
out_lt_pep_gppnet_vs_year <- allEffects(fit_lt_pep_gppnet_vs_year)
gg_lt_pep_gppnet_vs_year <- ggplot_gppnet_year(out_lt_pep_gppnet_vs_year)
gg_lt_pep_gppnet_vs_year
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)

# Anet LPJ-GUESS ~ Year
fit_lt_pep_cAtot_vs_year <- lmer(cA_tot ~ scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_cAtot_vs_year)
out <- summary(fit_lt_pep_cAtot_vs_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_cAtot_vs_year)
plot(allEffects(fit_lt_pep_cAtot_vs_year))
parres9 <- partialize(fit_lt_pep_cAtot_vs_year,"year")
out_lt_pep_cAtot_vs_year <- allEffects(fit_lt_pep_cAtot_vs_year)
gg_lt_pep_cAtot_vs_year <- ggplot_catot_year(out_lt_pep_cAtot_vs_year)
gg_lt_pep_cAtot_vs_year
# Unscaled
#trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
#error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)

# SOS ~ Year
fit_lt_pep_on_vs_year <- lmer(on ~ scale(year) + (1|id_site) + (1|species), data = df_pep,REML = F, na.action = "na.exclude")
summary(fit_lt_pep_on_vs_year)
out <- summary(fit_lt_pep_on_vs_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_on_vs_year)
plot(allEffects(fit_lt_pep_on_vs_year))
parres8 <- partialize(fit_lt_pep_on_vs_year,"year")
out_lt_pep_on_vs_year <- allEffects(fit_lt_pep_on_vs_year)
gg_lt_pep_on_vs_year <- ggplot_on_year(out_lt_pep_on_vs_year)
gg_lt_pep_on_vs_year
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)

# Supplementary Fig. S1
ff_lt_pep_off_vs_year <- gg_lt_pep_off_vs_year +
  labs(title = "EOS ~ Year", subtitle = "PEP data") +
  theme(legend.position = "none",plot.subtitle=element_text(size=10))

ff_lt_pep_gppnet_vs_year <- gg_lt_pep_gppnet_vs_year +
  labs(title = expression(paste(italic("A")[net], " ~ Year")), subtitle = "PEP data and P-model",
       y = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), x = "Year") +
  theme(legend.position = "none",plot.subtitle=element_text(size=10))

ff_lt_pep_cAtot_vs_year <- gg_lt_pep_cAtot_vs_year +
  labs(title = expression(paste(italic("A")[net], " ~ Year")), subtitle = "PEP data and LPJ model",
       y = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), x = "Year") +
  theme(legend.position = "none",plot.subtitle=element_text(size=10))

ff_lt_pep_on_vs_year <- gg_lt_pep_on_vs_year +
  labs(title = "SOS ~ Year", subtitle = "PEP data",
       x = "Year", y = "SOS (DOY)") +
  theme(legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .95),
        legend.direction="vertical",
        legend.margin = margin(.2, .2, .2, .2),
        legend.key.size = unit(.6, 'lines'),plot.subtitle=element_text(size=10))

figS1 <- (ff_lt_pep_off_vs_year + ff_lt_pep_gppnet_vs_year)/(ff_lt_pep_cAtot_vs_year + ff_lt_pep_on_vs_year) + plot_annotation(tag_levels = 'A')
figS1 
ggsave("~/phenoEOS/manuscript/figures/fig_S1_rev.png", width = 7.5, height = 7.5, dpi=300)
