# This script analyses the relationship of CO2 assimilation (simulated using the  P-model)
# and phenological dates from local observations (PEP725 data). Outputs include Figure S2.

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
pep_pmodel <- readRDS("~/phenoEOS/data/pep_pmodel_outputs.rds") #11.2h
pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = gpp - rd, 
         lue = gpp / apar)

# join both datasets
df_pep <- df_pep %>% 
  left_join(pep_pmodel)

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
r.squaredGLMM(fit_iav_pep_off_vs_gppnet)
plot(allEffects(fit_iav_pep_off_vs_gppnet))
parres11 <- partialize(fit_iav_pep_off_vs_gppnet,"gpp_net")
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet <- ggplot_iav_off_gppnet(out_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
out <- summary(fit_lt_pep_off_vs_gppnet_year)
estimate <- out$coefficients[,"Estimate"]
CI <- out$coefficient[,"Std. Error"]*1.96
r.squaredGLMM(fit_lt_pep_off_vs_gppnet_year)
plot(allEffects(fit_lt_pep_off_vs_gppnet_year))
parres12 <- partialize(fit_lt_pep_off_vs_gppnet_year,"gpp_net")
parres13 <- partialize(fit_lt_pep_off_vs_gppnet_year,"year")
out_pep_off_vs_gppnet_year <- allEffects(fit_lt_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet <- ggplot_lt_off_gppnet(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_year <- ggplot_lt_off_gppnet_year(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet + gg_lt_pep_off_vs_gppnet_year

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_gppnet, fit_lt_pep_off_vs_gppnet_year)
out_anova

## Supplementary Fig. S2
ff_lt_pep_off_vs_gppnet_year <- gg_lt_pep_off_vs_gppnet_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), subtitle = "PEP data and P-model") + 
  theme(legend.position = "none")

ff_lt_pep_off_vs_gppnet <- gg_lt_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), subtitle = "PEP data and P-model") + 
  theme(legend.position = "none")

ff_iav_pep_off_vs_gppnet <- gg_iav_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), subtitle = "PEP data and P-model") +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.15, .25),
        legend.direction="vertical",
        legend.margin = margin(.2, .2, .2, .2),
        legend.key.size = unit(.6, 'lines')) 

ss2 <- ff_lt_pep_off_vs_gppnet_year + ff_lt_pep_off_vs_gppnet + ff_iav_pep_off_vs_gppnet + plot_annotation(tag_levels = 'A')
ss2 
ggsave("~/phenoEOS/manuscript/figures/fig_S2.png", width = 8, height = 3, dpi=300)
ggsave("~/phenoEOS/manuscript/figures/fig_S2_rev.png", width = 9, height = 3.5, dpi=300)

