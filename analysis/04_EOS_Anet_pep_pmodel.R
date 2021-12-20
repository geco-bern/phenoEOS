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

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
r.squaredGLMM(fit_iav_pep_off_vs_gppnet)
plot(allEffects(fit_iav_pep_off_vs_gppnet))
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet <- ggplot_gpp_net(out_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
r.squaredGLMM(fit_lt_pep_off_vs_gppnet_year)
plot(allEffects(fit_lt_pep_off_vs_gppnet_year))
out_pep_off_vs_gppnet_year <- allEffects(fit_lt_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet <- ggplot_gpp_net(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_year3 <- ggplot_year(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet + gg_lt_pep_off_vs_year3

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_gppnet, fit_lt_pep_off_vs_gppnet_year)
out_anova

## Supplementary Fig. S2
ff_lt_pep_off_vs_year3 <- gg_lt_pep_off_vs_year3 +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), subtitle = "PEP data and P-model") + 
  scale_y_continuous( limits = c(276,293),breaks = seq(270,300,5)) + 
  scale_x_continuous(limits = c(1950,2021), breaks = seq(1960,2020,20))

ff_lt_pep_off_vs_gppnet <- gg_lt_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), subtitle = "PEP data and P-model") + 
  scale_y_continuous( limits = c(273,300),breaks = seq(270,300,10)) + 
  scale_x_continuous(limits = c(240,1400), breaks = seq(500,1000,500))

ff_iav_pep_off_vs_gppnet <- gg_iav_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), subtitle = "PEP data and P-model") +
  theme(plot.background = element_rect(fill = "grey")) + 
  scale_y_continuous( limits = c(278,291),breaks = seq(280,290,5)) + 
  scale_x_continuous(limits = c(200,1400), breaks = seq(500,1000,500))

ss2 <- ff_lt_pep_off_vs_year3 + ff_lt_pep_off_vs_gppnet + ff_iav_pep_off_vs_gppnet
ss2 + plot_annotation(tag_levels = 'A')
ggsave("~/phenoEOS/manuscript/figures/fig_S2.png", width = 8, height = 3, dpi=300)

