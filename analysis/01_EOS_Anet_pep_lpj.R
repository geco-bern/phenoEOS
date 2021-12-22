# This script analyses the relationship of CO2 assimilation (simulated using the LPJ-GUESS model)
# and phenological dates from local observations (PEP725 data). Outputs include Figure 1.

# load packages
library(dplyr)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)

# read data
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries)

# Interannual variation (IAV)
# EOS ~ Anet LPJ-GUESS
fit_iav_pep_off_vs_cAtot = lmer(off ~ scale(cA_tot) + (1|id_site) + (1|species) , data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_cAtot)
r.squaredGLMM(fit_iav_pep_off_vs_cAtot)
plot(allEffects(fit_iav_pep_off_vs_cAtot))
out_iav_pep_off_vs_cAtot <- allEffects(fit_iav_pep_off_vs_cAtot)
gg_iav_pep_off_vs_cAtot <- ggplot_cA_tot(out_iav_pep_off_vs_cAtot)
gg_iav_pep_off_vs_cAtot

# Long-term trends
# EOS ~ Anet LPJ + Year 
fit_lt_pep_off_vs_cAtot_year = lmer(off ~ scale(cA_tot) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_cAtot_year)
r.squaredGLMM(fit_lt_pep_off_vs_cAtot_year)
plot(allEffects(fit_lt_pep_off_vs_cAtot_year))
out_lt_pep_off_vs_cAtot_year <- allEffects(fit_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot <- ggplot_cA_tot(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_year2 <- ggplot_year(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot + gg_lt_pep_off_vs_year2

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_cAtot, fit_lt_pep_off_vs_cAtot_year)
out_anova

# Figure 1
ff_lt_pep_off_vs_year2 <- gg_lt_pep_off_vs_year2 +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), 
       subtitle = "PEP data and LPJ") +
  scale_y_continuous( limits = c(270,294),breaks = seq(270,290,10)) + 
  scale_x_continuous(limits = c(1950,2021), breaks = seq(1960,2020,20))

ff_lt_pep_off_vs_cAtot <- gg_lt_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), 
       subtitle = "PEP data and LPJ") +
  scale_y_continuous( limits = c(210,365),breaks = seq(240,360,40)) + 
  scale_x_continuous(limits = c(350,2400), breaks = seq(500,2400,500))

ff_iav_pep_off_vs_cAtot <- gg_iav_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), 
       subtitle = "PEP data and LPJ") +
  theme(plot.background = element_rect(fill = "grey")) + 
  scale_y_continuous( limits = c(210,365),breaks = seq(240,360,40)) + 
  scale_x_continuous(limits = c(350,2400), breaks = seq(500,2400,500))

pp1 <- ff_lt_pep_off_vs_year2 + ff_lt_pep_off_vs_cAtot + ff_iav_pep_off_vs_cAtot
pp1 + plot_annotation(tag_levels = 'A')
ggsave("~/phenoEOS/manuscript/figures/fig_1.png", width = 8, height = 3, dpi=300)

