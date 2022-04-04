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
library(jtools)

# read data
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

length(unique(df_pep$id_site)) #3855

# Interannual variation (IAV)
# EOS ~ Anet LPJ-GUESS
fit_iav_pep_off_vs_cAtot = lmer(off ~ scale(cA_tot) + (1|id_site) + (1|species) , data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_cAtot)
r.squaredGLMM(fit_iav_pep_off_vs_cAtot)
plot(allEffects(fit_iav_pep_off_vs_cAtot))
#plot_model(fit_iav_pep_off_vs_cAtot, type = "pred",show.data=T,terms = c("cA_tot"))
#visreg(fit_iav_pep_off_vs_cAtot, "cA_tot") 
parres1 <- partialize(fit_iav_pep_off_vs_cAtot,"cA_tot") # calculate partial residuals
out_iav_pep_off_vs_cAtot <- allEffects(fit_iav_pep_off_vs_cAtot,partial.residuals=T)
str(out_iav_pep_off_vs_cAtot)
gg_iav_pep_off_vs_cAtot <- ggplot_iav_off_catot(out_iav_pep_off_vs_cAtot)
gg_iav_pep_off_vs_cAtot 

# Long-term trends
# EOS ~ Anet LPJ + Year 
fit_lt_pep_off_vs_cAtot_year = lmer(off ~ scale(cA_tot) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_cAtot_year)
r.squaredGLMM(fit_lt_pep_off_vs_cAtot_year)
plot(allEffects(fit_lt_pep_off_vs_cAtot_year))
#visreg(fit_lt_pep_off_vs_cAtot_year, "cA_tot")
#visreg(fit_lt_pep_off_vs_cAtot_year, "year")
parres2 <- partialize(fit_lt_pep_off_vs_cAtot_year,"cA_tot")
parres3 <- partialize(fit_lt_pep_off_vs_cAtot_year,"year")
out_lt_pep_off_vs_cAtot_year <- allEffects(fit_lt_pep_off_vs_cAtot_year,partial.residuals = TRUE)
str(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot <- ggplot_lt_off_catot(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_year <- ggplot_lt_off_catot_year(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot + gg_lt_pep_off_vs_year + plot_layout(guides = "collect") & theme(legend.position = 'right')

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_cAtot, fit_lt_pep_off_vs_cAtot_year)
out_anova

# Figure 1
ff_lt_pep_off_vs_year <- gg_lt_pep_off_vs_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), 
       subtitle = "PEP data and LPJ") +
  theme(legend.position = "none") 

ff_lt_pep_off_vs_cAtot <- gg_lt_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), 
       subtitle = "PEP data and LPJ") +
  theme(legend.position = "none") 

ff_iav_pep_off_vs_cAtot <- gg_iav_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), 
       subtitle = "PEP data and LPJ") +
  theme(plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .95),
        legend.direction="vertical",
        legend.margin = margin(.2, .2, .2, .2),
        legend.key.size = unit(.6, 'lines')) 

pp1 <- ff_lt_pep_off_vs_year + ff_lt_pep_off_vs_cAtot + ff_iav_pep_off_vs_cAtot
pp1 + plot_annotation(tag_levels = 'A') #+ plot_layout(guides = "collect") & theme(legend.position = 'left')
ggsave("~/phenoEOS/manuscript/figures/fig_1.png", width = 8, height = 3, dpi=300)
ggsave("~/phenoEOS/manuscript/figures/fig_1_rev.png", width = 9, height = 3.5, dpi=300)

