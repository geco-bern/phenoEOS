# This script analyses the relationship of spring and autumn phenological dates 
# from local observations (PEP725 data). Outputs include Figure S3.

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

# read data
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# Interannual variation (IAV)
# EOS ~ SOS
fit_iav_pep_off_vs_on = lmer(off ~ scale(on) + (1|id_site) + (1|species) , data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_on)
r.squaredGLMM(fit_iav_pep_off_vs_on)
plot(allEffects(fit_iav_pep_off_vs_on))
parres14 <- partialize(fit_iav_pep_off_vs_on,"on")
out_iav_pep_off_vs_on <- allEffects(fit_iav_pep_off_vs_on)
gg_iav_pep_off_vs_on <- ggplot_iav_off_on(out_iav_pep_off_vs_on)
gg_iav_pep_off_vs_on

# Long-term trends
# EOS ~ SOS + Year
fit_lt_pep_off_vs_on_year = lmer(off ~ scale(on) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_on_year)
r.squaredGLMM(fit_lt_pep_off_vs_on_year)
plot(allEffects(fit_lt_pep_off_vs_on_year))
parres15 <- partialize(fit_lt_pep_off_vs_on_year,"on")
parres16 <- partialize(fit_lt_pep_off_vs_on_year,"year")
out_lt_pep_off_vs_on_year <- allEffects(fit_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on   <- ggplot_lt_off_on(out_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on_year <- ggplot_lt_off_on_year(out_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on + gg_lt_pep_off_vs_on_year

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_on, fit_lt_pep_off_vs_on_year, test="F")  #test="Chisq"
out_anova

## Supplementary Fig. S3
ff_lt_pep_off_vs_on_year <- gg_lt_pep_off_vs_on_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + SOS")), subtitle = "PEP data") +
  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_on <- gg_lt_pep_off_vs_on +
  labs(title = expression(paste("EOS ~ Year + ", bold("SOS"))), subtitle = "") +
  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_iav_pep_off_vs_on <- gg_iav_pep_off_vs_on +
  labs(title = "EOS ~ SOS", subtitle = "") +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .25),
        legend.direction="vertical",
        legend.margin = margin(.2, .2, .2, .2),
        legend.key.size = unit(.6, 'lines'),plot.subtitle=element_text(size=10))  

ss3 <- ff_lt_pep_off_vs_on_year + ff_lt_pep_off_vs_on + ff_iav_pep_off_vs_on + plot_annotation(tag_levels = 'A')
ss3 
ggsave("~/phenoEOS/manuscript/figures/fig_S3_rev.png", width = 9, height = 3.5, dpi=300)
