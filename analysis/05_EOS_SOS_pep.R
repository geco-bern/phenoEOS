# This script analyses the relationship of spring and autumn phenological dates 
# from ground observations (PEP725 data). Outputs include ED Fig. 7.

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

# Interannual variation (IAV) ####
# EOS ~ SOS
fit_iav_pep_off_vs_on = lmer(off ~ scale(on) + (1|id_site) + (1|species) , data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_on)
out <- summary(fit_iav_pep_off_vs_on)
out$coefficients
r.squaredGLMM(fit_iav_pep_off_vs_on)
plot(allEffects(fit_iav_pep_off_vs_on))
parres14 <- partialize(fit_iav_pep_off_vs_on,"on")
out_iav_pep_off_vs_on <- allEffects(fit_iav_pep_off_vs_on)
gg_iav_pep_off_vs_on <- ggplot_iav_off_on(out_iav_pep_off_vs_on)
gg_iav_pep_off_vs_on

# Long-term trends ####
# EOS ~ SOS + Year
fit_lt_pep_off_vs_on_year = lmer(off ~ scale(on) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_on_year)
out <- summary(fit_lt_pep_off_vs_on_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_off_vs_on_year)
plot(allEffects(fit_lt_pep_off_vs_on_year))
parres15 <- partialize(fit_lt_pep_off_vs_on_year,"on")
parres16 <- partialize(fit_lt_pep_off_vs_on_year,"year")
out_lt_pep_off_vs_on_year <- allEffects(fit_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on   <- ggplot_lt_off_on(out_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on_year <- ggplot_lt_off_on_year(out_lt_pep_off_vs_on_year)
gg_lt_pep_off_vs_on + gg_lt_pep_off_vs_on_year
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_on, fit_lt_pep_off_vs_on_year, test="F")  #test="Chisq"
out_anova

# ED Fig. 7 ####
ff_lt_pep_off_vs_on_year <- gg_lt_pep_off_vs_on_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + SOS")), subtitle = "PEP data") +
  theme(legend.position = "none",
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))

ff_lt_pep_off_vs_on <- gg_lt_pep_off_vs_on +
  labs(title = expression(paste("EOS ~ Year + ", bold("SOS"))), subtitle = "") +
  theme(legend.position = "none",
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))

ff_iav_pep_off_vs_on <- gg_iav_pep_off_vs_on +
  labs(title = "EOS ~ SOS", subtitle = "") +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .25),
        legend.direction="vertical",
        legend.margin = margin(.1, .1, .1, .1),
        legend.key.size = unit(.45, 'lines'),
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        legend.text = element_text(size=6)) 

figED7 <- ff_lt_pep_off_vs_on_year + ff_lt_pep_off_vs_on + ff_iav_pep_off_vs_on + 
  plot_annotation(tag_levels = 'A',tag_suffix = ')') & theme(plot.tag = element_text(size = 7))
figED7 
ggsave("~/phenoEOS/manuscript/figures/ED_Fig7.jpg", width = 180, height = 70, units="mm",dpi=300)
ggsave("~/phenoEOS/manuscript/figures/ED_Fig7.eps", device=cairo_ps, width = 180, height = 70, units="mm", dpi=300)
