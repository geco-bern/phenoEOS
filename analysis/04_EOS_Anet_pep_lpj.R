# This script analyses the relationship of CO2 assimilation (simulated using the LPJ-GUESS model)
# and phenological dates from ground observations (PEP725 data). Outputs include ED Fig. 2.
.

# load packages
library(dplyr)
library(lme4) 
library(MuMIn) 
library(lmerTest) 
library(effects) 
library(ggplot2)
library(patchwork)
library(jtools)
library(sjPlot)

# load functions for plots
source("~/phenoEOS/analysis/00_load_functions_data.R")

# read data
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))
length(unique(df_pep$id_site)) #3855

# Interannual variation (IAV) ####
# EOS ~ Anet LPJ-GUESS
fit_iav_pep_off_vs_cAtot = lmer(off ~ scale(cA_tot) + (1|id_site) + (1|species) , data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_cAtot)
out <- summary(fit_iav_pep_off_vs_cAtot)
estimate <- out$coefficients[,"Estimate"]
out$coefficients
r.squaredGLMM(fit_iav_pep_off_vs_cAtot)
plot(allEffects(fit_iav_pep_off_vs_cAtot))
aic <- AIC(fit_iav_pep_off_vs_cAtot)
tab_model(fit_iav_pep_off_vs_cAtot)
#visreg(fit_iav_pep_off_vs_cAtot, "cA_tot") 
parres1 <- partialize(fit_iav_pep_off_vs_cAtot,"cA_tot") # calculate partial residuals
out_iav_pep_off_vs_cAtot <- allEffects(fit_iav_pep_off_vs_cAtot,partial.residuals=T)
gg_iav_pep_off_vs_cAtot <- ggplot_iav_off_catot(out_iav_pep_off_vs_cAtot)
gg_iav_pep_off_vs_cAtot 
# Unscaled
trend_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot)
error_unscaled <- out$coefficients["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)
upperCI_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot) + out$coefficient["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)*1.96
lowerCI_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot) - out$coefficient["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)*1.96
trend_unscaled
error_unscaled

# Long-term trends ####
# EOS ~ Anet LPJ + Year 
fit_lt_pep_off_vs_cAtot_year = lmer(off ~ scale(cA_tot) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_cAtot_year)
out <- summary(fit_lt_pep_off_vs_cAtot_year)
out$coefficients
r.squaredGLMM(fit_lt_pep_off_vs_cAtot_year)
plot(allEffects(fit_lt_pep_off_vs_cAtot_year))
tab_model(fit_lt_pep_off_vs_cAtot_year)
aic <- AIC(fit_lt_pep_off_vs_cAtot_year)
#visreg(fit_lt_pep_off_vs_cAtot_year, "cA_tot")
#visreg(fit_lt_pep_off_vs_cAtot_year, "year")
parres2 <- partialize(fit_lt_pep_off_vs_cAtot_year,"cA_tot")
parres3 <- partialize(fit_lt_pep_off_vs_cAtot_year,"year")
out_lt_pep_off_vs_cAtot_year <- allEffects(fit_lt_pep_off_vs_cAtot_year,partial.residuals = TRUE)
gg_lt_pep_off_vs_cAtot <- ggplot_lt_off_catot(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_year <- ggplot_lt_off_catot_year(out_lt_pep_off_vs_cAtot_year)
gg_lt_pep_off_vs_cAtot + gg_lt_pep_off_vs_year + plot_layout(guides = "collect") & theme(legend.position = 'right')
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)
upperCI_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year) + out$coefficient["scale(year)","Std. Error"]/ sd(df_pep$year)*1.96
lowerCI_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year) - out$coefficient["scale(year)","Std. Error"]/ sd(df_pep$year)*1.96
trend_unscaled
error_unscaled

trend_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot)
error_unscaled <- out$coefficients["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)
upperCI_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot) + out$coefficient["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)*1.96
lowerCI_unscaled <- out$coefficients["scale(cA_tot)","Estimate"]/ sd(df_pep$cA_tot) - out$coefficient["scale(cA_tot)","Std. Error"]/ sd(df_pep$cA_tot)*1.96
trend_unscaled
error_unscaled

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_cAtot, fit_lt_pep_off_vs_cAtot_year)
out_anova

# ED Fig. 2 ####
ff_lt_pep_off_vs_year <- gg_lt_pep_off_vs_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), 
       subtitle = "PEP data and LPJ") +
  theme(legend.position = "none",
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7))

ff_lt_pep_off_vs_cAtot <- gg_lt_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), 
       subtitle = "") +
  theme(legend.position = "none",
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7)) 

ff_iav_pep_off_vs_cAtot <- gg_iav_pep_off_vs_cAtot +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), 
       subtitle = "") +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.85, .95),
        legend.direction="vertical",
        legend.margin = margin(.1, .1, .1, .1),
        legend.key.size = unit(.45, 'lines'),
        plot.title=element_text(size=7),plot.subtitle=element_text(size=6),
        axis.text=element_text(size=6),
        axis.title=element_text(size=7),
        legend.text = element_text(size=6))

figED2 <- ff_lt_pep_off_vs_year + ff_lt_pep_off_vs_cAtot + ff_iav_pep_off_vs_cAtot + 
  plot_annotation(tag_levels = 'A',tag_suffix = ')') & theme(plot.tag = element_text(size = 7))
figED2
ggsave("~/phenoEOS/manuscript/figures/ED_Fig2.jpg", width = 180, height = 70, units="mm",dpi=300)
ggsave("~/phenoEOS/manuscript/figures/ED_Fig2.eps", device=cairo_ps, width = 180, height = 70, units="mm", dpi=300)
