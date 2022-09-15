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
library(ggpubr)

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
  left_join(pep_pmodel) %>%
  mutate(id_site=as.factor(id_site),species=as.factor(species))

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
out <- summary(fit_iav_pep_off_vs_gppnet)
out$coefficients
r.squaredGLMM(fit_iav_pep_off_vs_gppnet)
plot(allEffects(fit_iav_pep_off_vs_gppnet))
parres11 <- partialize(fit_iav_pep_off_vs_gppnet,"gpp_net")
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet <- ggplot_iav_off_gppnet(out_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet
# Unscaled
trend_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net)
error_unscaled <- out$coefficients["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)
upperCI_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net) + out$coefficient["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)*1.96
lowerCI_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net) - out$coefficient["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)*1.96

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
out <- summary(fit_lt_pep_off_vs_gppnet_year)
out$coefficients
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
# Unscaled
trend_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year)
error_unscaled <- out$coefficients["scale(year)","Std. Error"]/ sd(df_pep$year)
upperCI_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year) + out$coefficient["scale(year)","Std. Error"]/ sd(df_pep$year)*1.96
lowerCI_unscaled <- out$coefficients["scale(year)","Estimate"]/ sd(df_pep$year) - out$coefficient["scale(year)","Std. Error"]/ sd(df_pep$year)*1.96

trend_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net)
error_unscaled <- out$coefficients["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)
upperCI_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net) + out$coefficient["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)*1.96
lowerCI_unscaled <- out$coefficients["scale(gpp_net)","Estimate"]/ sd(df_pep$gpp_net) - out$coefficient["scale(gpp_net)","Std. Error"]/ sd(df_pep$gpp_net)*1.96

# Model comparison interannual vs. long-term
out_anova <- anova(fit_iav_pep_off_vs_gppnet, fit_lt_pep_off_vs_gppnet_year)
out_anova

# Supplementary Fig. S2
ff_lt_pep_off_vs_gppnet_year <- gg_lt_pep_off_vs_gppnet_year +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])),subtitle  = "PEP data and P-model") + 
  theme(legend.position = "none",plot.subtitle=element_text(size=10))

ff_lt_pep_off_vs_gppnet <- gg_lt_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)]))) + 
  theme(legend.position = "none",plot.subtitle=element_text(size=10))

ff_iav_pep_off_vs_gppnet <- gg_iav_pep_off_vs_gppnet +
  labs(title = expression(paste("EOS ~ ", italic("A")[net]))) +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
        legend.key = element_rect(fill = NA, color = NA),
        legend.position = c(.15, .20),
        legend.direction="vertical",
        legend.margin = margin(.1, .1, .1, .1),
        legend.key.size = unit(.45, 'lines'),plot.subtitle=element_text(size=10)) 

ss2 <- ff_lt_pep_off_vs_gppnet_year + ff_lt_pep_off_vs_gppnet + ff_iav_pep_off_vs_gppnet + 
  plot_annotation(tag_levels = 'A')
ss2 
ggsave("~/phenoEOS/manuscript/figures/fig_S2_rev.png", width = 9, height = 3.5, dpi=300)

# Sensitivity analysis ####

# daylength threshold of 10.0 hours

# read data pep LPJ-GUESS
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# read data pep P-model
pep_pmodel <- readRDS("~/phenoEOS/data/sensitivity//pep_pmodel_Anet_10h.rds") 

pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>%
  mutate(gpp_net=ifelse(gpp_net==0, NA, gpp_net))

df_pep <- df_pep %>% 
  left_join(pep_pmodel) %>%
  mutate(id_site=as.factor(id_site),species=as.factor(species))

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
out <- summary(fit_iav_pep_off_vs_gppnet)
out$coefficients
parres11 <- partialize(fit_iav_pep_off_vs_gppnet,"gpp_net")
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet_10h <- ggplot_iav_off_gppnet(out_pep_off_vs_gppnet)

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
out <- summary(fit_lt_pep_off_vs_gppnet_year)
out$coefficients
parres12 <- partialize(fit_lt_pep_off_vs_gppnet_year,"gpp_net")
parres13 <- partialize(fit_lt_pep_off_vs_gppnet_year,"year")
out_pep_off_vs_gppnet_year <- allEffects(fit_lt_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_10h <- ggplot_lt_off_gppnet(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_year_10h <- ggplot_lt_off_gppnet_year(out_pep_off_vs_gppnet_year)

# doy threshold of 23 Sep
# doy_cutoff <- lubridate::yday("2001-09-23")

# read data pep LPJ-GUESS
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# read data pep P-model
pep_pmodel <- readRDS("~/phenoEOS/data/sensitivity//pep_pmodel_Anet_23S.rds") 

pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>%
  mutate(gpp_net=ifelse(gpp_net==0, NA, gpp_net))

df_pep <- df_pep %>% 
  left_join(pep_pmodel) %>%
  mutate(id_site=as.factor(id_site),species=as.factor(species))

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
out <- summary(fit_iav_pep_off_vs_gppnet)
out$coefficients
parres11 <- partialize(fit_iav_pep_off_vs_gppnet,"gpp_net")
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet_23S <- ggplot_iav_off_gppnet(out_pep_off_vs_gppnet)

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
out <- summary(fit_lt_pep_off_vs_gppnet_year)
out$coefficients
parres12 <- partialize(fit_lt_pep_off_vs_gppnet_year,"gpp_net")
parres13 <- partialize(fit_lt_pep_off_vs_gppnet_year,"year")
out_pep_off_vs_gppnet_year <- allEffects(fit_lt_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_23S <- ggplot_lt_off_gppnet(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_year_23S <- ggplot_lt_off_gppnet_year(out_pep_off_vs_gppnet_year)

# doy threshold of 21 Jun
# doy_cutoff <- lubridate::yday("2001-06-21")

# read data pep LPJ-GUESS
df_pep <- data.table::fread("~/phenoEOS/data/DataMeta_3_Drivers_20_11_10.csv") %>% 
  as_tibble() %>% 
  rename(lon = LON, lat = LAT, year = YEAR, off = DoY_off, on = DoY_out, 
         anom_off = autumn_anomaly, anom_on = spring_anomaly, 
         species = Species, id_site = PEP_ID, sitename = timeseries) %>%
  mutate(id_site=as.character(id_site))

# read data pep P-model
pep_pmodel <- readRDS("~/phenoEOS/data/sensitivity//pep_pmodel_Anet_21J.rds") 

pep_pmodel <- pep_pmodel %>% 
  mutate(gpp_net = Anet_pmodel - rd_pmodel) %>%
  mutate(gpp_net=ifelse(gpp_net==0, NA, gpp_net))

df_pep <- df_pep %>% 
  left_join(pep_pmodel) %>%
  mutate(id_site=as.factor(id_site),species=as.factor(species))

# Interannual variation (IAV)
# EOS ~ Anet P-model
fit_iav_pep_off_vs_gppnet = lmer(off ~ scale(gpp_net) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_iav_pep_off_vs_gppnet)
out <- summary(fit_iav_pep_off_vs_gppnet)
out$coefficients
parres11 <- partialize(fit_iav_pep_off_vs_gppnet,"gpp_net")
out_pep_off_vs_gppnet <- allEffects(fit_iav_pep_off_vs_gppnet)
gg_iav_pep_off_vs_gppnet_21J <- ggplot_iav_off_gppnet(out_pep_off_vs_gppnet)

# Long-term trends
# EOS ~ Anet P-model + Year 
fit_lt_pep_off_vs_gppnet_year = lmer(off ~ scale(gpp_net) + scale(year) + (1|id_site) + (1|species), data = df_pep, na.action = "na.exclude")
summary(fit_lt_pep_off_vs_gppnet_year)
out <- summary(fit_lt_pep_off_vs_gppnet_year)
out$coefficients
parres12 <- partialize(fit_lt_pep_off_vs_gppnet_year,"gpp_net")
parres13 <- partialize(fit_lt_pep_off_vs_gppnet_year,"year")
out_pep_off_vs_gppnet_year <- allEffects(fit_lt_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_21J <- ggplot_lt_off_gppnet(out_pep_off_vs_gppnet_year)
gg_lt_pep_off_vs_gppnet_year_21J <- ggplot_lt_off_gppnet_year(out_pep_off_vs_gppnet_year)

# Supplementary Fig. S4
ff_iav_pep_off_vs_gppnet_10h <- gg_iav_pep_off_vs_gppnet_10h +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), subtitle = 
         "") +
  theme(#plot.background = element_rect(colour = "darkgrey", fill=NA, size=2),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(.15, .20),
    legend.direction="vertical",
    legend.margin = margin(.1, .1, .1, .1),
    legend.key.size = unit(.45, 'lines'))

ff_lt_pep_off_vs_gppnet_year_10h <- gg_lt_pep_off_vs_gppnet_year_10h +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), subtitle = 
         "PEP data and P-model \nDaylength threshold of 10 h.") +  
  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_gppnet_10h <- gg_lt_pep_off_vs_gppnet_10h +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), subtitle = 
         "") +  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_iav_pep_off_vs_gppnet_23S <- gg_iav_pep_off_vs_gppnet_23S +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), subtitle = 
         "") +  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_gppnet_year_23S <- gg_lt_pep_off_vs_gppnet_year_23S +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), subtitle = 
         "PEP data and P-model \nDOY threshold in Sept 23") +  
  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_gppnet_23S <- gg_lt_pep_off_vs_gppnet_23S +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), subtitle = 
         "") +  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_iav_pep_off_vs_gppnet_21J <- gg_iav_pep_off_vs_gppnet_21J +
  labs(title = expression(paste("EOS ~ ", italic("A")[net])), subtitle = 
         "") +  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_gppnet_year_21J <- gg_lt_pep_off_vs_gppnet_year_21J +
  labs(title = expression(paste("EOS ~ ", bold("Year"), " + ", italic("A")[net])), subtitle = 
         "PEP data and P-model \nDOY threshold in June 21") +  
  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ff_lt_pep_off_vs_gppnet_21J <- gg_lt_pep_off_vs_gppnet_21J +
  labs(title = expression(paste("EOS ~ Year + ", bolditalic("A")[bold(net)])), subtitle = 
         "") +  theme(legend.position = "none",plot.subtitle=element_text(size=10)) 

ss3 <- ff_lt_pep_off_vs_gppnet_year_10h + ff_lt_pep_off_vs_gppnet_10h + ff_iav_pep_off_vs_gppnet_10h +
  ff_lt_pep_off_vs_gppnet_year_23S + ff_lt_pep_off_vs_gppnet_23S + ff_iav_pep_off_vs_gppnet_23S +
  ff_lt_pep_off_vs_gppnet_year_21J + ff_lt_pep_off_vs_gppnet_21J + ff_iav_pep_off_vs_gppnet_21J +
  plot_annotation(tag_levels = 'A') + 
  plot_layout(ncol = 3)
ss3 
ggsave("~/phenoEOS/manuscript/figures/fig_S3_rev.png", width = 8, height = 10, dpi=300)
