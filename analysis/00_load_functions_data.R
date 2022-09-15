# This script loads the functions for the subsequent plots and includes the data.

# Plot effects of EOS ~ Anet LPJ-GUESS: fit_iav_pep_off_vs_cAtot
ggplot_iav_off_catot <- function(x){
  df <- tibble(upper = x$`scale(cA_tot)`$upper[,1],
               lower = x$`scale(cA_tot)`$lower[,1],
               off = x$`scale(cA_tot)`$fit[,1],
               cA_tot = x$`scale(cA_tot)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres1, aes(cA_tot, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log",limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = cA_tot, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(cA_tot, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    scale_x_continuous(limits = c(min(parres1$cA_tot),max(parres1$cA_tot)), breaks = seq(500,2300,500)) +
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Anet LPJ + Year: fit_lt_pep_off_vs_cAtot_year
ggplot_lt_off_catot <- function(x){
  df <- tibble(upper = x$`scale(cA_tot)`$upper[,1],
               lower = x$`scale(cA_tot)`$lower[,1],
               off = x$`scale(cA_tot)`$fit[,1],
               cA_tot = x$`scale(cA_tot)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres2, aes(cA_tot, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log",limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = cA_tot, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(cA_tot, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    scale_x_continuous(limits = c(min(parres2$cA_tot),max(parres2$cA_tot)), breaks = seq(500,2300,500)) +
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Anet LPJ + Year: fit_lt_pep_off_vs_cAtot_year
ggplot_lt_off_catot_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres3, aes(year, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    scale_x_continuous(limits = c(min(parres3$year),max(parres3$year)), breaks = seq(1950,2015,20)) +
    labs(x = "Year", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Anet P-model: fit_iav_modis_off_vs_gppnet
ggplot_gppnet_modis <- function(x){
  df <- tibble(upper = x$`scale(gpp_net)`$upper[,1],
               lower = x$`scale(gpp_net)`$lower[,1],
               off   = x$`scale(gpp_net)`$fit[,1],
               gpp_net = x$`scale(gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres4, aes(gpp_net, off),bins = 60) +     
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log",limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = gpp_net, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(gpp_net, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(0,365),breaks = seq(50,350,100)) + 
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Mean Anet + Anomalies Anet: fit_modis_anom_gppnet
ggplot_mean_gppnet <- function(x){
  df <- tibble(upper = x$`scale(mean_gpp_net)`$upper[,1],
               lower = x$`scale(mean_gpp_net)`$lower[,1],
               off = x$`scale(mean_gpp_net)`$fit[,1],
               mean_gpp_net = x$`scale(mean_gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres5, aes(mean_gpp_net, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log",limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = mean_gpp_net, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    #geom_line(data = df, aes(mean_gpp_net, off), col = "black",size=.8) +
    geom_smooth(data = df, aes(x=mean_gpp_net, y=off),col="black", method = "lm",fullrange = T,size = .8, se=F,show.legend = F) +
    theme_classic() +
    scale_y_continuous(limits = c(0,365),breaks = seq(50,350,100)) + 
    scale_x_continuous(breaks = seq(0,2500,500)) + 
    labs(x = expression(paste("Mean " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Mean Anet + Anomalies Anet: fit_modis_anom_gppnet
ggplot_anom_gppnet <- function(x){
  df <- tibble(upper = x$`scale(anom_gpp_net)`$upper[,1],
               lower = x$`scale(anom_gpp_net)`$lower[,1],
               off   = x$`scale(anom_gpp_net)`$fit[,1],
               anom_gpp_net = x$`scale(anom_gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres6, aes(anom_gpp_net, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log",limits=c(1,6350),breaks=c(5,50,500,4000)) + 
    geom_ribbon(data = df, aes(x = anom_gpp_net, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    #geom_line(data = df, aes(anom_gpp_net, off), col = "black",size=.8) +
    geom_smooth(data = df, aes(x=anom_gpp_net, y=off),col="black", method = "lm",fullrange = T,size = .8, se=F,show.legend = F) +
    theme_classic() +
    scale_y_continuous(limits = c(0,365),breaks = seq(50,350,100)) + 
    scale_x_continuous(breaks = seq(-1000,1000,500)) + 
    labs(x = expression(paste("Anomalies " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Year: fit_lt_pep_off_vs_year
ggplot_off_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres7, aes(year, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, off), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres7$year),max(parres7$year)), ylim = c(175,365)) +
    scale_x_continuous(breaks = seq(1950,2020,20)) + 
    scale_y_continuous(breaks = seq(200,350,50)) + 
    labs(x = "Year", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of SOS ~ Year: fit_lt_pep_on_vs_year
ggplot_on_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               on = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres8, aes(year, on),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, on), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres8$year),max(parres8$year)), ylim = c(50,200)) +
    scale_x_continuous(breaks = seq(1950,2020,20)) + 
    scale_y_continuous(breaks = seq(50,200,50)) +
    labs(x = "Year", y = "SOS (DOY)")
  return(gg)
}

# Plot effects of Anet LPJ-GUESS ~ Year: fit_lt_pep_cAtot_vs_year
ggplot_catot_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               cA_tot = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres9, aes(year, cA_tot),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, cA_tot), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres9$year),max(parres9$year)), ylim = c(300,2200)) +
    scale_x_continuous(breaks = seq(1950,2020,20)) + 
    scale_y_continuous(breaks = seq(0,2300,500)) +
    labs(x = "Year", y = "cA_tot")
  return(gg)
}

# Plot effects of Anet P-model ~ Year: fit_lt_pep_gppnet_vs_year
ggplot_gppnet_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               gpp_net = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres10, aes(year, gpp_net),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, gpp_net), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres10$year),max(parres10$year)), ylim = c(300,2200)) +
    scale_x_continuous(breaks = seq(1950,2020,20)) + 
    scale_y_continuous(breaks = seq(0,2300,500)) +
    labs(x = "Year", y = "gpp_net")
  return(gg)
}

# Plot effects of  EOS ~ Anet P-model: fit_iav_pep_off_vs_gppnet
ggplot_iav_off_gppnet <- function(x){
  df <- tibble(upper = x$`scale(gpp_net)`$upper[,1],
               lower = x$`scale(gpp_net)`$lower[,1],
               off   = x$`scale(gpp_net)`$fit[,1],
               gpp_net = x$`scale(gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres11, aes(gpp_net, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = gpp_net, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(gpp_net, off), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres11$gpp_net),max(parres11$gpp_net)), ylim = c(175,365)) +
    scale_x_continuous(breaks = seq(0,2300,500)) + 
    scale_y_continuous(breaks = seq(200,350,50)) + 
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of  EOS ~ Anet P-model + Year: fit_lt_pep_off_vs_gppnet_year
ggplot_lt_off_gppnet <- function(x){
  df <- tibble(upper = x$`scale(gpp_net)`$upper[,1],
               lower = x$`scale(gpp_net)`$lower[,1],
               off   = x$`scale(gpp_net)`$fit[,1],
               gpp_net = x$`scale(gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres12, aes(gpp_net, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = gpp_net, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(gpp_net, off), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres12$gpp_net),max(parres12$gpp_net)), ylim = c(175,365)) +
    scale_x_continuous(breaks = seq(0,2300,500)) + 
    scale_y_continuous(breaks = seq(200,350,50)) + 
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Anet P-model + Year: fit_lt_pep_off_vs_gppnet_year
ggplot_lt_off_gppnet_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off   = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres13, aes(year, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, off), col = "black",size=.8) +
    theme_classic() +
    coord_cartesian(xlim = c(min(parres13$year),max(parres13$year)), ylim = c(175,365)) +
    scale_x_continuous(breaks = seq(1950,2020,20)) + 
    scale_y_continuous(breaks = seq(200,350,50)) + 
    labs(x ="Year", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ SOS: fit_iav_pep_off_vs_on
ggplot_iav_off_on <- function(x){
  df <- tibble(upper = x$`scale(on)`$upper[,1],
               lower = x$`scale(on)`$lower[,1],
               off = x$`scale(on)`$fit[,1],
               on = x$`scale(on)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres14, aes(on, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = on, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(on, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    labs(x = "SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ SOS + Year: fit_lt_pep_off_vs_on_year
ggplot_lt_off_on <- function(x){
  df <- tibble(upper = x$`scale(on)`$upper[,1],
               lower = x$`scale(on)`$lower[,1],
               off = x$`scale(on)`$fit[,1],
               on = x$`scale(on)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres15, aes(on, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = on, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(on, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    labs(x = "SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ SOS + Year: fit_lt_pep_off_vs_on_year
ggplot_lt_off_on_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres16, aes(year, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,5900),breaks=c(5,50,500,4000)) +
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(year, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(175,365),breaks = seq(200,350,50)) + 
    labs(x = "Year", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ SOS: fit_iav_modis_off_vs_on
ggplot_on_modis <- function(x){
  df <- tibble(upper = x$`scale(on)`$upper[,1],
               lower = x$`scale(on)`$lower[,1],
               off = x$`scale(on)`$fit[,1],
               on = x$`scale(on)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres17, aes(on, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,9500),breaks=c(5,50,500,4000)) + 
    geom_ribbon(data = df, aes(x = on, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(on, off), col = "black",size=.8) +
    theme_classic() +
    labs(x = "SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

# Plot effects of EOS ~ Mean SOS + Anomalies SOS: fit_lt_modis_anom_on
ggplot_mean_on <- function(x){
  df <- tibble(upper = x$`scale(mean_on)`$upper[,1],
               lower = x$`scale(mean_on)`$lower[,1],
               off = x$`scale(mean_on)`$fit[,1],
               mean_on = x$`scale(mean_on)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres18, aes(mean_on, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,9500),breaks=c(5,50,500,4000)) +   
    geom_ribbon(data = df, aes(x = mean_on, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(mean_on, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(0,365),breaks = seq(50,350,100)) + 
    labs(x = "Mean SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

ggplot_anom_on <- function(x){
  df <- tibble(upper = x$`scale(anom_on)`$upper[,1],
               lower = x$`scale(anom_on)`$lower[,1],
               off   = x$`scale(anom_on)`$fit[,1],
               anom_on = x$`scale(anom_on)`$x[,1])
  gg <- ggplot() + 
    geom_hex(data = parres19, aes(anom_on, off),bins = 60) + 
    scale_fill_gradientn("",colours = alpha(colorRampPalette( c("gray65", "navy", "red", "yellow"))(5),.7),
                         trans = "log", limits=c(1,9500),breaks=c(5,50,500,4000)) +  
    geom_ribbon(data = df, aes(x = anom_on, ymin = lower, ymax = upper), alpha = 0.2, fill = "black") +
    geom_line(data = df, aes(anom_on, off), col = "black",size=.8) +
    theme_classic() +
    scale_y_continuous(limits = c(0,365),breaks = seq(50,350,100)) + 
    labs(x = "Anomalies SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}
