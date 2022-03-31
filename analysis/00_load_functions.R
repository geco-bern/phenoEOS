# This script loads the functions for the subsequent plots.

ggplot_cA_tot <- function(x){
  df <- tibble(upper = x$`scale(cA_tot)`$upper[,1],
               lower = x$`scale(cA_tot)`$lower[,1],
               off = x$`scale(cA_tot)`$fit[,1],
               cA_tot = x$`scale(cA_tot)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = cA_tot, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(cA_tot, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  
  return(gg)
}

ggplot_iav_cA_tot <- ggplot_cA_tot
ggplot_lt_cA_tot <- ggplot_cA_tot

ggplot_on <- function(x){
  df <- tibble(upper = x$`scale(on)`$upper[,1],
               lower = x$`scale(on)`$lower[,1],
               off = x$`scale(on)`$fit[,1],
               on = x$`scale(on)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = on, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(on, off), col = "royalblue") +
    theme_classic() +
    labs(x = "SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

ggplot_on_pep <- ggplot_on
ggplot_on_modis <- ggplot_on

ggplot_year <- function(x){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(year, off), col = "royalblue") +
    theme_classic() +
    labs(x = "Year", y = "EOS (DOY)")
  return(gg)
}

ggplot_offyear <- ggplot_year
ggplot_onyear <- ggplot_year
ggplot_cA_totyear <- ggplot_year
ggplot_gppnetyear <- ggplot_year

ggplot_gpp_net <- function(x){
  df <- tibble(upper = x$`scale(gpp_net)`$upper[,1],
               lower = x$`scale(gpp_net)`$lower[,1],
               off   = x$`scale(gpp_net)`$fit[,1],
               gpp_net = x$`scale(gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = gpp_net, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(gpp_net, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste(italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  
  return(gg)
}

ggplot_gpp_net_modis <- ggplot_gpp_net
ggplot_gpp_net_pep <- ggplot_gpp_net

ggplot_year_anet <- function(x, y){
  df <- tibble(upper = x$`scale(year)`$upper[,1],
               lower = x$`scale(year)`$lower[,1],
               off = x$`scale(year)`$fit[,1],
               year = x$`scale(year)`$x[,1]) %>% 
    bind_rows(
      tibble(upper = y$`scale(year)`$upper[,1],
             lower = y$`scale(year)`$lower[,1],
             anet   = y$`scale(year)`$fit[,1],
             year = y$`scale(year)`$x[,1])
    )
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = year, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(year, off), col = "royalblue") +
    theme_classic() +
    labs(x = "Year", y = "EOS (DOY)")
  return(gg)
}

ggplot_mean_cA_tot <- function(x){
  df <- tibble(upper = x$`scale(mean_cA_tot)`$upper[,1],
               lower = x$`scale(mean_cA_tot)`$lower[,1],
               off = x$`scale(mean_cA_tot)`$fit[,1],
               mean_cA_tot = x$`scale(mean_cA_tot)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = mean_cA_tot, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(mean_cA_tot, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste("Mean " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

ggplot_anom_cA_tot <- function(x){
  df <- tibble(upper = x$`scale(anom_cA_tot)`$upper[,1],
               lower = x$`scale(anom_cA_tot)`$lower[,1],
               off   = x$`scale(anom_cA_tot)`$fit[,1],
               anom_cA_tot = x$`scale(anom_cA_tot)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = anom_cA_tot, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(anom_cA_tot, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste("Anomalies " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  
  return(gg)
}

ggplot_mean_gpp_net <- function(x){
  df <- tibble(upper = x$`scale(mean_gpp_net)`$upper[,1],
               lower = x$`scale(mean_gpp_net)`$lower[,1],
               off = x$`scale(mean_gpp_net)`$fit[,1],
               mean_gpp_net = x$`scale(mean_gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = mean_gpp_net, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(mean_gpp_net, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste("Mean " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  return(gg)
}

ggplot_anom_gpp_net <- function(x){
  df <- tibble(upper = x$`scale(anom_gpp_net)`$upper[,1],
               lower = x$`scale(anom_gpp_net)`$lower[,1],
               off   = x$`scale(anom_gpp_net)`$fit[,1],
               anom_gpp_net = x$`scale(anom_gpp_net)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = anom_gpp_net, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(anom_gpp_net, off), col = "royalblue") +
    theme_classic() +
    labs(x = expression(paste("Anomalies " ,italic("A")[net], " (gC m"^-2, " yr"^-1, ")")), y = "EOS (DOY)")
  
  return(gg)
}

ggplot_mean_on <- function(x){
  df <- tibble(upper = x$`scale(mean_on)`$upper[,1],
               lower = x$`scale(mean_on)`$lower[,1],
               off = x$`scale(mean_on)`$fit[,1],
               mean_on = x$`scale(mean_on)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = mean_on, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(mean_on, off), col = "royalblue") +
    theme_classic() +
    labs(x = "Mean SOS (DOY)", y = "EOS (DOY)")
  return(gg)
}

ggplot_anom_on <- function(x){
  df <- tibble(upper = x$`scale(anom_on)`$upper[,1],
               lower = x$`scale(anom_on)`$lower[,1],
               off   = x$`scale(anom_on)`$fit[,1],
               anom_on = x$`scale(anom_on)`$x[,1])
  gg <- ggplot() + 
    geom_ribbon(data = df, aes(x = anom_on, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = df, aes(anom_on, off), col = "royalblue") +
    theme_classic() +
    labs(x = "Anomalies SOS (DOY)", y = "EOS (DOY)")
  
  return(gg)
}
