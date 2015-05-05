# FRED transformation functions
# October 11, 2012 Revised March 19, 2015
# Author: Ray Nelson
###############################################################################
require(magrittr)

# ---------------------------------- Change
chg <- function(tsData) {
  tsData %>% 
    diff %>% 
    na.omit
}

# --------------------------------- Year over year change
ch1 <- function(tsData, n_obs_per_year) {
  tsData %>% 
    diff(lag = n_obs_per_year) %>% 
    na.omit
}

# --------------------------------- Percentage change
pch <- function(tsData) {
  (tsData/lag(tsData, 1)-1) * 100 %>% 
    na.omit
}

# ---------------------------------- Year over year percentage change
pc1 <- function(tsData, n_obs_per_year) {
  (tsData/lag(tsData, n_obs_per_year)-1) * 100 %>% 
    na.omit
}

# ---------------------------------- Compounded annual change
pca <- function(tsData, n_obs_per_year) {
  na.omit(((tsData/lag(tsData, 1))^n_obs_per_year-1) * 100)
}

# ----------------------------- Continously compounded percentage change
cch <- function(tsData) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * 100)
}

# ----------------------------- Continuously compounded annual change
cca <- function(tsData, n_obs_per_year) {
  na.omit((log(tsData) - log(lag(tsData, 1))) * n_obs_per_year * 100)
}
 
# ----------------------------- Final plot function
ggforecast <- function(past, future, span.val, input.date){
  
  load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
  recessions <- subset(recessions, Start >= input.date)
  
  startDate <- past$time[1]
  endDate <- future$time[nrow(future)]
  
  ggplot ( data = past ) +
    geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                     ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
    geom_ribbon ( data = future , fill = 'lightblue ' ,
                aes ( x = time , ymin = lower95 , ymax = upper95 ) ) +
    geom_ribbon ( data = future , fill = 'yellow' ,
                aes( x = time , ymin = lower80 , ymax = upper80 ) ) +
    geom_line ( data = future , aes ( x = time , y = forecast ) , size = 1.0 ,
              colour = 'red' ) +
    geom_point ( aes ( x = time , y = values ) , size = 1.5 , color = "red" ) +
    geom_line ( aes  ( x = time , y = values ) , color = "blue" ) +
    geom_smooth ( aes ( x = time, y = values ) , method = "loess" , span = span.val,
                size = .65 , color = "black" , fill = "springgreen4" ) +
    scale_x_date( "" , limits = c( startDate , endDate ) )
}

# future data frame assembly function 
# this version of the function works best for displaying
# the data in a table format
# the next function is exactly the same except that
# as.character portion of the first item in the data frame is left off
###########################

forecast.frame <- function(ets.data){
  framed <- data.frame(as.character(as.Date(time(ets.data$mean))),
                      ets.data$mean,
                      ets.data$lower[,2],
                      ets.data$lower[, 1],
                      ets.data$upper[, 1],
                      ets.data$upper[, 2])
  names(framed) <- c('time', 'forecast', 'lower95', 'lower80', 
                     'upper80', 'upper95')
  return(framed)
}

######################
# This version is idea for getting the data into a plotable form.
######################

forecast.plot.frame <- function(ets.data){
  framed <- data.frame(as.Date(time(ets.data$mean)),
                       ets.data$mean,
                       ets.data$lower[,2],
                       ets.data$lower[, 1],
                       ets.data$upper[, 1],
                       ets.data$upper[, 2])
  names(framed) <- c('time', 'forecast', 'lower95', 'lower80', 
                     'upper80', 'upper95')
  return(framed)
}

#####
# This function is like the one before but it assembles
# a plot for the observed data used in the forecast
# for this is best combined with the forecast.plot.frame
# to use the ggforecast function.
#####

past.data <- function(ets.data){
  
  plot.data <- data.frame(as.Date(time(ets.data$x)),
                  ets.data$x,
                  ets.data$fitted)
  
  names(plot.data) <- c("time", "values", "fitted")
  
  return(plot.data)
}

#################
  # Function for making a seasonal, trend, timeseries data frame
  # It gets fed a  stl.model function as well time series that everythings
  # based on
  ########################

four.way.frame <- function(stl.model, retail.forecast) {
  
  stl.plot.data <- stl.model$time.series %>% as.data.frame
  stl.plot.data$time <- stl.model$time.series %>% time %>% as.Date
  stl.plot.data$value <- retail.forecast$x %>% as.numeric
  return(stl.plot.data)
}

########################################
        ## Function for assembling the graph for the seasonal index of the months
        # Another one will need ot be created for quarterly data
        #########################

barChartData <- function(stl.forecast){
  
  mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sept", "Oct", "Nov", "Dec")  # vector used for assignement
  
  season <- stl.forecast$seasonal
  
  time <- as.Date(time(season))
  
  value <- data.frame(season)
  
  months <- factor(mon[month(time)], levels = mon)
  
  plot.data <- cbind(time, value, months)
  
  return(plot.data)
}



