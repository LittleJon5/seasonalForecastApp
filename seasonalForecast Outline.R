library("fImport")
library("forecast")
require("magrittr")
require("lubridate")
require("ggplot2")
require("gridExtra")
require("reshape2")
require("tseries")
require("lubridate")

# Get data from FRED

retail <- fredSeries("RSAFSNA", from = "1992-01-01")
plot(retail)

# Classical decomposition forecast

retail.stl <- stl(retail, s.window = "periodic") 

# Creating a data frame

stl.plot.data <- as.data.frame(retail.stl$time.series)

# Createing additional columns

stl.plot.data$time <- as.Date(time(retail))
stl.plot.data$value <- retail$RSAFSNA

four.way.frame(retail.stl, retail)

# Plot Data for the forecast imitation

plot.data <- melt(stl.plot.data, id.vars = c("time"))

# Plot of the data

ggplot(data = plot.data, aes(x = time, y = value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free") +
  labs(x = "Time", y = "Value \n", title = "Graphs and Figures \n")

# Forecast 

(retail.forecast <- forecast(retail.stl, h = 12))
plot(retail.forecast)
sindex <- sindexf(retail.stl, h = 12)

past <- past.data(retail.forecast)

future <- forecast.plot.frame(retail.forecast)

startDate <- past$time[1]
endDate <- future$time[nrow(future)]

input.date <- as.Date(time(retail.forecast$mean[1])) %>% as.character

load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
recessions <- subset(recessions, Start >= input.date ) # input date is character format

ggplot(data = past) +
  geom_rect(data = recessions, aes(xmin = Start,
                                     xmax = End, 
                                     ymin = -Inf,
                                     ymax = +Inf),
              fill = 'grey65',
              alpha = 0.4 ) +
  
  geom_ribbon(data = future,
              fill = 'lightblue',
              aes( x = time , ymin = lower95 , ymax = upper95 ) ) +
  
  geom_ribbon (data = future , fill = 'yellow' ,
               aes( x = time , ymin = lower80 , ymax = upper80 ) ) +
  
  geom_line ( data = future , aes ( x = time , y = forecast ) , size = 1.0 ,
              colour = 'red' ) +
  
  geom_point ( aes ( x = time , y = values ) , size = 1.5 , color = "red" ) +
  
  geom_line ( aes  ( x = time , y = values ) , color = "blue" ) +
  
  geom_smooth ( aes ( x = time, y = values ) , method = "loess" ,
                size = .65 , color = "black" , fill = "springgreen4" ) +
  
  scale_x_date( "" , limits = c( startDate , endDate ) )


# Bar Chart 
############

barLabels <- mon[sindex %>% as.timeSeries %>% month]
barplot(as.numeric(sindex), names.arg = barLabels)

# Exponential smoothing forecast
(retail.stl <- stl(retail, s.window = "periodic"))
(retail.forecast <- forecast(retail.stl, h = 12))
plot(retail.forecast)
# I think that the general idea of how to get the month labels will work here too

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

bar.data <- barChartData(retail.forecast)

ggplot(data = bar.data, aes(x = months, y = season)) +
  geom_bar(stat = "identity", color = "blue") +
  labs(x = " \n Time", y = " \n Season")




