require(shiny)
require(forecast)
require(ggplot2)
require(fImport)
require(magrittr)
require(lubridate)
require(reshape2)
source("helper.R")

shinyServer(function(input, output) {
  
##################
#------------------------------- Indicator category
# This recieves to the Indicator and saves it as category.input
# for later use
                                        
    category.input <- reactive({
      input$class
    })
    
    
####################
    # The next part of this converts the user input for data.type
    # and turns into the associated fred ticker symbol
    ###############
 
    indicator.type <- eventReactive(input$getData, {
      
                        switch(input$data.type,
                               "Total Construction Spending" = "TTLCON",
                               "Total Construction Spending: Nonresidential" = "PNRESCON",
                               "Total Price Constrution Spending: Residential" = "PRRESCON",
                               "Retail and Food Services Sales" = "RSAFSNA",
                               "E-Commerce Retail Sales" = "ECOMNSA",
                               "Vehicle Miles Traveled" = "TRFVOLUSM227NFWA",
                               "Total Nonfarm" = "PAYNSA",
                               "Construction Workers" = "CEU2000000001",
                               "Total Vehicle Sales" = "TOTALNSA",
                               "Civilian Unemployment Rate" = "UNRATENSA",
                               "All Employees: Leisure and Hospitality: Food Services and Drinking Plances" = "CEU7072200001",
                               "All Employees: Construction Workers" = "CEU2000000001"
                               )
      })
  
###########################
    # This part of the server gets the data from FRED. It uses the 
    # indicator.type() to indicate which fred series I'll pull
    #######################
    
    fred.data <- reactive({
                            fredSeries(indicator.type(), from = input$date) %>%
                              applySeries(by = "monthly", FUN = mean)
                          })
  
##############################
    # This part of the is what we used to dermine the
    # the compound frequncy of the fredSeries
    # this will come into play in the next part of the server
    # we need this information for some of the manipulation functions
    ##########################
    
                  
    fred.compound <- reactive({
                                frequency(fred.data())
                              })
#######################
    # This function calls on functions located in the helper script that
    # change the time series. 
    ###################
    
    fred.final <- reactive({
                            switch(input$manipulate,
                                   "No Transformation" = fred.data(),
                                   "Change" = fred.data() %>% chg,
                                   "Change From a Year Ago" = fred.data() %>% ch1(n_obs_per_year = fred.compound()),
                                   "Percent Change" = fred.data() %>% pch,
                                   "Percent Change from a Year Ago" = fred.data() %>% pc1(n_obs_per_year = fred.compound()),
                                   "Compounded Annual Rate of Change" = fred.data() %>% pca(n_obs_per_year = fred.compound()),
                                   "Continuously Compounded Rate of Change" = fred.data() %>% cch,
                                   "Countinuously Compounded Annual Rate of Change" = fred.data() %>% cca(n_obs_per_year = fred.compound()),
                                   "Natural Log" = fred.data() %>% log
                                   )   
                            })
######################
    # The next function takes the final time series and scales it
    # next it makes an ets model
    # then if forecast the models out by how ever far the user desires
    ##################
    stl.model <- reactive({
                        (fred.final()/ as.numeric(input$scalefactor)) %>%
                        stl(s.window = "periodic", na.action = na.omit)
                        })
    
    stl.forecast <- reactive({
                          stl.model() %>% forecast(h = input$horizon)
                          })
########################
    # this part out puts the model paramerter the model table on the ui
    #####################
    
      
   
    output$timeseries <- renderPlot({
      
      plot.data <- four.way.frame(stl.model(), stl.forecast()) %>% as.data.frame
      
      startDate <- plot.data$time[1] %>% as.Date %>% as.character
      endDate <- plot.data$time[nrow(plot.data)] %>% as.Date
      
      load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
      recessions <- subset(recessions, Start >= startDate)
      
      ggplot(data = plot.data) +
        geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                              ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
        geom_point(aes(y = value, x = time), color = "red") +
        geom_line(aes(y = value, x = time), color = "blue") +
        labs(x = "Date", y = "Value \n") +
        scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
      
    })
    
    output$trend <- renderPlot({
      plot.data <- four.way.frame(stl.model(), stl.forecast()) %>% as.data.frame
      
      startDate <- plot.data$time[1] %>% as.Date %>% as.character
      endDate <- plot.data$time[nrow(plot.data)] %>% as.Date
      
      load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
      recessions <- subset(recessions, Start >= startDate)
      
      
      ggplot(data = plot.data) +
        geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                              ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
        geom_point(aes(y = trend, x = time), color = "red") +
        geom_line(aes(y = trend, x = time), color = "blue") +
        labs(x = "Date", y = "Value \n") +
        scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
    })
    
    output$season <- renderPlot({
      
      plot.data <- four.way.frame(stl.model(), stl.forecast()) %>% as.data.frame
      
      startDate <- plot.data$time[1] %>% as.Date %>% as.character
      endDate <- plot.data$time[nrow(plot.data)] %>% as.Date
      
      load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
      recessions <- subset(recessions, Start >= startDate)
      
      
      ggplot(data = plot.data) +
        geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                              ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
        geom_point(aes(y = seasonal, x = time), color = "red" ) +
        geom_line(aes(y = seasonal, x = time), color = "blue" ) +
        labs(x = "Date", y = "Value \n") +
        scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
    })
    
   output$resid <- renderPlot({
     
     plot.data <- four.way.frame(stl.model(), stl.forecast()) %>% as.data.frame
     
     startDate <- plot.data$time[1] %>% as.Date %>% as.character
     endDate <- plot.data$time[nrow(plot.data)] %>% as.Date
     
     load(url("http://marriottschool.net/teacher/govfinance/recessions.RData"))
     recessions <- subset(recessions, Start >= startDate)
     
     
     ggplot(data = plot.data) +
       geom_rect ( data = recessions , aes ( xmin = Start , xmax = End , 
                                             ymin = -Inf , ymax = +Inf ) , fill = 'grey65', alpha = 0.4 ) +
       geom_bar(aes(y = remainder, x = time), color = "blue", stat = "identity" ) +
       labs(x = "Date", y = "Value \n") +
       scale_x_date( "" , limits = c( as.Date(startDate) , as.Date(endDate) ) )
     
   })
   
    output$barChart <- renderPlot({
      
      bar.data <- barChartData(stl.forecast())
      
      ggplot(data = bar.data, aes(x = months, y = season)) +
        geom_bar(stat = "identity", color = "blue", fill= "yellow") +
        labs(x = " \n Time", y = " \n Season")
      
    })
    
    output$text <- renderPrint({
      
      stl.forecast()$model$method
                                    
      })
    
    
    
    output$text2 <- renderPrint({
      
      stl.forecast()$model$par
      
    })
    
    
    output$text3 <- renderPrint({
      
      nrow.value <- nrow(stl.forecast()$model$states)
      
      stl.forecast()$model$states[nrow.value, ]
      
    })
    
#############################
    # This displays the forecast information
    # it calls the forecast.frame function from the helper script
    #########################
    
    output$table <- renderTable({
      
      validate(
        need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button,
             when this message disappears your forecast is on its way.")
      )
      
#       forecast.df <- forecast.frame(stl.forecast())
#       forecast.df
      
      stl.forecast() %>% as.data.frame
      
    })
    
#######################
    # This calls three functions from helper.r to get the two data
    # required to use in the plot function
    # #################
    
   output$plot <- renderPlot({
    
    validate(
      need(input$getData, "Please Select the data you wish to Forecast and Click the 'Forecast' Button.
           When this message disappears your forecast is on its way.")
    )
     
    forecast.df <- forecast.plot.frame(stl.forecast())
 
    plot.data <- past.data(stl.forecast())
    
    ggforecast(plot.data, forecast.df, input$smooth, input$date)
    
 })
   
  
  
  }
)


