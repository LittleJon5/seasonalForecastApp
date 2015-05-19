library(shiny)
library(forecast)
library(ggplot2)
library(fImport)

shinyUI(fluidPage(
  
  
# ------------------------- This loads the css file for apps style
  includeCSS("styles.css"),
  
  titlePanel(h1("US Macroeconomic Situation and Forecast")),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(h3("Graphs, Models, and Forecasts")),
      
      selectInput("data.type", label = h4("Economic Indicator from Fred"),
                  choices = c("Total Construction Spending",
                              "Total Construction Spending: Nonresidential",
                              "Total Price Constrution Spending: Residential",
                              "Retail and Food Services Sales",
                              "E-Commerce Retail Sales",
                              "Vehicle Miles Traveled",
                              "Total Nonfarm",
                              "Construction Workers",
                              "Total Vehicle Sales",
                              "Civilian Unemployment Rate",
                              "All Employees: Leisure and Hospitality: Food Services and Drinking Plances",
                              "All Employees: Construction Workers"
                  ),
                  selected = "Total Construction Spending"
      ),
      
     
      
      
      actionButton("getData", "Forecast"),
      
      helpText(h3("Options")),
      
      selectInput("manipulate", label = h4("Transformations"),
                  choices = c("No Transformation",
                              "Change",
                              "Change From a Year Ago",
                              "Percent Change",
                              "Percent Change from a Year Ago",
                              "Compounded Annual Rate of Change",
                              "Continuously Compounded Rate of Change",
                              "Countinuously Compounded Annual Rate of Change",
                              "Natural Log"),
                  selected = "No Transformation"),
      
      selectInput("scalefactor", 
                  label = h4("Scale Factor"), 
                  c("No Change" = 1,
                    "Thousands" = 1000,
                    "Millions" = 1000000,
                    "Billions" = 1000000000),
                  selected = "No change"),
      
      sliderInput("smooth",
                  label = h4("Smoothing Parameter"),
                  min=.05,
                  max=.5,
                  value= .21,
                  animate = TRUE),
      
      numericInput("horizon", 
                   label = h4("Forecast Horizon"), 
                   value = 12), 
      
      dateInput("date", 
                label = h4("Initial Date (YYYY-MM-DD)"), 
                value = "1959-01-01")
    ),
    
    
    
    mainPanel(
        tabsetPanel(
          tabPanel("Forecast Graph", h5(plotOutput("plot"))),
          tabPanel("Decomposition",
                   helpText(h5("Time Series")),
                   h5(plotOutput("timeseries")),
                   helpText(h5("Trend")),
                   h5(plotOutput("trend")),
                   helpText(h5("Seasonality")),
                   h5(plotOutput("season")),
                   helpText(h5("Residuals")),
                   h5(plotOutput("resid"))
                   
          ),
          tabPanel("Additive Seasonal Index",
                   h5(plotOutput("barChart"))),
          tabPanel("Model",
                   helpText(h5("Method")),
                   h2(verbatimTextOutput("text")),
                   helpText(h5("ETS Transition Equation:")),
                   helpText(h5("Smoothing Constants and Estimated Parameters")),
                   h2(verbatimTextOutput("text2"))),
          tabPanel("Forecasts",
                   helpText(h5("Forecasted Values")),
                   h5(tableOutput("table")))
      
  )))))
