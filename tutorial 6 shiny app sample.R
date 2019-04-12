# =======================================================
# Simulation of Forecasting Models for Time Series
# =======================================================
#
# This is Shiny web application, a powerful R front-end.
# You can run the demo using the 'Run App' button above.
#
# Resources on building similar RShiny web applications:
# RShiny website  : http://shiny.rstudio.com/
# RShiny tutorial : https://shiny.rstudio.com/tutorial/
# RShiny examples : https://shiny.rstudio.com/gallery/

#install.packages("dplyr")

library(shiny)
library(tseries)
library(forecast)
    
# -------------------------------------------------------
# Define UI for the application (for input parameters)
#
# Choose the user inputs and place them in the Sidebar
# Display the output of target Forecasts in Main Panel

cryptoData <- read.csv("crypto-markets.csv", header = TRUE)
cryptoData2 <- subset(cryptoData, select = c("symbol", "date", "open", "close"))
#symbolInt <- as.integer(cryptoData2$symbol)
cryptoData2 <- as.data.frame.matrix(cryptoData2) 

Bitcoin <- cryptoData2[grep("BTC", cryptoData2$symbol),]

Ethereum <- cryptoData2[grep("ETH", cryptoData2$symbol),]

XRP <- cryptoData2[grep("XRP", cryptoData2$symbol),]

BTCcash <- cryptoData2[grep("BCH", cryptoData2$symbol),]

LiteCoin <- cryptoData2[grep("LTC", cryptoData2$symbol),]

# need to dropdown and choose the type of crypto currency (according to symbol)

ui <- fluidPage(
  
  # Application title
  titlePanel("Time Series Forecast"),
  
  # Sidebar for inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose the Dataset",
                  list("Crypto-Data" = "cryptoData",
                       "Bitcoin" = "Bitcoin",
                       "Ethereum" = "Ethereum",
                       "XRP" = "XRP", 
                       "Bitcoin Cash" = "BTCcash",
                       "LiteCoin"= "LiteCoin")),
      numericInput("horizon", "Forecast Horizon (Months)", 24),
      submitButton("Update View")
    ),
    
    # Show the forecast
    mainPanel(
      tabsetPanel(
        tabPanel("Mean Forecast", plotOutput("meanForecastPlot")), 
        tabPanel("Naive Forecast", plotOutput("naiveForecastPlot")),
        tabPanel("Drift Forecast", plotOutput("driftForecastPlot")),
        tabPanel("Seasonal Forecast", plotOutput("snaiveForecastPlot"))
      )
    )
  )
)


# -------------------------------------------------------
# Define server logic for the application (for output)
#
# Construct the target Forecast Plot on data to display
# Display the output of target Forecasts in Main Panel

server <- function(input, output) {

  # Get the dataset chosen by the user
  getDataset <- reactive({
    if (input$dataset == "Bitcoin") { return(Bitcoin) }
    else if (input$dataset == "Ethereum") { return(Ethereum) }
    else if (input$dataset == "XRP") { return(XRP) }
    else if (input$dataset == "BTCcash") {return(BTCcash)}
    else { return(LiteCoin) } 
  })
  
  # Record the input forecast horizon
  getHorizon <- reactive({ input$horizon })

  # Plot the forecast plot for Mean
  output$meanForecastPlot <- renderPlot({
    fitModel <- meanf(getDataset(), h = getHorizon())
    autoplot(fitModel, main="", xlab="", ylab="")
  })
  
  # Plot the forecast plot for Naive
  output$naiveForecastPlot <- renderPlot({
    fitModel <- naive(getDataset(), h = getHorizon())
    autoplot(fitModel, main="", xlab="", ylab="")
  })
  
  # Plot the forecast plot for Drift
  output$driftForecastPlot <- renderPlot({
    fitModel <- rwf(getDataset(), h = getHorizon(), drift = TRUE)
    autoplot(fitModel, main="", xlab="", ylab="")
  })
  
  # Plot the forecast plot for Seasonal
  output$snaiveForecastPlot <- renderPlot({
    fitModel <- snaive(getDataset(), h = getHorizon())
    autoplot(fitModel, main="", xlab="", ylab="")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
