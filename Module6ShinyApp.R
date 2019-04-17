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
library(dplyr)




# -------------------------------------------------------
# Define UI for the application (for input parameters)
#
# Choose the user inputs and place them in the Sidebar
# Display the output of target Forecasts in Main Panel


cryptoData <- read.csv("crypto-markets.csv", header = TRUE)
cryptoData2 <- subset(cryptoData, select = c("symbol", "date", "open", "close"))
cryptoData2 <- as.data.frame.matrix(cryptoData2)

#Bitcoin
Bitcoin <- subset(cryptoData2, symbol =="BTC" & date >= "2014-01-01" & date <= "2018-11-29")
write.csv(Bitcoin, "bitcoin.csv")
Bitcoin <- read.csv("bitcoin.csv", header = TRUE)
BitcoinTimeSeriesOpen<-ts(Bitcoin$open, start=c(2014,1), end=c(2018,334), frequency=365)
BitcoinTimeSeriesClose<-ts(Bitcoin$close, start=c(2014,1), end=c(2018,334), frequency=365)

#Ethereum
Ethereum <- subset(cryptoData2, symbol =="ETH" & date >= "2016-01-01" & date <= "2018-11-29")
write.csv(Ethereum,"ethereum.csv")
Ethereum <- read.csv("ethereum.csv",header=TRUE)
EthereumTimeSeriesOpen<- ts(Ethereum$open, start=c(2016,1), end=c(2018,334), frequency=365)
EthereumTimeSeriesClose<- ts(Ethereum$close, start=c(2016,1), end=c(2018,334), frequency=365)

#XRP
XRP <- subset(cryptoData2, symbol =="XRP" & date >= "2014-01-01" & date <= "2018-11-29")
write.csv(XRP,"XRP.csv")
XRP <- read.csv("XRP.csv",header=TRUE)
XRPTimeSeriesOpen<- ts(XRP$open, start=c(2014,1), end=c(2018,334), frequency=365)
XRPTimeSeriesClose<- ts(XRP$close, start=c(2014,1), end=c(2018,334), frequency=365)

#BitcoinCash
BCash <- subset(cryptoData2, symbol =="BCH" & date >= "2017-07-23" & date <= "2018-11-29")
write.csv(BCash,"BCash.csv")
BCash <- read.csv("BCash.csv",header=TRUE)
BCashTimeSeriesOpen<- ts(BCash$open, start=c(2017,204), end=c(2018,334), frequency=365)
BCashTimeSeriesClose<- ts(BCash$close, start=c(2017,204), end=c(2018,334), frequency=365)

#LiteCoin
LiteCoin <- subset(cryptoData2, symbol =="LTC" & date >= "2014-01-01" & date <= "2018-11-29")
write.csv(LiteCoin,"LiteCoin.csv")
LiteCoin <- read.csv("LiteCoin.csv",header=TRUE)
LiteCoinTimeSeriesOpen<- ts(LiteCoin$open, start=c(2014,1), end=c(2018,334), frequency=365)
LiteCoinTimeSeriesClose<- ts(LiteCoin$close, start=c(2014,1), end=c(2018,334), frequency=365)



# need to dropdown and choose the type of crypto currency (according to symbol)

ui <- fluidPage(
  
  # Application title
  titlePanel("Time Series Forecast"),
  
  # Sidebar for inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose the Dataset",
                  list(
                       "Bitcoin (Open Price)" = "Bitopen",
                       "Bitcoin (Close Price)" = "Bitclose",
                       "Ethereum (Open Price)" = "Eopen",
                       "Ethereum (Close Price)" = "Eclose",
                       "XRP (Open Price)" = "XRPopen", 
                       "XRP (Close Price)" = "XRPclose",
                       "Bitcoin Cash (Open Price)" = "BTCopen",
                       "Bitcoin Cash (Close Price)" = "BTCclose",
                       "LiteCoin (Open Price)"= "LCopen",
                       "Litecoin (Close Price)" = "LCclose"
                       )),
      
      submitButton("Update View")
    ),
    
    # Show the forecast
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel ("Accuracy", dataTableOutput("accuracy"))
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
    if (input$dataset == "Bitopen") { return(BitcoinTimeSeriesOpen) }
    else if (input$dataset == "Bitclose") { return(BitcoinTimeSeriesClose) }
    else if (input$dataset == "Eopen") { return(EthereumTimeSeriesOpen) }
    else if (input$dataset == "Eclose") { return(EthereumTimeSeriesClose) }
    else if (input$dataset == "XRPopen") { return(XRPTimeSeriesOpen) }
    else if (input$dataset == "XRPclose") { return(XRPTimeSeriesClose) }
    else if (input$dataset == "BTCopen") {return(BCashTimeSeriesOpen)}
    else if (input$dataset == "BTCclose") {return(BCashTimeSeriesClose)}
    else if(input$dataset == "LCopen") {return(LiteCoinTimeSeriesOpen)}
    else if(input$dataset == "LCclose") {return(LiteCoinTimeSeriesClose)}
  })
  
  getStart <- function(){
    if (input$dataset == "Bitopen") { return(c(2014,1)) }
    else if (input$dataset == "Bitclose") { return(c(2014,1)) }
    else if (input$dataset == "Eopen") { return(c(2016,1)) }
    else if (input$dataset == "Eclose") { return(c(2016,1)) }
    else if (input$dataset == "XRPopen") { return(c(2014,1)) }
    else if (input$dataset == "XRPclose") { return(c(2014,1)) }
    else if (input$dataset == "BTCopen") {return(c(2017,204))}
    else if (input$dataset == "BTCclose") {return(c(2017,204))}
    else if(input$dataset == "LCopen") {return(c(2014,1))}
    else if(input$dataset == "LCclose") {return(c(2014,1))}
    
  }
  
  # Record the input forecast horizon
  getHorizon <- reactive({ input$horizon })
  
  # Plot the prediction
  output$plot <- renderPlot({
    train<- window(getDataset(), start = getStart(), end= c(2018,334), frequency=365)
    etsFit <- ets(train, model = 'ZZZ', damped = FALSE)
    tsFCOpen<- forecast(etsFit, h = 62)
    autoplot(tsFCOpen, series = "Predicted", 
             xlab = "Time", ylab = "Price")
    
  })
  

output$accuracy<-renderDataTable({train<- window(getDataset(), start = c(2014,1), end= c(2018,243), frequency=365)
                                   test<- window(getDataset(), start = c(2018,244), end= c(2018,304), frequency=365)
                                   etsFit <- ets(train, model = 'ZZZ', damped = FALSE)
                                   tsFCOpen <- forecast(etsFit, h = 62)
                                   accuracy(forecast(etsFit, h = 62), test)})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
