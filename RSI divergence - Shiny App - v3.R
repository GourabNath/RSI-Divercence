#=========================================================================================================
#
#
#                              STOCK MARKET WATCHER - RSI DIVERGENCE
#                              ************************************* 
#
# This application displays RSI divergence observed for the given companies over a given time
# window.
#
#
#---------------------------------------------------------------------------------------------------------
#
#                                    Market Watcher Team     
#                                 -------------------------
#                             Avik Das, Gourab Nath, Rishav Nath
#
#---------------------------------------------------------------------------------------------------------
# 
#                             Version 3 - Created by Gourab Nath
#                                     (20.04.2022 23.27 PM)
#
#                          COMMIT - 1. Used tabsets layout
#                                   2. Added the progress bar
#                                   3. Added the graphhical output feature
#
#---------------------------------------------------------------------------------------------------------
#
# The NSE-500 data can be found here (with Yahoo FInance Ticker):
# https://docs.google.com/spreadsheets/d/1NbDrJfQ6aHCp2w3SvzNbAXYCD3ng0vW9/edit?usp=sharing&ouid=107533347299874033323&rtpof=true&sd=true 
#
#---------------------------------------------------------------------------------------------------------

#Read NSE-500 data (from local drive)
library(RCurl)
x <- "https://raw.githubusercontent.com/GourabNath/RSI-Divercence/main/N500.csv"
nifty500 <- read.csv(url(x))


#Choose the column with Yahoo Finance Ticker
stocks = nifty500[,4]



#The RSI divergence identifier for a given stock
#-----------------------------------------------------------------------
library(quantmod)

rsi_divergence <- function(stock, start_date, end_date=Sys.Date(), w)
{
  stock_name = stock
  startDate = as.Date(start_date)
  endDate = as.Date(end_date) 
  
  stock_data = getSymbols.yahoo(stock_name, from = startDate, to = endDate,
                                periodicity = "daily", auto.assign=FALSE)
  
  stock_data = na.omit(stock_data)
  stock_data = xts(stock_data)
  
  
  rsi = RSI(Cl(stock_data),n=14)
  close = as.numeric(stock_data[,4])
  
  n = length(rsi)
  
  rsi_w = rsi[(n-w+1):n]
  close_w = close[(n-w+1):n]
  range = 1:w
  
  
  reg_rsi = lm(rsi_w ~ range)
  reg_close = lm(close_w ~ range)
  
  b1 = reg_rsi$coefficients[2]
  b2 = reg_close$coefficients[2]
  
  
  if(b1 > 0 & b2 < 0)
  {
    return("divergence")
  }
  else
  {
    return(NA)
  }
  
}



#THE SHINY APP
#-----------------------------------------------------------------------------------------
d = Sys.Date()

library(shiny)
library(shinythemes)


ui <- fluidPage(theme = shinytheme("cerulean"),
  titlePanel(title=h1("STOCK WATCHERS - RSI DIVERGENCE", align="center", 
                      style = "font-weight: 500; color: black; background-color: #6BC1D1;
                      padding: 20px")),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      dateRangeInput("dates", "Date range", start="2021-01-01", 
                     end=as.character(Sys.Date())),
      br(),
      numericInput("n", "window", 30),
      br(),
      actionButton("go", "Update", width=180),
      
      downloadButton("downloadData", label="Download", class = "butt1"),
      tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;}")), 
      
      
      br(),
      br(),
      br(),
      selectInput("symb", "Stock Name", choices = c("^NSEI")),
      br(),
      br(),
      textOutput("nrows")
      
    ),
  
  mainPanel(
    tabsetPanel(type="tab",
                tabPanel("Data", tableOutput("data")),
                tabPanel("Graphs", plotOutput("plot_price"), plotOutput("plot_rsi"))
  )
  
)
))

stocks = nifty500[,4]
server <- function(input, output, session) {
  
  data <- eventReactive(input$go, {
    
    withProgress(message = 'Loading:', value = 0, {
      
      index=0
      divergence_indicator = c()
      for(stock in stocks)
      {
        index = index+1
        indicator = rsi_divergence(stock, input$dates[1], input$dates[2],  input$n)
        divergence_indicator = c(divergence_indicator, indicator)
        incProgress(1/500,detail = paste(index, stock))
        #Sys.sleep(0.2)
      }
      
      nifty500$divergence = divergence_indicator
      subset(nifty500, !is.na(divergence))
      
    })
  })
  
  output$data <- renderTable({
    data()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(d, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    })
  
  output$nrows <- renderText(
    paste("Number of Companies =", nrow(data()))
  )
  
  observe({updateSelectInput(session, "symb",
                             label = "Stocks with RSI Divergence",
                             choices = data()[,"yahoo.finance.symbol"]
  )
  })
  
  dataInput <- reactive({
    getSymbols.yahoo(input$symb, from = input$dates[1], to = input$dates[2],
                     periodicity = "daily", auto.assign=FALSE)
    
  })
  
  
  output$plot_price <- renderPlot(height=350, {
    stock_data = na.omit(dataInput())
    stock_data = xts(stock_data)
    close = stock_data[,4]
    plot.xts(close, col=2, main=input$symb)
    
  })
  
  output$plot_rsi <- renderPlot(height=200, {
    stock_data = na.omit(dataInput())
    stock_data = xts(stock_data)
    rsi = RSI(Cl(stock_data),n=14)
    plot.xts(rsi, col="black", main="RSI")
    
  })
  
  
}
  


shinyApp(ui, server)