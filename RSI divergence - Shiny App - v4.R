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
#                             Version 4 - Created by Gourab Nath
#                                     (21.04.2022 23.27 PM)
#
#                                      WHATS NEW? 
#                                     1. Use SMA
#                             2. Takes SMA order as input
#            3. Regression pf price based on SMA (to avoid extreme values)
#
#---------------------------------------------------------------------------------------------------------
#
# The NSE-500 data can be found here (with Yahoo FInance Ticker):
# https://docs.google.com/spreadsheets/d/1NbDrJfQ6aHCp2w3SvzNbAXYCD3ng0vW9/edit?usp=sharing&ouid=107533347299874033323&rtpof=true&sd=true 
#
#---------------------------------------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(RCurl)
library(quantmod)
library(TTR)

#Read NSE-500 data (from local drive)

x <- "https://raw.githubusercontent.com/GourabNath/RSI-Divercence/main/N500.csv"
nifty500 <- read.csv(url(x))


#Choose the column with Yahoo Finance Ticker
stocks = nifty500[,4]



#The RSI divergence identifier for a given stock
#-----------------------------------------------------------------------

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
  close_w = SMA(close,n=10)[(n-w+1):n]
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

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel(title=h1("STOCK WATCHERS - RSI DIVERGENCE", align="center", 
                                    style = "font-weight: 500; color: black; background-color: #6BC1D1;
                      padding: 20px")),
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               dateRangeInput("dates", "Date range", start="2021-01-01", 
                                              end=as.character(Sys.Date())),
                               numericInput("n", "window", 35),
                               numericInput("sma", "SMA Order", 5),
                               br(),
                               actionButton("go", "Update", width=190),
                               
                               downloadButton("downloadData", label="Download", class = "butt1"),
                               tags$head(tags$style(".butt1{background-color:orange;} .butt1{color: black;}")), 
                               br(),
                               br(),
                               br(),
                               selectInput("symb", "Stock Name", choices = c("^NSEI")),
                               uiOutput("tab"),
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
        
        #indicator = rsi_divergence(stock, input$dates[1], input$dates[2],  input$n)
        
        #----
        
        
        stock_data = getSymbols.yahoo(stock, from = input$dates[1], to = input$dates[2],
                                      periodicity = "daily", auto.assign=FALSE)
        
        stock_data = na.omit(stock_data)
        stock_data = xts(stock_data)
        
        
        rsi = RSI(Cl(stock_data),n=14)
        close = as.numeric(stock_data[,4])
        
        n = length(rsi)
        w = input$n
        
        rsi_w = rsi[(n-w+1):n]
        close_w = SMA(close,n=input$sma)[(n-w+1):n]
        range = 1:w
        
        
        
        reg_rsi = lm(rsi_w ~ range)
        reg_close = lm(close_w ~ range)
        
        b1 = reg_rsi$coefficients[2]
        b2 = reg_close$coefficients[2]
        
        
        if(b1 > 0 & b2 < 0)
        {
          divergence_indicator = c(divergence_indicator, "divergence")
        }
        else
        {
          divergence_indicator = c(divergence_indicator, NA)
        }
        
      
        
        
        #----
        
        #divergence_indicator = c(divergence_indicator, indicator)
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
    sma = SMA(close, n=input$sma)
    plot(close, col=2, main=input$symb)
    lines(sma, col=3, on=1, main=input$symb)
    
  })
  
  output$plot_rsi <- renderPlot(height=200, {
    stock_data = na.omit(dataInput())
    stock_data = xts(stock_data)
    rsi = RSI(Cl(stock_data),n=14)
    sma = SMA(rsi, n=input$sma)
    plot.xts(rsi, col="black", main="RSI")
    lines(sma, col=3, on=1, main="RSI")
    
  })
  
  url <- reactive({
    if(input$symb != "^NSEI")
    {
      name = substr(input$symb, 1, nchar(input$symb)-3)
      a(paste("Screener Link:", name), href=paste("https://www.screener.in/company/", name, "/consolidated/", sep=""))
    }
  })
  
  output$tab <- renderUI({
    if(input$symb != "^NSEI")
    {
      tagList(url())
    }
  })
  
}



shinyApp(ui, server)