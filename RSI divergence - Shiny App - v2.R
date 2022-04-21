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
#                             Version 2 - Created by Gourab Nath
#                                     (19.04.2022 07.05 PM)
#
#               COMMIT - Added the download data option & Changed to Grid Layout
#
#---------------------------------------------------------------------------------------------------------
#
# The NSE-500 data can be found here (with Yahoo FInance Ticker):
# https://docs.google.com/spreadsheets/d/1NbDrJfQ6aHCp2w3SvzNbAXYCD3ng0vW9/edit?usp=sharing&ouid=107533347299874033323&rtpof=true&sd=true 
#
#---------------------------------------------------------------------------------------------------------

#Read NSE-500 data (from local drive)
nifty500 = read.csv("C:\\Users\\goura\\Documents\\000. Research\\RSI\\N500.csv")

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
    #print(stock_name)
    return("divergence")
  }
  else
  {
    return(NA)
  }
  
}


#The RSI Divergence Identifier for a list of stocks - Finally returns the filtered data
#-----------------------------------------------------------------------------------------
filter_data = function(window, date1, date2)
{
  divergence_indicator = c()
  index=0
  for(stock in stocks)
  {
    indicator = rsi_divergence(stock, start_date = date1, end_date = date2,  w=window)
    divergence_indicator = c(divergence_indicator, indicator)
    index=index+1
    print(index)
  }
  
  nifty500$divergence = divergence_indicator
  outData = subset(nifty500, !is.na(divergence))
  return(outData)
}




#THE SHINY APP
#-----------------------------------------------------------------------------------------
d = Sys.Date()

library(shiny)

ui <- fluidPage(
  titlePanel("STOCK WATCHERS - RSI Divergence"),
  
  hr(),
  
  fluidRow(
    column(3,
           numericInput("n", "window", 30),
    br(),
    actionButton("go", "Update"),
    downloadButton("downloadData", "Download")
    ),
    
    column(3, offset = 0,
           textInput("date1", "Start Date", "2021-01-01"),
           textInput("date2", "End Date", d)
    ),
  
  tableOutput("data")
))


server <- function(input, output) {
  
  data <- eventReactive(input$go, {
    filter_data(input$n, input$date1, input$date2)
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
}

shinyApp(ui, server)


