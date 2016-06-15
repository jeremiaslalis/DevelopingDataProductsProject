#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(rCharts)

data("UKDriverDeaths")

#Pre-process the Data
accident <- function(d){
  ds <- as.data.frame(d)
  colnames(ds) <- "Actual_Deaths"
  ds$Actual_Deaths <- as.numeric(ds$Actual_Deaths)
  
  #Jan 1969 to Dec 1984
  date <- data.frame(m=character(0))
  
  pred <- data.frame(Forecasted_Deaths = numeric(0))
  
  #Building Data Frames for Years and Months
  for(yr in 1969:1984){
    for(mth in 1:12){
      if(mth < 10){
        mYr <- paste("0", as.character(mth), sep="")
        mYr <- paste(yr, mth, "01", sep = "-")
      }else
        mYr <- paste(yr, mth, "01", sep="-")
      date <- rbind(as.matrix(date), as.character(mYr)) 
    }
  }
  date <- as.data.frame(date)
  
  #Building Data Frames for Forecasted Values
  #initialize
  pred <- rbind(as.matrix(pred), c(0))
  
  #start Naive Forecasting
  for(i in 1:nrow(ds)-1){
    pred <- rbind(as.matrix(pred), ds[i,c("Actual_Deaths")])
  }
  
  pred <- as.data.frame(pred)
  
  #Combine Columns
  ds <- cbind(ds, pred)  
  newDS <- cbind(date, ds)
  newDS <- transform(newDS, m=as.character(m))
  
  newDS
}

d <- accident(UKDriverDeaths)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$disPlot <- renderChart2({
    # filter data
    startMonth <- input$StartMonth
    startYear <- input$StartYear
    endMonth <- input$EndMonth
    endYear <- input$EndYear
    choices <- input$variable
    bod <- paste(startYear, startMonth, "01", sep="-")
    eod <- paste(endYear, endMonth, "01", sep="-")
    
    filtered  <- d[d$m>=bod & d$m<=eod, ]
    
    if(length(choices)==0){
      mPlot(x = "m", y = list("Actual_Deaths", "Forecasted_Deaths"), data = filtered, type = 'Line',
            labels = list('Actual # of Deaths', 'Forecasted # of Deaths'), pointSize = 0)
    }else if(length(choices)>1){
      # draw the line graph using rChart
      mPlot(x = "m", y = list("Actual_Deaths", "Forecasted_Deaths"), data = filtered, type = 'Line',
          labels = list('Actual # of Deaths', 'Forecasted # of Deaths'), pointSize = 0)
    }else{
      if(choices[1]==1){
        mPlot(x = "m", y = list("Actual_Deaths"), data = filtered, type = 'Line',
              labels = list('Actual # of Deaths'), pointSize = 0)
      } else {
        mPlot(x = "m", y = list("Forecasted_Deaths"), data = filtered, type = 'Line',
              labels = list('Forecasted # of Deaths'), pointSize = 0)
      }
    }
    
  
  })
  
  output$startDate <- renderText({
    startMonth <- input$StartMonth
    startYear <- input$StartYear
    paste("Start Date (YYYY-MM-DD): ", startYear, "-", startMonth, "-01")
  })
  
  output$endDate <- renderText({
    endMonth <- input$EndMonth
    endYear <- input$EndYear
    paste("End Date (YYYY-MM-DD): ", endYear, "-", endMonth, "-01")
  })
  
  output$errorRate <- renderText({
    # filter data
    startMonth <- input$StartMonth
    startYear <- input$StartYear
    endMonth <- input$EndMonth
    endYear <- input$EndYear
    choices <- input$variable
    bod <- paste(startYear, startMonth, "01", sep="-")
    eod <- paste(endYear, endMonth, "01", sep="-")
    
    filtered  <- d[d$m>=bod & d$m<=eod, ]
    numerator <- 0
    denominator <- 0
    
    for (i in 1:nrow(filtered)){
      numerator <- numerator + abs(d[i,c("Forecasted_Deaths")]-d[i,c("Actual_Deaths")])
      denominator <- denominator + d[i,c("Actual_Deaths")] + d[i,c("Forecasted_Deaths")]
    }
    
    smape <- 0.5 * (numerator/denominator) * 100
    paste("Error Rate : ", smape,"%")
  })
})
  
