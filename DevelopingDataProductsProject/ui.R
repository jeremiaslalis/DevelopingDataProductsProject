#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(rCharts)

years <- c(1969)

for(i in 1970:1984){
  years <- c(years, i)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Forecasting Serious Injuries/Deaths of Drivers in UK (Jan 1969 - Dec 1984) using Naive Forecasting Method"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      helpText("Step 1: Choose what to show (Actual/Forecasted); Step 2: Choose the Start Date and End Date; Step 3: Click the Submit button to show the graph."),
      checkboxGroupInput("variable","Show:", choices = list("Actual" = 1,"Forecasted" = 2)),   
      selectInput("StartMonth", "Select Start Month:", c(1,2,3,4,5,6,7,8,9,10,11,12)),
      selectInput("StartYear", "Select Start Year:", years),
      selectInput("EndMonth", "Select Start Month:", c(1,2,3,4,5,6,7,8,9,10,11,12)),
      selectInput("EndYear", "Select Start Year:", years),
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       showOutput("disPlot", "morris"),
       verbatimTextOutput("startDate"),
       verbatimTextOutput("endDate"),
       h4('Symmetric Mean Absolute Percentage Error (SMAPE):'),
       verbatimTextOutput("errorRate")
       
    )
  )
))
