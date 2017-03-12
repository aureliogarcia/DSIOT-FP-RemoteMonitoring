library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel(("Remote Monitoring: raw sensor data")),

    fluidRow(
    
    column(2,
           uiOutput("reacSelector")
    ),
    
    column(9, offset = 1,
           uiOutput("reacSlider")
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("plot1"))
  ),
  
  fluidRow(
      column(12,
             plotOutput("plot2"))
  )
  
))