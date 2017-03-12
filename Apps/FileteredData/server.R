library(shiny)
library(ggplot2)
library(lubridate)
library(tree)
library(tidyr)
library(stringr)
library(plyr)
library(dplyr)

shinyServer(function(input, output, session) {
  
  Sys.setenv(TZ="CET")
  
  rssi_data <- readRDS("Data/filtered_rssi_data.RDS")
  
  output$reacSelector <- renderUI({
      users <- unique(rssi_data$user_id)
      selectInput("user_id", "User", users)
  })

  data_plot <- reactive({
      filter(rssi_data, user_id == input$user_id)
  })
  
  output$reacSlider <- renderUI({
    mindate = trunc.POSIXt(min(data_plot()$datetime), "hour")
    maxdate = max(trunc.POSIXt(max(data_plot()$datetime + 3600), "hour"))
    sliderInput("term", "Term", 
                min = mindate , max = maxdate, 
                value = c(mindate, maxdate), 
                step = 3600,
                width = "80%")
  })
  
  time_margins <- reactive({
    input$term
  })
  
  output$plot1 <- renderPlot({
    t1 <- time_margins()[1]
    t2 <- time_margins()[2]
    dataset <- data_plot()
    dataset1 <- dataset[dataset$datetime > t1 & dataset$datetime < t2,] %>%
        select(user_id, datetime, room_1:room_6) %>%
        gather(room, rssi, room_1:room_6)
    ggplot(data = dataset1) + 
        geom_line(aes(x = datetime, y = rssi, colour = room), size=.5) +
        ylim(-100, -20) +
        labs(x = "Datetime", y = "RSSI", colour = "Room")
        
    
    })
  
  
  output$plot2 <- renderPlot({
      t1 <- time_margins()[1]
      t2 <- time_margins()[2]
      dataset <- data_plot()
      dataset2 <- dataset[dataset$datetime > t1 & dataset$datetime < t2,] %>%
          select(user_id, datetime, fk_room_1:fk_room_6) %>%
          gather(room, filtered_rssi, fk_room_1:fk_room_6)
      ggplot(data = dataset2) + 
          geom_line(aes(x = datetime, y = filtered_rssi, colour = room), size=.5) +
          ylim(-100, -20) +
          labs(x = "Datetime", y = "Filtered RSSI", colour = "Room") +
          scale_color_discrete(labels = c('room_1', 'room_2','room_3','room_4','room_5','room_6'))
      
  })
  
  
})