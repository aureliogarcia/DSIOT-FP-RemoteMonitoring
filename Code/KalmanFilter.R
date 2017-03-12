# KalmanFilter.R
# 
# Filters noise in rssi data to make every user more comparable
# and to let us better infer activity levels and other behaviour variables

# Loading libraries

library(readr)
library(tidyr)
library(dplyr)
library(TTR)

# Reading rssi data

rssi_data <- read_csv("Data/rssi_data.csv", 
                      col_types = "iTnnnnnn", 
                      locale = locale(tz = "CET")) %>% as.data.frame()

#### Cuando se encuentra un -95 toma el valor anterior
#### Calcula una varianza corrida en cada  paso, para tener solo la información de la varianza del último minuto.

noise_filtering <- function(sensor_data) {
    
    # Initial values
    
    # First we check if there is any missing sensor. If so, we add it with NA values
    
    # room_1 = living room
    # room_2 = kitchen
    # room_3 = bedroom
    # room_4 = bathroom
    # room_5 = custom room 1
    # room_6 = custom room 2
    
    sensor_data$room_1 <-  if(length(sensor_data$room_1)==0) NA else sensor_data$room_1
    sensor_data$room_2 <-  if(length(sensor_data$room_2)==0) NA else sensor_data$room_2
    sensor_data$room_3 <-  if(length(sensor_data$room_3)==0) NA else sensor_data$room_3
    sensor_data$room_4 <-  if(length(sensor_data$room_4)==0) NA else sensor_data$room_4
    sensor_data$room_5 <-  if(length(sensor_data$room_5)==0) NA else sensor_data$room_5
    sensor_data$room_6 <-  if(length(sensor_data$room_6)==0) NA else sensor_data$room_6
    
    
    
    # If any sensor has not an initial value, we start adding a -95 until we find a real measured value
    
    if(is.na(sensor_data$room_1[1])) sensor_data$room_1[1] <- -95
    if(is.na(sensor_data$room_2[1])) sensor_data$room_2[1] <- -95
    if(is.na(sensor_data$room_3[1])) sensor_data$room_3[1] <- -95
    if(is.na(sensor_data$room_4[1])) sensor_data$room_4[1] <- -95
    if(is.na(sensor_data$room_5[1])) sensor_data$room_5[1] <- -95
    if(is.na(sensor_data$room_6[1])) sensor_data$room_6[1] <- -95

    # Q parameter for the Kalman filter (process noise)
    
    Q = 0.005
    
    # Other initial values for starting the filter
    
    sensor_data$fk_room_1[1] <- sensor_data$room_1[1]
    sensor_data$P_room_1[1] <- 1 

    sensor_data$fk_room_2[1] <- sensor_data$room_2[1]
    sensor_data$P_room_2[1] <- 1 

    sensor_data$fk_room_3[1] <- sensor_data$room_3[1]
    sensor_data$P_room_3[1] <- 1 

    sensor_data$fk_room_4[1] <- sensor_data$room_4[1]
    sensor_data$P_room_4[1] <- 1 

    sensor_data$fk_room_5[1] <- sensor_data$room_5[1]
    sensor_data$P_room_5[1] <- 1 

    sensor_data$fk_room_6[1] <- sensor_data$room_6[1]
    sensor_data$P_room_6[1] <- 1 

    
    # We add Pp (prior estimate) and K (filter value) variables with empty values
    
    sensor_data$Pp_room_1 <- NA
    sensor_data$K_room_1 <- NA
    
    sensor_data$Pp_room_2 <- NA
    sensor_data$K_room_2 <- NA
    
    sensor_data$Pp_room_3 <- NA
    sensor_data$K_room_3 <- NA
    
    sensor_data$Pp_room_4 <- NA
    sensor_data$K_room_4 <- NA
    
    sensor_data$Pp_room_5 <- NA
    sensor_data$K_room_5 <- NA
    
    sensor_data$Pp_room_6 <- NA
    sensor_data$K_room_6 <- NA
    
    # We create moving windows for every sensor room. 
    # We will calculate the R kalman parameter (measurement noise) as the signal variance in this moving window
    
    moving_window_room_1 <- sensor_data$room_1[1]
    moving_window_room_2 <- sensor_data$room_2[1]
    moving_window_room_3 <- sensor_data$room_3[1]
    moving_window_room_4 <- sensor_data$room_4[1]
    moving_window_room_5 <- sensor_data$room_5[1]
    moving_window_room_6 <- sensor_data$room_6[1]
    
    k <- 1 # Width of moving window, in minutes
    
    # Loop to filter noise and update filter paramenters
    
    for(i in c(2: nrow(sensor_data))){

        # room_1
        if(sensor_data$room_1[i] == -95 | is.na(sensor_data$room_1[i])) sensor_data$room_1[i] <- sensor_data$room_1[i-1]
        
        moving_window_room_1 <- append(moving_window_room_1, sensor_data$room_1[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_1, na.rm = T)
        
        sensor_data$Pp_room_1[i] <- sensor_data$P_room_1[i-1] + Q
        sensor_data$K_room_1[i] <- sensor_data$Pp_room_1[i] / (sensor_data$Pp_room_1[i] + Rs)
        sensor_data$fk_room_1[i] <- sum(c(sensor_data$fk_room_1[i-1],
                                       sensor_data$K_room_1[i] * sum(c(sensor_data$room_1[i], -sensor_data$fk_room_1[i-1]), na.rm=T)),
                                     na.rm=T)
        sensor_data$P_room_1[i] <- (1 - sensor_data$K_room_1[i]) * sensor_data$Pp_room_1[i]

        # room_2
        if(sensor_data$room_2[i] == -95 | is.na(sensor_data$room_2[i])) sensor_data$room_2[i] <- sensor_data$room_2[i-1]
        
        moving_window_room_2 <- append(moving_window_room_2, sensor_data$room_2[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_2, na.rm = T)
        
        sensor_data$Pp_room_2[i] <- sensor_data$P_room_2[i-1] + Q
        sensor_data$K_room_2[i] <- sensor_data$Pp_room_2[i] / (sensor_data$Pp_room_2[i] + Rs)
        sensor_data$fk_room_2[i] <- sum(c(sensor_data$fk_room_2[i-1],
                                            sensor_data$K_room_2[i] * sum(c(sensor_data$room_2[i], -sensor_data$fk_room_2[i-1]), na.rm=T)),
                                          na.rm=T)
        sensor_data$P_room_2[i] <- (1 - sensor_data$K_room_2[i]) * sensor_data$Pp_room_2[i]

        # room_3
        if(sensor_data$room_3[i] == -95 | is.na(sensor_data$room_3[i])) sensor_data$room_3[i] <- sensor_data$room_3[i-1]
        
        moving_window_room_3 <- append(moving_window_room_3, sensor_data$room_3[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_3, na.rm = T)
        
        sensor_data$Pp_room_3[i] <- sensor_data$P_room_3[i-1] + Q
        sensor_data$K_room_3[i] <- sensor_data$Pp_room_3[i] / (sensor_data$Pp_room_3[i] + Rs)
        sensor_data$fk_room_3[i] <- sum(c(sensor_data$fk_room_3[i-1],
                                            sensor_data$K_room_3[i] * sum(c(sensor_data$room_3[i], -sensor_data$fk_room_3[i-1]), na.rm=T)),
                                          na.rm=T)
        sensor_data$P_room_3[i] <- (1 - sensor_data$K_room_3[i]) * sensor_data$Pp_room_3[i]

        # room_4
        if(sensor_data$room_4[i] == -95 | is.na(sensor_data$room_4[i])) sensor_data$room_4[i] <- sensor_data$room_4[i-1]
        
        moving_window_room_4 <- append(moving_window_room_4, sensor_data$room_4[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_4, na.rm = T)
        
        sensor_data$Pp_room_4[i] <- sensor_data$P_room_4[i-1] + Q
        sensor_data$K_room_4[i] <- sensor_data$Pp_room_4[i] / (sensor_data$Pp_room_4[i] + Rs)
        sensor_data$fk_room_4[i] <- sum(c(sensor_data$fk_room_4[i-1],
                                            sensor_data$K_room_4[i] * sum(c(sensor_data$room_4[i], -sensor_data$fk_room_4[i-1]), na.rm=T)),
                                          na.rm=T)
        sensor_data$P_room_4[i] <- (1 - sensor_data$K_room_4[i]) * sensor_data$Pp_room_4[i]

        # room_5
        if(sensor_data$room_5[i] == -95 | is.na(sensor_data$room_5[i])) sensor_data$room_5[i] <- sensor_data$room_5[i-1]
        
        moving_window_room_5 <- append(moving_window_room_5, sensor_data$room_5[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_5, na.rm = T)
        
        sensor_data$Pp_room_5[i] <- sensor_data$P_room_5[i-1] + Q
        sensor_data$K_room_5[i] <- sensor_data$Pp_room_5[i] / (sensor_data$Pp_room_5[i] + Rs)
        sensor_data$fk_room_5[i] <- sum(c(sensor_data$fk_room_5[i-1],
                                            sensor_data$K_room_5[i] * sum(c(sensor_data$room_5[i], -sensor_data$fk_room_5[i-1]), na.rm=T)),
                                          na.rm=T)
        sensor_data$P_room_5[i] <- (1 - sensor_data$K_room_5[i]) * sensor_data$Pp_room_5[i]

        # room_6
        if(sensor_data$room_6[i] == -95 | is.na(sensor_data$room_6[i])) sensor_data$room_6[i] <- sensor_data$room_6[i-1]
        
        moving_window_room_6 <- append(moving_window_room_6, sensor_data$room_6[i], after = 0)[1:(6*k)]
        Rs <- var(moving_window_room_6, na.rm = T)
        
        sensor_data$Pp_room_6[i] <- sensor_data$P_room_6[i-1] + Q
        sensor_data$K_room_6[i] <- sensor_data$Pp_room_6[i] / (sensor_data$Pp_room_6[i] + Rs)
        sensor_data$fk_room_6[i] <- sum(c(sensor_data$fk_room_6[i-1],
                                            sensor_data$K_room_6[i] * sum(c(sensor_data$room_6[i], -sensor_data$fk_room_6[i-1]), na.rm=T)),
                                          na.rm=T)
        sensor_data$P_room_6[i] <- (1 - sensor_data$K_room_6[i]) * sensor_data$Pp_room_6[i]

    }
    
    return(sensor_data)
    
}

###################

filtered_rssi_data <- data.frame() 

for(i in unique(rssi_data$user_id)) {
    
    print(paste("Processing user", i))
    
    user_data <- rssi_data %>% filter(user_id == i)
    
    filtered_data <- noise_filtering(user_data)
    
    print(paste("Binding user", i, "data"))
    
    filtered_rssi_data <- bind_rows(filtered_rssi_data, filtered_data)
    
    print(paste("Row numbers = ", nrow(filtered_rssi_data)))
    
}

<<<<<<< HEAD
=======
filtered_rssi_data <- filtered_rssi_data %>%
    select(user_id, datetime, starts_with("room_"), starts_with("fk_room_"))

>>>>>>> parent of 287914b... M
saveRDS(filtered_rssi_data, "Data/filtered_rssi_data.RDS")
write_csv(filtered_rssi_data, "Data/filtered_rssi_data.csv")
