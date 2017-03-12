# DSIOT-FP-RemoteMonitoring

## Welcome

This repository includes the information and data related to the **Remote Monitoring Foundation Project** from the **Data Science for IoT Futuretext** course.

The project uses poximity sensors in different rooms to monitor activity of elderly people at home. We want to use the data from all the sensors to know the approximate location of the users and their state of activity. We also want to learn from their usual behaviour and use event pattern recognition to better infer the activities of the users and to detect anomalies so we can raise warns.
 
We have a data set for 12 users from whom we want to learn their pattern behaviour. We do not have a set of labeled anomalies. Moreover, anomalies can occurr in a lot of different (and unexpected) ways. So we must use unsupervised learning and  anomaly detection techniques.
 
You can participate in this project if you wish to suggest an approach for any of the problems, and then work on it.
 
To participate you must provide experience in at least one of:
- signal processing
- pattern mining and behaviour recognition
- complex event processing
- location from BLE sensors
- R/Python programming (R is preferred)

## News

2017-03-12

Added code for the Kalman filter. [KalmanFilter.R](https://github.com/aureliogarcia/DSIOT-FP-RemoteMonitoring/blob/master/Code/KalmanFilter.R)

Added a Shiny App to help visualize filtered data. Run server.R in Rstudio. [Filtered Data App](https://github.com/aureliogarcia/DSIOT-FP-RemoteMonitoring/tree/master/Apps/FileteredData)


2017-03-04

Added [ProjectOutline.pdf](https://github.com/aureliogarcia/DSIOT-FP-RemoteMonitoring/blob/master/ProjectOutline.pdf)


2017-02-24

You can access the project data in the [Data](https://github.com/aureliogarcia/DSIOT-FP-RemoteMonitoring/tree/master/Data) folder.

Added a Shiny App to help quickly plot and explore the data. Run server.R in RStudio.  [EDA App](https://github.com/aureliogarcia/DSIOT-FP-RemoteMonitoring/tree/master/Apps/EDA)



