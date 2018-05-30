library(ggplot2)
library(ggthemes)
library(ggmap)
library(ggalt)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)
library(lubridate)
library(RJSONIO)
library(magrittr)
source("processData.R")

myData <- process_data(2018)
myNighttimeData <- myData %>%
  filter(Time_Reported < "06:00:00" | Time_Reported > "18:00:00")
myDaytimeData <- myData %>%
  filter(Time_Reported > "06:00:00" & Time_Reported < "18:00:00")


build_diagram_duy <- function(map.type) {
  myMap <- get_googlemap(
    center = c(lon = -122.3035, lat = 47.65534),
    maptype = "roadmap",
    zoom = 14
  )
  
  if (map.type == "year_crime") {
    ggmap(myMap) +
      geom_point(aes(x = myData$Longitude, y = myData$Latitude),
                 data = myData,
                 alpha = 0.25,
                 color = "darkred",
                 size = 4
      ) +
      labs(x = NULL, y = NULL, title = paste("2018 Crime Distribution At UW")) +
      theme(plot.title = element_text(color = "#666666", face = "bold",
                                      size = 20, hjust = 0)) +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (map.type == "dangerous_place") {
    ggmap(myMap, extent = "device") +
      geom_density2d(
        data = myData, aes(x = myData$Longitude, y = myData$Latitude),
        size = 0.3
      ) +
      stat_density2d(
        data = myData, aes(x = myData$Longitude, y = myData$Latitude,
                           fill = ..level.., alpha = ..level..),
        size = 0.01,
        bins = 15, geom = "polygon"
      ) +
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0, 0.75), guide = FALSE) +
      labs(x = NULL, y = NULL, title = "Crime Density At UW", size = 15,
           fill = "Crime Density") +
      theme(plot.title = element_text(color = "#666666", face = "bold",
                                      size = 20, hjust = 0)) +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else if (map.type == "nighttime") {
    ggmap(myMap) +
      geom_point(aes(x = myNighttimeData$Longitude, y = myNighttimeData$Latitude),
                 data = myNighttimeData,
                 alpha = 0.25,
                 color = "slateblue",
                 size = 4
      ) +
      labs(x = NULL, y = NULL,
           title = "UW Crime Distribution From 6PM to 6AM (Nighttime)",
           size = 15) +
      theme(plot.title = element_text(color = "#666666", face = "bold",
                                      size = 20, hjust = 0)) +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    ggmap(myMap) +
      geom_point(aes(x = myDaytimeData$Longitude, y = myDaytimeData$Latitude),
                 data = myDaytimeData,
                 alpha = 0.25,
                 color = "coral",
                 size = 4
      ) +
      labs(x = NULL, y = NULL,
           title = "UW Crime Distribution From 6AM to 6PM (Daytime)",
           size = 15) +
      theme(plot.title = element_text(color = "#666666",
                                      face = "bold", size = 20, hjust = 0)) +
      theme(plot.title = element_text(hjust = 0.5))
  }  
}
