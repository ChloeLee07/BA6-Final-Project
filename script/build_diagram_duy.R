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
source("processData.R")

build_diagram_duy <- function(map.type) {

myData <- process_data(2018)

myMap1 <- get_googlemap(
  center = c(lon = -122.3035, lat = 47.65534),
  maptype = "roadmap",
  zoom = 14
)

ggmap(myMap1) +
  geom_point(aes(x = myData$Longitude, y = myData$Latitude),
    data = myData,
    alpha = 0.25,
    color = "darkred",
    size = 4
  ) +
  labs(x = NULL, y = NULL, title = "2018 Crime Distribution At UW", size = 5)

myMap2 <- get_googlemap(
  center = c(lon = -122.3035, lat = 47.65534),
  maptype = "roadmap",
  zoom = 14
)

ggmap(myMap2, extent = "device") +
  geom_density2d(
    data = myData, aes(x = myData$Longitude, y = myData$Latitude),
    size = 0.3
  ) +
  stat_density2d(
    data = myData, aes(x = myData$Longitude, y = myData$Latitude, fill = ..level.., alpha = ..level..),
    size = 0.01,
    bins = 15, geom = "polygon"
  ) +
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0, 0.75), guide = FALSE) +
  labs(x = NULL, y = NULL, title = "Crime Density At UW", size = 5)

myData3 <- myData %>%
  filter(Time_Reported < "06:00:00" | Time_Reported > "18:00:00")

myMap3 <- get_map(
  location = "university of washington",
  maptype = "roadmap",
  zoom = 14
)
ggmap(myMap3) +
  geom_point(aes(x = myData3$Longitude, y = myData3$Latitude),
    data = myData3,
    alpha = 0.25,
    color = "darkred",
    size = 4
  ) +
  labs(x = NULL, y = NULL, title = "UW Crime Distribution From 6PM to 6AM (Nighttime)", size = 5)


myData4 <- myData %>%
  filter(Time_Reported > "06:00:00" & Time_Reported < "18:00:00")

myMap4 <- get_map(
  location = "university of washington",
  maptype = "roadmap",
  zoom = 14
)

ggmap(myMap4) +
  geom_point(aes(x = myData4$Longitude, y = myData4$Latitude),
    data = myData4,
    alpha = 0.25,
    color = "darkred",
    size = 4
  ) +
  labs(x = NULL, y = NULL, title = "UW Crime Distribution From 6AM to 6PM (Daytime)", size = 5)

 if(map.type == "year_crime"){
   return (myMap1)
 } else if(map.type == "dangerous_place"){
   return (myMap2)
 } else if (map.type == "nighttime") {
   return (myMap3)
 } else {
   return (myMap4)
 }   


}
