library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")
library("plotly")
library("ggmap")
library("ggalt")
library("viridis")
library("scales")
library("grid")
library("gridExtra")

library("lubridate")
library("RJSONIO")
library("magrittr")

# source in the data
source("processData.R")
source("script/chart_one_data.R")
#source("./script/build_diagram_duy.R")

shinyServer(function(input, output) {

  output$hist <- renderPlotly ({
    # use the selected year as y value to produce the chart
    x_data <- chart_one_data %>%
      filter(Year == input$year_choice)

    #create the histogram
    info <- paste0("Offense Type: ", x_data[["Offense_Type"]], "<br>",
                   "Number: ", x_data[["number"]])
    ggplotly(ggplot(x_data, aes(x = Offense_Type, y = number, fill = number,
                                text = info)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                       size = 4, margin = margin(2, 10, 2, 10)),
            axis.title.y = element_text(angle = 0, vjust = 0.5),
            legend.background = element_rect(fill = "white",
                                             size = 0.5, linetype = "solid",
                                             colour = "black"),
            legend.title = element_text(size = 8)) +
      xlab("Offense Type") +
      ylab("Numbers") +
      scale_fill_gradient(low = rgb(77 / 255, 188 / 255, 233 / 255),
                          high = rgb(204 / 255, 42 / 255, 65 / 255)),
      tooltip = "info") %>%
      layout(margin = list(l = 50, r = 100, b = 170, t = 20, pad = 4))
  })
  
  #duy
  myData <- process_data("2018")
  output$duy_plot <- renderPlot({
    myMap <- get_googlemap(
      center = c(lon = -122.3035, lat = 47.65534),
      maptype = "roadmap",
      zoom = 14
    )
    
    if (input$radiovar == "year_crime") {
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
      
    } else if (input$radiovar == "dangerous_place") {
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
      
    } else if (input$radiovar == "nighttime") {
      myNighttimeData <- myData %>%
        filter(Time_Reported < "06:00:00" | Time_Reported > "18:00:00")
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
      myDaytimeData <- myData %>%
        filter(Time_Reported > "06:00:00" & Time_Reported < "18:00:00")
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
  })
})
