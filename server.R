library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")
library("plotly")

# source in the data
source("processData.R")
source("script/chart_one_data.R")
source("script/chart_three.R")
source("./script/build_diagram_duy.R")

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
  
  #robin chart3
  output$chart_three <- renderPlotly({
    # return(plot_chart_three(input$choice_three))
    # by_year_data <-
    #   group_by(my_data, Year) %>% 
    #   summarize(number = n())
    # plot_ly(by_year_data, x = ~Year, y = ~number, mode = 'lines+markers', type = 'scatter', line = list(shape = "spline"),
    #                  text = ~paste('Year: ', Year, 
    #                                "</br>Number of incidents: ", number)) %>% 
    #   layout(title = "Number of incidents by year",
    #          xaxis = list(title = "Year"),
    #          yaxis = list(title = "Number of incidents"))
    my_data <- get_chart_three_data(2010)
    
    my_data <- process_data_with_dates(year) %>% 
      mutate(Hour_Occurred = as.numeric(hours(Time_Occurred)),
             day_Occurred = as.numeric(days(Date_Occurred)),
             weekday = weekdays(Date_Occurred, abbreviate = FALSE))
    
    if(input$choice_three == "year") {
      by_year_data <-
        group_by(my_data, Year) %>% 
        summarize(number = n())
      plot_ly(by_year_data, x = ~Year, y = ~number, mode = 'lines+markers', type = 'scatter', line = list(shape = "spline"),
              text = ~paste('Year: ', Year, 
                            "</br>Number of incidents: ", number)) %>% 
        layout(title = "Number of incidents by year",
               xaxis = list(title = "Year"),
               yaxis = list(title = "Number of incidents"))
    } else if(input$choice_three == "week") {
      by_week_data <-
        group_by(my_data, weekday) %>% 
        summarize(number = n())
      plot_ly(by_week_data, x = ~weekday, y = ~number, mode = 'lines+markers', type = 'scatter', line = list(shape = "spline"),
              text = ~paste('Week: ', weekday, 
                            "</br>Number of incidents: ", number)) %>% 
        layout(title = "Number of incidents by weekday",
               xaxis = list(title = "Weekdays"),
               yaxis = list(title = "Number of incidents"))
    } else if(input$choice_three == "hour") {
      by_hour_data <-
        group_by(my_data, Hour_Occurred) %>% 
        summarize(number = n())
      plot_ly(by_hour_data, x = ~Hour_Occurred, y = ~number, mode = 'lines+markers', type = 'scatter', line = list(shape = "spline"),
              text = ~paste('Hour: ', Hour_Occurred, 
                            "</br>Number of incidents: ", number)) %>% 
        layout(title = "Number of incidents by hour",
               xaxis = list(title = "Hour",
                            dtick=1,
                            range = c(0,23)),
               yaxis = list(title = "Number of incidents"))
    }
  })
  
  #duy
  output$duy_plot <- renderPlot({
    return(build_diagram_duy(input$radiovar))
  })
})
