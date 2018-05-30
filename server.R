library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")
library("plotly")

# source in the data
source("processData.R")
source("script/chart_one_data.R")
source("script/build_diagram_duy.R")
source("script/chart_four_data.R")

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

  # not pretty, but working
  output$hist_four <- renderPlot({
    if (input$months_mode) {
      if (input$off_type == 'ALL') {
        filtered_data <- chart_four_all_month_data %>%
          filter(input$span[1] <= Year & Year <= input$span[2])
      } else {
        filtered_data <- chart_four_month_data %>%
          filter(
            Offense_Type == input$off_type & 
              input$span[1] <= Year &
              Year <= input$span[2])
      }
      
      lhs <- get_year_month(input$span[1], 1)
      rhs <- get_year_month(input$span[2], 12)
      ggplot(filtered_data) +
        scale_x_discrete(limits = 
                           year_month_ordering[lhs <= year_month_ordering & 
                                                 year_month_ordering <= rhs]) +
        geom_rect(aes(
          xmin = get_year_month(Year, Month), 
          xmax = get_year_month(Year, Month + 1),
          fill = factor(Year)),
          ymin = -Inf, ymax = Inf, alpha = 0.2,
          data = filtered_data) +
        geom_line(aes(x = Year_Month, y = n, group = 1)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 5)) +
        xlab("Time Frame (Year_Month)") +
        ylab("Occurrences") +
        labs(fill = "Year")
    } else {
      if (input$off_type == 'ALL') {
        filtered_data <- chart_four_all_year_data %>%
          filter(input$span[1] <= Year & Year <= input$span[2])
      } else {
        filtered_data <- chart_four_year_data %>%
          filter(
            Offense_Type == input$off_type & 
              input$span[1] <= Year &
              Year <= input$span[2])
      }
      
      ggplot(filtered_data) +
        scale_x_discrete(limits = filtered_data$Year) +
        geom_rect(aes(
          xmin = Year, 
          xmax = Year + 1,
          fill = factor(Year)),
          ymin = -Inf, ymax = Inf, alpha = 0.2,
          data = filtered_data) +
        geom_line(aes(x = Year, y = n)) +
        theme(axis.text.x = element_text(hjust = 0.5)) +
        xlab("Year") +
        ylab("Occurrences") +
        labs(fill = "Year")
    }
    
  })
  
  #duy
  output$duy_plot <- renderPlot({
    return(build_diagram_duy(input$radiovar))
  })
})

