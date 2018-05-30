library("dplyr")
library("shiny")
library("ggplot2")
library("lintr")
library("ggthemes")
library("plotly")

# source in the data
source("processData.R")
source("script/chart_one_data.R")
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

  #duy
  output$duy_plot <- renderPlot({
    return(build_diagram_duy(input$radiovar))
  })
})
