library(plotly)
source("processData.R")

my_data <- process_data_with_dates(year) %>%
  mutate(Hour_Occurred = format(strptime(my_data$Time_Occurred,"%H:%M:%S"),"%H"),
         weekday = weekdays(Date_Occurred, abbreviate = FALSE))

chart_three <- function(year, choice_three) {

  if (choice_three == 1) {
    by_year_data <-
      group_by(my_data, Year) %>%
      summarize(number = n())
    plot_ly(by_year_data, x = ~Year, y = ~number, mode = "lines+markers",
            type = "scatter", line = list(shape = "spline"),
            text = ~paste("Year: ", Year,
                          "</br>Number of incidents: ", number)) %>%
      layout(title = "Number of incidents by year",
             xaxis = list(title = "Year",
                          dtick = 1),
             yaxis = list(title = "Number of incidents"))
  } else if (choice_three == 2) {
    by_week_data <-
      group_by(my_data, weekday) %>%
      summarize(number = n())
    plot_ly(by_week_data, x = ~weekday, y = ~number, mode = "lines+markers",
            type = "scatter", line = list(shape = "spline"),
            text = ~paste("Week: ", weekday,
                          "</br>Number of incidents: ", number)) %>%
      layout(title = "Number of incidents by weekday",
             xaxis = list(title = "Weekdays"),
             yaxis = list(title = "Number of incidents"))
  } else if (choice_three == 3) {
    by_hour_data <-
      group_by(my_data, Hour_Occurred) %>%
      summarize(number = n())
    plot_ly(by_hour_data, x = ~Hour_Occurred, y = ~number,
            mode = "lines+markers", type = "scatter",
            line = list(shape = "spline"),
            text = ~paste("Hour: ", Hour_Occurred,
                          "</br>Number of incidents: ", number)) %>%
      layout(title = "Number of incidents by hour",
             xaxis = list(title = "Hour",
                          dtick = 1,
                          range = c(0, 23)),
             yaxis = list(title = "Number of incidents"))
  }
}