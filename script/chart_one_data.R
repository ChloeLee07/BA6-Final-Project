# data sorted for making a histogram
seattle_pd <- process_data(2010)

chart_one_data <- seattle_pd %>%
  group_by(Year, Offense_Type) %>%
  count(Offense_Type) %>%
  rename(number = n) 

# data sorted for making a time series chart
time_series_data <- seattle_pd %>%
  group_by(Year) %>%
  count(Year) %>%
  rename(number = n)
# ggplot(data = time_series_data, aes(x = Year, y = number))+
#   geom_line(color = "#00AFBB", size = 2)

