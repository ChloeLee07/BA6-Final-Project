# extracting data from 2010 to 2018
police_data <- process_data(2010)

# data sorted for making a histogram
# also removing some of the strings that I think details aren't important
chart_one_data <- police_data %>%
  mutate(Offense_Type = gsub("^(NARC-[A-Z]+|FRAUD+|ROBBERY-[A-Z]+|
                             |BURGLARY-[A-Z]+|DISPUTE+|DISTURBANCE+|
                             |PROPERTY LOST+|PROPERTY STOLEN+|
                             |ROBBERY-[A-Z]+|VEH-[A-Z]+|HOMICIDE+|
                             |PROPERTY DAMAGE+|FORGERY+|THEFT+|ASSLT+).*",
                             "\\1",  Offense_Type)) %>%
  group_by(Year, Offense_Type) %>%
  count(Offense_Type) %>%
  rename(number = n)
# testing
################################ delete later ##################################
x_data <- chart_one_data %>%
  filter(Year == "2018")
ggplot(x_data, aes(x = Offense_Type, y = number, fill = number)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = number), size = 1.4, vjust = -0.5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   size = 5, margin = margin(2, 10, 2, 10)),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white",
                                         size = 0.5, linetype = "solid",
                                         colour = "black"),
        legend.title = element_text(size = 8)) +
  xlab("Offense Type") +
  ylab("Numbers") +
  scale_fill_gradient(low = rgb(77 / 255, 188 / 255, 233 / 255),
                      high = rgb(204 / 255, 42 / 255, 65 / 255))
################################ delete later ##################################

# data sorted for making a time series chart
# still need specific date to make the graph perfect
time_series_data <- police_data %>%
  group_by(Year) %>%
  count(Year) %>%
  rename(number = n)

# making the time series chart
# ggplot(data = time_series_data, aes(x = Year, y = number))+
#   geom_line(color = "#00AFBB", size = 2)

