# Is the safety around Seattle trending up or down?
library("dplyr")
source("processData.R")

dataset <- process_data_with_dates(2010)

chart_four_year_data <- dataset %>% 
  group_by(Year, Offense_Type) %>%
  summarise(n = n()) %>%
  ungroup()

chart_four_all_year_data <- dataset %>% 
  group_by(Year) %>%
  summarise(n = n()) %>%
  ungroup()

get_year_month <- function(year, month) {
  paste0(year, ifelse(month < 10, '_0', '_'), month)
}

chart_four_month_data <- dataset %>% 
  group_by(Year, Month, Offense_Type) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Year_Month = get_year_month(Year, Month)) %>%
  arrange(Year, Month)

year_month_ordering <- unique(chart_four_month_data$Year_Month)

chart_four_month_data <- chart_four_month_data %>%
  mutate(Year_Month = factor(Year_Month, year_month_ordering))

chart_four_all_month_data <- dataset %>% 
  group_by(Year, Month) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(Year_Month = factor(get_year_month(Year, Month), year_month_ordering)) %>%
  arrange(Year, Month)

