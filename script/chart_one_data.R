source("processData.R")
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
