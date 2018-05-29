library("dplyr")
library("stringr")
library("chron")

# TODO: actually use Socrata API instead of CSV
data <- read.csv("data/police_data.csv", stringsAsFactors = FALSE)

# This function returns the processed data frame.
# Dataset is limited to entries between 'starting_year' and current date 
# USAGE: process_data(2010) will return the dataset with all entries 2010-today
###
# For times like Time_Reported:
#   hour = as.numeric(hours(time))
#   minutes = as.numeric(minutes(time))
#   seconds = as.numeric(seconds(time))
#   where time could be "process_data(2018)$Time_Reported[1]"
# For dates like Date_Reported:
#   day = as.numeric(days(date))
#   month = as.numeric(months(date))
#   year does not work as easily for some reason
#   - use the year column or "as.numeric(as.character(years(date)))" lol... 
#   where date could be "process_data(2018)$Date_Reported[1]"
process_data <- function(starting_year) {
  processed_data <- data %>% filter(Year >= starting_year)
  names(processed_data) <- gsub("[.]", "_", names(processed_data))
  
  processed_data <- processed_data %>%
    select(
      RMS_CDW_ID,
      Date_Reported,
      Offense_Code,
      Offense_Type,
      Summary_Offense_Code,
      Summarized_Offense_Description,
      Hundred_Block_Location,
      Longitude,
      Latitude,
      Month,
      Year
    )  %>%
    rename(ID = RMS_CDW_ID) %>%
    mutate(
      Date_Time_Reported = as.POSIXct(Date_Reported, format = '%m/%d/%Y %I:%M:%S %p'),
      Date_Reported = dates(strftime(Date_Time_Reported, format = '%Y-%m-%d'), format = "y-m-d"),
      Time_Reported = times(strftime(Date_Time_Reported, format = '%H:%M:%S'))
    )
}


