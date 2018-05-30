library("dplyr")
library("stringr")

# TODO: actually use Socrata API instead of CSV
data <- read.csv("data/police_data.csv", stringsAsFactors = FALSE)

# This function returns the processed data frame.
# Dataset is limited to entries between 'starting_year' and current date 
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
    # This date and time stuff is gross. TODO: try chron package
    # Using POSIXct defines ordering for the dates and times, so you
    # can use arrange and other sorting functions.
    mutate(
      Date_Time_Reported = as.POSIXct(Date_Reported, format = '%m/%d/%Y %I:%M:%S %p'),
      Date_Reported = as.Date(Date_Time_Reported),
      Time_Reported = strftime(Date_Time_Reported, format = '%H:%M:%S')
    )
}


# This function returns the processed data frame.
# Dataset is limited to entries between 'starting_year' and current date 
# USAGE: process_data(2010) will return the dataset with all entries 2010-today
###
# For times like Time_Reported:
#   hour = as.numeric(hours(time))
#   minutes = as.numeric(minutes(time))
#   seconds = as.numeric(seconds(time))
#   where time could be "process_data(2018)$Time_Reported[1]"
##
# For dates like Date_Reported:
#   day = as.numeric(days(date))
#   month = as.numeric(months(date))
#   year does not work as easily for some reason
#   - use the year column or "as.numeric(as.character(years(date)))" lol... 
#   where date could be "process_data(2018)$Date_Reported[1]"
##
# Date_Time columns like Date_Time_Reported are chron objects and can use the time
# methods (hours, minutes, seconds) and the date methods (years, months, days)
##
# You can subtract dates and get a chron object: 
#   duration <- Date_Time_Occurred_End - Date_Time_Occurred
#   duration_in_days <- as.numeric(days(duration))
#   duration_in_hours <- as.numeric(hours(duration))


process_data_with_dates <- function(starting_year) {
  library("chron")
  processed_data <- data %>% filter(Year >= starting_year)
  names(processed_data) <- gsub("[.]", "_", names(processed_data))
  
  processed_data <- processed_data %>%
    select(
      RMS_CDW_ID,
      Date_Reported,
      Occurred_Date_or_Date_Range_Start,
      Occurred_Date_Range_End,
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
    rename(
      ID = RMS_CDW_ID, 
      Date_Occurred_Start = Occurred_Date_or_Date_Range_Start,
      Date_Occurred_End = Occurred_Date_Range_End) %>%
    mutate(
      Date_Occurred_End = ifelse(Date_Occurred_End == "", Date_Occurred_Start, Date_Occurred_End),
      
      Date_Time_Occurred = as.POSIXct(Date_Occurred_Start, format = '%m/%d/%Y %I:%M:%S %p'),
      Date_Occurred = dates(strftime(Date_Time_Occurred, format = '%Y-%m-%d'), format = "y-m-d"),
      Time_Occurred = times(strftime(Date_Time_Occurred, format = '%H:%M:%S')),
      Date_Time_Occurred = chron(Date_Occurred, Time_Occurred),
      
      Date_Time_Occurred_End = as.POSIXct(Date_Occurred_End, format = '%m/%d/%Y %I:%M:%S %p'),
      Date_Occurred_End = dates(strftime(Date_Time_Occurred_End, format = '%Y-%m-%d'), format = "y-m-d"),
      Time_Occurred_End = times(strftime(Date_Time_Occurred_End, format = '%H:%M:%S')),
      Date_Time_Occurred_End = chron(Date_Occurred_End, Time_Occurred_End),
      
      Date_Time_Reported = as.POSIXct(Date_Reported, format = '%m/%d/%Y %I:%M:%S %p'),
      Date_Reported = dates(strftime(Date_Time_Reported, format = '%Y-%m-%d'), format = "y-m-d"),
      Time_Reported = times(strftime(Date_Time_Reported, format = '%H:%M:%S')),
      Date_Time_Reported = chron(Date_Reported, Time_Reported)
    ) %>%
    select(-Date_Occurred_Start)
}

