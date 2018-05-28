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

# process_data(2018)




