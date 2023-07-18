#load relevant libraries
library(tidyverse) #data exploration
library(lubridate) #time simplification
library(hms) #time conversion
library(data.table) #faster csv method

#load 12-month period, June 2022 to May 2023, csv data
jun_06_df <- read.csv("Csv_Data/202206-divvy-tripdata.csv")
jul_07_df <- read.csv("Csv_Data/202207-divvy-tripdata.csv")
aug_08_df <- read.csv("Csv_Data/202208-divvy-tripdata.csv")
sep_09_df <- read.csv("Csv_Data/202209-divvy-tripdata.csv")
oct_10_df <- read.csv("Csv_Data/202210-divvy-tripdata.csv")
nov_11_df <- read.csv("Csv_Data/202211-divvy-tripdata.csv")
dec_12_df <- read.csv("Csv_Data/202212-divvy-tripdata.csv")
jan_01_df <- read.csv("Csv_Data/202301-divvy-tripdata.csv")
feb_02_df <- read.csv("Csv_Data/202302-divvy-tripdata.csv")
mar_03_df <- read.csv("Csv_Data/202303-divvy-tripdata.csv")
apr_04_df <- read.csv("Csv_Data/202304-divvy-tripdata.csv")
may_05_df <- read.csv("Csv_Data/202305-divvy-tripdata.csv")

#merge all data frame into one
yearly_df <- rbind(jun_06_df, jul_07_df, aug_08_df, sep_09_df, oct_10_df, nov_11_df, dec_12_df, jan_01_df, feb_02_df, mar_03_df, apr_04_df, may_05_df)

#new data frame to insert new columns
yearly_data <- yearly_df

#add ride_length column which includes ride duration
yearly_data$ride_length <- difftime(yearly_df$ended_at, yearly_df$started_at, units = "mins")
yearly_data$ride_length <- round(yearly_data$ride_length, digits = 1)

#new columns for: year, month, day, hour, day of the week
yearly_data$date <- as.Date(yearly_data$started_at) #default formate is yyyy-mm-dd
yearly_data$day_of_week <- format(as.Date(yearly_data$date), "%A") #find the day of the week
yearly_data$year <- format(as.Date(yearly_data$date), "%Y") #create column for year
yearly_data$month <- format(as.Date(yearly_data$date), "%m") #create column for month
yearly_data$day <- format(as.Date(yearly_data$date), "%d") #create column for day
yearly_data$time <- format(as.POSIXct(yearly_df$started_at), format = "%H:%M:%S") #format time as HH:MM:SS
yearly_data$time <- as_hms(yearly_data$time) #convert into numerical hms
yearly_data$hour <- hour(yearly_data$time) #column for hour

#column for different season
yearly_data <- yearly_data %>% 
  mutate(season = case_when(month == "06" ~ "Summer",
                            month == "07" ~ "Summer",
                            month == "08" ~ "Summer",
                            month == "09" ~ "Fall",
                            month == "10" ~ "Fall",
                            month == "11" ~ "Fall",
                            month == "12" ~ "Winter",
                            month == "01" ~ "Winter",
                            month == "02" ~ "Winter",
                            month == "03" ~ "Spring",
                            month == "04" ~ "Spring",
                            month == "05" ~ "Spring",)
  )


#create column for different time of the day
yearly_data <-yearly_data %>% 
  mutate(time_of_day = case_when(hour == "0" ~ "Night",
                                 hour == "1" ~ "Night",
                                 hour == "2" ~ "Night",
                                 hour == "3" ~ "Night",
                                 hour == "4" ~ "Night",
                                 hour == "5" ~ "Night",
                                 hour == "6" ~ "Morning",
                                 hour == "7" ~ "Morning",
                                 hour == "8" ~ "Morning",
                                 hour == "9" ~ "Morning",
                                 hour == "10" ~ "Morning",
                                 hour == "11" ~ "Morning",
                                 hour == "12" ~ "Afternoon",
                                 hour == "13" ~ "Afternoon",
                                 hour == "14" ~ "Afternoon",
                                 hour == "15" ~ "Afternoon",
                                 hour == "16" ~ "Afternoon",
                                 hour == "17" ~ "Afternoon",
                                 hour == "18" ~ "Evening",
                                 hour == "19" ~ "Evening",
                                 hour == "20" ~ "Evening",
                                 hour == "21" ~ "Evening",
                                 hour == "22" ~ "Night",
                                 hour == "23" ~ "Night")
  )

#clean data frame
yearly_data <- na.omit(yearly_data) #remove NA values
yearly_data <- distinct(yearly_data) #remove duplicates
yearly_data <- yearly_data[!(yearly_data$ride_length <=0),] #remove 0 or negative ride_length
yearly_data <- yearly_data %>% 
  select (-c(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng)) #remove unnecessary columns

#export data for tableau visualization
fwrite(yearly_data,"Csv_Data/yearly_data.csv")