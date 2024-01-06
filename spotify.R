#import library
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
install.packages("data.tree")
library(data.tree)


data <- read.csv("data/data.csv")

# define the ts column as a date/time object
data$ts <- as.POSIXct(data$ts, format="%Y-%m-%d %H:%M:%S")

# create a new column with the hour of the day
data$hour <- hour(data$ts)

# create a new column with the day of the week
data$day <- weekdays(data$ts)

# Quick informations about the data
summary(data)

# Plot the number of songs played by day of the week
ggplot(data, aes(x=day)) + geom_bar() + ggtitle("Number of songs played by day of the week")

# Plot the number of songs played by hour of the day
ggplot(data, aes(x=hour(ts))) + geom_bar() + ggtitle("Number of songs played by hour of the day")

#Stream hours by week
#here it is more pertinent to plot by the amount of time listened than by the nb of stream
weekly_time <- data %>% 
  group_by(ts = floor_date(ts, "week")) %>% 
  summarize(hours_played = sum(ms_played)/(1000*60*60)) %>%
  arrange(ts) %>%
  ggplot(aes(x = ts, y = hours_played)) +
  geom_col(aes(fill = hours_played))

weekly_time



# save data as a csv file
write.csv(data, "data.csv")

