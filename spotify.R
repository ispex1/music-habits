#import library
library(ggplot2)
library(dplyr)
library(readr)


data <- read.csv("data.csv")

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

# save data as a csv file
write.csv(data, "data.csv")
