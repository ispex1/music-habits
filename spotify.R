#import library
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gghighlight)
library(ggrepel)
library(tidyr)

data <- read.csv("data/esteban/data.csv")

# define the ts column as a date/time object
data$ts <- as.POSIXct(data$ts, format="%Y-%m-%d %H:%M:%S")

# create a new column with the day of the week
data$day <- weekdays(data$ts)

# Quick informations about the data
#summary(data)

# top 10 artists listened
data_artists <- data %>% group_by(master_metadata_album_artist_name) %>% summarise(count=n(), hours=sum(ms_played/3600000)) %>% arrange(desc(count))
# I want to add a column with the percentage of the total number of songs listened
data_artists <- data_artists %>% mutate(percentage = count / sum(count) * 100)
# Add an index column
data_artists <- data_artists %>% mutate(index = 1:n())

# Create a cumulative percentage column
data_artists <- data_artists %>% arrange(desc(count)) %>% mutate(cumulative_percentage = cumsum(percentage))

# save it in a csv file
write.csv(data_artists, "data/data_artists.csv")

