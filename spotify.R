#import library
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gghighlight)
library(ggrepel)

data <- read.csv("data/esteban/data.csv")

# define the ts column as a date/time object
data$ts <- as.POSIXct(data$ts, format="%Y-%m-%d %H:%M:%S")

# create a new column with the day of the week
data$day <- weekdays(data$ts)

# Quick informations about the data
summary(data)

df_filtered <- data[data$genres %in% "vaporwave", ]
write.csv(df_filtered, "data/vaporwave.csv")

# Return the top 10 artists listened and print it
top10artists <- data %>% group_by(master_metadata_album_artist_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(10) %>% ungroup()

# Group_by every songs by month/year and add the ms_played each time its grouped 
evolution <- data %>% group_by(master_metadata_album_artist_name, month =as.Date(floor_date(ts,"month"))) %>% summarise(hours=sum(ms_played/3600000))

# Plot the evolution of the top 10 artists listened
ggplot(evolution, aes(x=month, y=hours, color=master_metadata_album_artist_name)) + 
    geom_line() +
    scale_x_date(date_breaks = "9 month", date_labels = "%b %Y") +
    gghighlight(master_metadata_album_artist_name %in% top10artists$master_metadata_album_artist_name, label_params = list(size = 10)) +
    labs(title = "Evolution of the top 10 artists listened", subtitle = "From 2018 to 2023", color = "Top 10 artists") +
    xlab("")+
    theme_light()


#save it in a csv file
write.csv(evolution, "data/evolution.csv")