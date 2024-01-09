#import library
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gghighlight)
library(ggrepel)
library(tidyr)
library(reshape2)

data <- read.csv("data/data_este.csv")

#make ts a date
data$ts <- as.POSIXct(data$ts, format = "%Y-%m-%d %H:%M:%S")

data_genre <- data %>% 
  separate_rows(global_genre, sep = ", ") %>% 
  mutate(month = floor_date(ts, "2 months")) %>% 
  group_by(month, global_genre) %>% 
  summarise(count = n()) %>% 
  ungroup()

data_genre <- data_genre %>%
  group_by(month) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# Plot the geom_area
ggplot(data_genre, aes(x = month, y = percent, fill = global_genre)) +
  geom_area() +
  labs(title = "Tendances des genres musicaux au fil du temps",
       x = "Année",
       y = "Pourcentage",
       fill = "Genre") +
  theme_minimal()

  

