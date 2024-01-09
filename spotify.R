#import library
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(gghighlight)
library(ggrepel)
library(tidyr)
library(reshape2)

# Read the data
data <- read.csv("data/data_este.csv")
data_gab <- read.csv("data/data_gab.csv")

# STRIPE CHART WITH THE HOURS PLAYED BY WEEK

# Define the color palette with 9 different colors from orange to dark red
col_pal <- colorRampPalette(c("#ffa600", "#FF5733", "#C70039", "#900C3F", "#520404"))(9)

# Stream hours by week
weekly_time <- data %>% 
  group_by(ts = floor_date(ts, "week")) %>% 
  summarize(hours_played = sum(ms_played)/(3600000)) %>%
  arrange(ts) %>%
  ggplot(aes(x = ts, y = 1)) +
  geom_col(aes(fill = hours_played))+
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_pal), trans='reverse')+
  guides(fill = guide_colorbar(barwidth = 1))+
  theme_void()
weekly_time


# EVOLUTION OF THE TOP 10 ARTISTS LISTENED

# Return the top 10 artists listened
top10artists <- data %>%
  count(master_metadata_album_artist_name, sort = TRUE) %>%
  head(10)

# Group_by every songs by month/year and add the ms_played each time its grouped 
evolution <- data %>% 
  group_by(master_metadata_album_artist_name, month=as.Date(floor_date(ts,"month"))) %>% 
  summarise(hours=sum(ms_played/3600000))

# Plot the evolution of the top 10 artists listened
ggplot(evolution, aes(x=month, y=hours, color=master_metadata_album_artist_name)) + 
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    gghighlight(master_metadata_album_artist_name %in% top10artists$master_metadata_album_artist_name, label_params = list(size = 10)) +
    labs(title = "Evolution of the top 10 artists listened", subtitle = "From 2018 to 2023", color = "Top 10 artists") +
    xlab("")+
    theme_light()


# REPARTITION FUNCTION

# Creation of data_artists ; the number of songs listened by artist + the % 
data_artists <- data %>% 
    group_by(master_metadata_album_artist_name) %>%
    summarise(count=n(), hours=sum(ms_played/3600000)) %>%
    arrange(desc(count)) %>% 
    mutate(percentage = count / sum(count) * 100) %>% 
    mutate(cumulative_percentage = cumsum(percentage)) %>% 
    mutate(index = row_number())

# Return the index of the top x percent artists listened in count
top_1_percent <- data_artists[data_artists$count >= quantile(data_artists$count, 0.99), ] %>% tail(1)
top_5_percent <- data_artists[data_artists$count >= quantile(data_artists$count, 0.95), ] %>% tail(1)
top_10_percent <- data_artists[data_artists$count >= quantile(data_artists$count, 0.90), ] %>% tail(1)
top_50_percent <- data_artists[data_artists$count >= quantile(data_artists$count, 0.50), ] %>% tail(1)

indices_to_show <- c(10, top_1_percent$index, top_5_percent$index, top_10_percent$index, top_50_percent$index)
custom_labels <- c("Top 10", "Top 1%", "Top 5%", "Top 10%", "Top 50%")

# Plot the curve
ggplot(data_artists, aes(x = index, y = cumulative_percentage)) +
    geom_line() +
    geom_point(  data = data_artists[data_artists$index %in% indices_to_show, ], 
                shape = 16, 
                color = "#C70039", 
                size = 2) +
    geom_text(  data = data_artists[data_artists$index %in% indices_to_show, ], 
                aes(label = custom_labels,), 
                hjust = -0.1, 
                vjust = 1.5, 
                size = 3) +
    labs( title = "Repartition function",
        subtitle = "Between the rank and the total percentage",
        x = "Rank",
        y = "Cumulative percentage")+
    theme_light()


# EVOLUTION OF THE GENRES TRENDS

data_genre <- data %>% 
  separate_rows(global_genre, sep = ", ") %>% 
  mutate(month = floor_date(ts, "2 months")) %>% 
  group_by(month, global_genre) %>% 
  summarise(count = n(), .groups = "drop_last") %>% 
  group_by(month) %>% 
  mutate(percent = count / sum(count) * 100)

ggplot(data_genre, aes(x = month, y = percent, fill = global_genre)) +
  geom_area() +
  labs(title = "Musical genre trends over time",
       x = "Year",
       y = "Percentage",
       fill = "Genre") +
  theme_minimal()