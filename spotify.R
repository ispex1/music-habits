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

# convert ts to POSIXct
data$ts <- as.POSIXct(data$ts, format = "%Y-%m-%d %H:%M:%S")

# STRIPCHART

# Define the color palette with 9 different colors from orange to dark red
col_pal <- colorRampPalette(c("#ffa600", "#FF5733", "#C70039", "#900C3F", "#520404"))(9)

# Stream hours by week
# here it is more pertinent to plot by the amount of time listened than by the nb of stream
weekly_time <- data %>% 
  group_by(week = floor_date(ts, "week")) %>% 
  summarize(hours_played = sum(ms_played)/(3600000)) %>%
  arrange(week)

ggplot(weekly_time, aes(x = week, y = 1)) +
  geom_col(aes(fill = hours_played))+
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_pal), trans='reverse')+
  guides(fill = guide_colorbar(barwidth = 1))+
  theme_void()+
  labs(fill = NULL)


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
    labs(title = "Evolution of the top 10 artists listened", subtitle = "From 2018 to 2023", color = "Top 10 artists", x=element_blank(), y="Hours listened")+
    theme_minimal()+
    theme(plot.margin = margin(t = 10, r = 20, b = 30, l = 20))


# REPARTITION FUNCTION

# creation of data_artists ; the number of songs listened by artist + the % 
data_artists <- data %>% 
    group_by(master_metadata_album_artist_name) %>%
    summarise(count=n(), hours=sum(ms_played/3600000)) %>%
    arrange(desc(count)) %>% 
    mutate(percentage = count / sum(count) * 100) %>% 
    mutate(cumulative_percentage = cumsum(percentage)) %>% 
    mutate(index = row_number())

#return the index of the top artists listened in count
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
  labs(title = "Trends of the genres listened", subtitle = "From 2018 to 2023", x = element_blank() ,y = "Percentage") +
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())


# COMPARISON OF THE GENRES LISTENED BETWEEN ESTEBAN AND GABRIEL

# Read the data
data_gab <- read.csv("data/data_gab.csv")

## Create a recapitulative dataframe of the genres
create_data_genre <- function(data) {
  data <- data %>% 
  separate_rows(global_genre, sep = ", ") %>% 
  mutate(global_genre = trimws(global_genre)) %>%
  group_by(global_genre) %>% 
  summarise(count = n(), hours = sum(ms_played/3600000)) %>% 
  arrange(desc(count))
  
  return(data)
}

## Apply function
data_genre_este <- create_data_genre(data)
data_genre_gab <- create_data_genre(data_gab)

## Merging in one dataframe and calculate percentages
total <- merge(data_genre_este, data_genre_gab, by = "global_genre", all = TRUE) %>%
  mutate(
    total = count.x + count.y,
    percent_este = count.x / total * 100,
    percent_gab = count.y / total * 100
  ) %>%
  select(global_genre, total, percent_este, percent_gab) %>%
  pivot_longer(cols = c(percent_gab, percent_este), names_to = "category", values_to = "percentage")

## plot I want the y axis to be Genre and the x axis to be the Percentage
ggplot(total, aes(x = global_genre, y = percentage, fill = category)) +
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  geom_hline(yintercept = 50, linetype = "dashed")+
  scale_fill_discrete("",
                      labels=c("Esteban", "Gabriel"))+
  theme_minimal() +
  labs(title = "Comparison of the genres listened between Esteban and Gabriel",
        x = "Genre",
        y = "Percentage") +
  theme(legend.position = "bottom")

