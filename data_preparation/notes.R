# 1st plots to understand the data

# Plot the number of songs played by day of the week
ggplot(data, aes(x=day)) + geom_bar() + ggtitle("Number of songs played by day of the week")

# Plot the number of songs played by hour of the day
ggplot(data, aes(x=hour(ts))) + geom_bar() + ggtitle("Number of songs played by hour of the day")

# Plot the number of songs played by month
ggplot(data, aes(x=month(ts))) + geom_bar() + ggtitle("Number of songs played by month")

# Plot the number of songs played by year
ggplot(data, aes(x=year(ts))) + geom_bar() + ggtitle("Number of songs played by year")

# Plot the number of songs played by year and month
ggplot(data, aes(x=month(ts), fill=year(ts))) + geom_bar() + ggtitle("Number of songs played by year and month")

# Creation of a new column with the information if I am at home or not
data$home <- ifelse(data$platform == "Home Speaker", "Home", "Not Home")

# Plot the number of songs played by genre and if I am at home or not
ggplot(data, aes(x=home)) + geom_bar() + ggtitle("Number of songs played by genre and if I am at home or not")


---------------------------------------------------------------

# STRIPCHART

# Define the color palette with 9 different colors from orange to dark red
col_pal <- colorRampPalette(c("#ffa600", "#FF5733", "#C70039", "#900C3F", "#520404"))(9)

# Stream hours by week
# here it is more pertinent to plot by the amount of time listened than by the nb of stream
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

---------------------------------------------------------------

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

---------------------------------------------------------------

# REPARTITION FUNCTION

# creation of data_artists ; the number of songs listened by artist + the % 
data_artists <- data %>% 
    group_by(master_metadata_album_artist_name) %>%
    summarise(count=n(), hours=sum(ms_played/3600000)) %>%
    arrange(desc(count)) %>% 
    mutate(percentage = count / sum(count) * 100) %>% 
    mutate(cumulative_percentage = cumsum(percentage)) %>% 
    mutate(index = row_number())

#return the index of the top 1% and the top 5% artists listened in count
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
    
---------------------------------------------------------------

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
  labs(title = "Tendances des genres musicaux au fil du temps",
       x = "Année",
       y = "Pourcentage",
       fill = "Genre") +
  theme_minimal()

---------------------------------------------------------------

# COMPARISON OF THE GENRES LISTENED BETWEEN ESTEBAN AND GABRIEL

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

## plot
ggplot(df_long, aes(x = global_genre, y = percentage, fill = category)) +
  geom_col(position = position_stack(reverse = TRUE))+
  coord_flip()+
  geom_hline(yintercept = 50, linetype = "dashed")+
  scale_fill_discrete("Comparaison des genre",
                      labels=c("Esteban", "Gabriel"))+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, hjust = 1),
        plot.margin = margin(rep(15, 4)))


---------------------------------------------------------------

# GRAPHICS NOT USED

# relationship between the number of streams and the amount of time listened for the top 10 genre listened 
data_genre <- data %>% 
  separate_rows(global_genre, sep = ", ") %>% 
  mutate(global_genre = trimws(global_genre)) %>%
  group_by(global_genre) %>% 
  summarise(count = n(), hours = sum(ms_played/3600000)) %>% 
  arrange(desc(count))

ggplot(data_genre, aes(x=count, y=hours, color=global_genre)) + 
    geom_point() +
    labs(title = "Number of streams compare to the amount of time listened", subtitle = "From 2018 to 2023", color = "Top 10 genres") +
    xlab("Number of streams") +
    ylab("Amount of time listened (hours)") +
    theme_light()

# camembert
ggplot(data_genre, aes(x = "", y = count, fill = global_genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Genre Distribution",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# heatmap
data_genre <- data %>% 
  separate_rows(global_genre, sep = ", ") %>% 
  mutate(global_genre = trimws(global_genre)) %>%
  group_by(global_genre) %>% 
  summarise(count = n(), hours = sum(ms_played/3600000)) %>% 
  arrange(desc(count))

data_genre <- data_genre %>%
  group_by(month) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

ggplot(data_genre, aes(x = month, y = global_genre, fill = percent)) +
  geom_tile() +
  scale_fill_gradientn(colors = rev(col_pal), trans='reverse') +
  labs(title = "Heatmap de la popularité des genres au fil du temps",
       x = "Année",
       y = "Genre") +
  theme_minimal()