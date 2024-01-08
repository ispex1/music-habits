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

# I want to make a research on the "genre" listened if I am at home or not (When I am at home, the Platform is Home Speaker)
# I will create a new column with the information if I am at home or not
data$home <- ifelse(data$platform == "Home Speaker", "Home", "Not Home")

# Plot the number of songs played by genre and if I am at home or not
ggplot(data, aes(x=home)) + geom_bar() + ggtitle("Number of songs played by genre and if I am at home or not")


---------------------------------------------------------------

# STRIPCHART

# Define the color palette with 9 different colors from orange to dark red
col_pal <- colorRampPalette(c("#ffa600", "#FF5733", "#C70039", "#900C3F", "#520404"))(9)

#Stream hours by week
#here it is more pertinent to plot by the amount of time listened than by the nb of stream
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

# Return the top 10 artists listened and print it
top10artists <- data %>% group_by(master_metadata_album_artist_name) %>% summarise(count=n()) %>% arrange(desc(count)) %>% head(10) %>% ungroup()*
#top10_number_time <- data %>% group_by(master_metadata_album_artist_name) %>% summarise(count=n(), hours=sum(ms_played/3600000)) %>% arrange(desc(count)) %>% head(10) %>% ungroup()

# Group_by every songs by month/year and add the ms_played each time its grouped 
evolution <- data %>% group_by(master_metadata_album_artist_name, month =as.Date(floor_date(ts,"month"))) %>% summarise(hours=sum(ms_played/3600000))

# Plot the evolution of the top 10 artists listened
ggplot(evolution, aes(x=month, y=hours, color=master_metadata_album_artist_name)) + 
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "% b %Y") +
    gghighlight(master_metadata_album_artist_name %in% top10artists$master_metadata_album_artist_name, label_params = list(size = 10)) +
    labs(title = "Evolution of the top 10 artists listened", subtitle = "From 2018 to 2023", color = "Top 10 artists") +
    xlab("")+
    theme_light()

---------------------------------------------------------------

# REPARTITION FUNCTION

# Plot the curve 
# need to pimp it
ggplot(data_artists, aes(x = index, y = cumulative_percentage)) +
  geom_line() + 
  labs(title = "Repartition function", subtitle = "Between the rank and the total percentage", x = "Rank", y = "Cumulative percentage")

---------------------------------------------------------------

# Compare the relationship between the number of streams and the amount of time listened for the top 10 artists listened 

# Return the top 10 artists listened and print it
top10_number_time <- data %>% group_by(master_metadata_album_artist_name) %>% summarise(count=n(), hours=sum(ms_played/3600000)) %>% arrange(desc(count)) %>% head(10) %>% ungroup()

# Plot the number of streams compare to the amount of time listened for the top 10 artists listened
ggplot(top10artists, aes(x=count, y=hours, color=master_metadata_album_artist_name)) + 
    geom_point() +
    labs(title = "Number of streams compare to the amount of time listened", subtitle = "From 2018 to 2023", color = "Top 10 artists") +
    xlab("Number of streams") +
    ylab("Amount of time listened (hours)") +
    theme_light()