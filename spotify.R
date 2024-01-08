#import library
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(data.tree)
library(gghighlight)
library(stringr)


theme_pal <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )


col_pal <- brewer.pal(9, "YlGn")



load_data <- function(path) {
  #load csv
  data <- read.csv(path)
  
  # define the ts column as a date/time object
  data$ts <- as.POSIXct(data$ts, format="%Y-%m-%d %H:%M:%S")
  
  # create a new column with the hour of the day
  data$hour <- hour(data$ts)
  
  # create a new column with the day of the week
  data$day <- weekdays(data$ts)
  
  ##Adding global genres
  #rap
  data$global_genre[grepl("rap", data$genres, ignore.case = TRUE) 
                         | grepl("hip hop", data$genres, ignore.case = TRUE)
                         | grepl("drill", data$genres, ignore.case = TRUE)] <- "rap"
  #pop
  data$global_genre <- ifelse(grepl("pop", data$genres, ignore.case = TRUE)
                                   & !grepl("pop urbaine", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "pop", sep = ", "), 
                                          "pop"), 
                                   data$global_genre)
  #jazz
  data$global_genre <- ifelse(grepl("jazz", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "jazz", sep = ", "), 
                                          "jazz"), 
                                   data$global_genre)
  #electro
  data$global_genre <- ifelse(grepl("electro", data$genres, ignore.case = TRUE)
                                   | grepl("house", data$genres, ignore.case = TRUE)
                                   | grepl("techno", data$genres, ignore.case = TRUE)
                                   | grepl("edm", data$genres, ignore.case = TRUE)
                                   | grepl("tronic", data$genres, ignore.case = TRUE)
                                   | grepl("french touch", data$genres, ignore.case = TRUE)
                                   | grepl("idm", data$genres, ignore.case = TRUE)
                                   | grepl("lo-fi", data$genres, ignore.case = TRUE)
                                   | grepl("hardstyle", data$genres, ignore.case = TRUE)
                                   | grepl("beat", data$genres, ignore.case = TRUE)
                                   | grepl("chillhop", data$genres, ignore.case = TRUE)
                                   | grepl("gabber", data$genres, ignore.case = TRUE)
                                   | grepl("dnb", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "electronic", sep = ", "), 
                                          "electronic"), 
                                   data$global_genre)
  #reggae
  data$global_genre <- ifelse(grepl("reggae", data$genres, ignore.case = TRUE)
                                   | grepl("dancehall", data$genres, ignore.case = TRUE)
                                   | grepl("rocksteady", data$genres, ignore.case = TRUE)
                                   | grepl("dub", data$genres, ignore.case = TRUE)
                                   | grepl("ska", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "reggae", sep = ", "), 
                                          "reggae"), 
                                   data$global_genre)
  #metal (on en écoute même pas lol)
  data$global_genre <- ifelse(grepl("metal", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "metal", sep = ", "), 
                                          "metal"), 
                                   data$global_genre)
  #rnb
  data$global_genre <- ifelse(grepl("r&b", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "r&b", sep = ", "), 
                                          "r&b"), 
                                   data$global_genre)
  #rock
  data$global_genre <- ifelse(grepl("rock(?!steady)", data$genres, ignore.case = TRUE, perl = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "rock", sep = ", "), 
                                          "rock"), 
                                   data$global_genre)
  #classical
  data$global_genre <- ifelse(grepl("classic", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "classical", sep = ", "), 
                                          "classical"), 
                                   data$global_genre)
  #folk/traditionnal
  data$global_genre <- ifelse(grepl("folk", data$genres, ignore.case = TRUE)
                                   |grepl("chanson", data$genres, ignore.case = TRUE), 
                                   ifelse(!is.na(data$global_genre), 
                                          paste(data$global_genre, "folk", sep = ", "), 
                                          "folk"), 
                                   data$global_genre)
  #other
  data$global_genre <- ifelse(is.na(data$global_genre),
                                   "other",
                                   data$global_genre)
  #other with their own genre (par curiosité)
  #data$global_genre <- ifelse(is.na(data$global_genre),ifelse(!is.na(data$genres), data$genres, data$global_genre),data$global_genre)
  return(data)
}


data_este <- load_data("data/esteban/data.csv")

data_gab <- load_data("data/gabriel/data.csv")



data_este %>%
  filter(ts >= as.Date("2023-01-01")) %>%
  mutate(last_genre = word(global_genre, -1, sep = ", ")) %>%
  group_by(last_genre) %>%
  summarise(count = n()) %>%
  top_n(20, wt = count) %>%
  ggplot(aes(x = reorder(last_genre, -count), y = count, fill = last_genre)) +
  geom_bar(stat = "identity") +
  ggtitle("Top 20 Genres by Frequency (Considering Last Word) Esteban") +
  xlab("Genre") +
  ylab("Count") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



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
  ggplot(aes(x = ts, y = 1)) +
  geom_col(aes(fill = hours_played))+
  scale_y_discrete(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_pal), trans='reverse')+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Testouille",
       caption = "test analysis")+
  theme_pal

weekly_time



#Comparaison genre este VS gab
counts_este <- data_este %>%
  group_by(global_genre) %>%
  summarise(count_df_a = n())


# Création du graphique
ggplot(data_percentages, aes(x = percentage, y = global_genre, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Pourcentage de lignes par global_genre dans les deux datasets") +
  xlab("Pourcentage") +
  ylab("Global Genre") +
  theme_minimal() +
  theme(legend.position = "top")

# save data as a csv file
write.csv(data, "data.csv")