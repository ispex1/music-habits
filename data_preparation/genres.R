load_data <- function(path) {
  #load csv
  data <- read.csv(path)

  # Convert the ts column to a POSIXct object
  data$ts <- as.POSIXct(data$ts, format = "%Y-%m-%d %H:%M:%S")
  
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


data_este <- load_data("./data/esteban/data.csv")
write.csv(data_este, "./data/data_este.csv", row.names = FALSE)

data_gab <- load_data("./data/gabriel/data.csv")
write.csv(data_gab, "./data/data_gab.csv", row.names = FALSE)
