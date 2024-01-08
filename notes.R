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
