# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(pacman)  # Assuming all necessary packages are already installed

# Set working directory and file path
file_path <- "/Users/caitlin/Desktop/spotify.xlsx"

# Read the Excel file into a data frame
data_set <- read_excel(file_path)

# Ensure columns are treated as factors or characters as needed
data_set$Feature <- as.factor(data_set$Feature)
data_set$Solo <- as.factor(data_set$Solo)
data_set$Streams <- as.factor(data_set$Streams)

# Get the top 10 artists by different metrics
top_artists_by_streams <- data_set %>%
  arrange(desc(Streams)) %>%
  head(10)

top_artists_by_daily <- data_set %>%
  arrange(desc(Daily)) %>%
  head(10)

# Adjust columns for analysis
top_artists_by_streams$Solo <- as.character(top_artists_by_streams$Solo)
top_artists_by_streams$Streams <- as.character(top_artists_by_streams$Streams)
top_artists_by_streams$Feature <- as.character(top_artists_by_streams$Feature)
top_artists_by_streams$Solo[is.na(top_artists_by_streams$Solo) | top_artists_by_streams$Solo == "0"] <- "Missing"
top_artists_by_streams$Streams[is.na(top_artists_by_streams$Streams) | top_artists_by_streams$Streams == "0"] <- "Missing"
top_artists_by_streams$Feature[is.na(top_artists_by_streams$Feature) | top_artists_by_streams$Feature == "0"] <- "Missing"

# Create matrix for barplot
barplot_data <- matrix(
  c(as.numeric(top_artists_by_streams$Solo), as.numeric(top_artists_by_streams$Streams), as.numeric(top_artists_by_streams$Feature)),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(c("Solo", "Streams", "Feature"), top_artists_by_streams$Artist)
)

# Create barplots
barplot(barplot_data, beside = TRUE, col = c("blue", "red", "green"), main = "Top 10 Artists: Solo vs. Streams vs. Feature", xlab = "Artist", ylab = "Streams", legend.text = TRUE)

# GGplot for Streams
ggplot(data = top_artists_by_streams, aes(x = reorder(Artist, Streams), y = Streams)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Artists with the Most Streams", x = "Artist", y = "Total Streams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# GGplot for Daily Streams
ggplot(data = top_artists_by_daily, aes(x = reorder(Artist, Daily), y = Daily)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 10 Artists with the Most Daily Streams", x = "Artist", y = "Daily Streams") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
