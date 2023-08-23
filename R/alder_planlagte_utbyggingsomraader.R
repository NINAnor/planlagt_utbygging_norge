# --------------------------------------------------------------------------- #
# Name: alder_planlagte_utbyggingsomraader.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Planlagt utbygging Norge
library(tidyverse)

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
data <- read.csv("Planlagt-Utbygging2023_0000_Norge_25833_XY.csv") |> as_tibble()
head(data)

# Assuming data$ikrafttredelsesdato is a numeric variable, convert it to character
data$ikrafttredelsesdato <- as.character(data$ikrafttredelsesdato)

# Convert the character variable to Date format (assuming it's in the format: %Y%m%d)
data$ikrafttredelsesdato <- as.Date(data$ikrafttredelsesdato, format = "%Y%m%d")

# Assuming data is your tibble containing the column "ikrafttredelsesdato"
# First, filter out observations older than 1900 or with missing "ikrafttredelsesdato"
filtered_data <- data %>%
  filter(!is.na(ikrafttredelsesdato) & year(ikrafttredelsesdato) >= 1900)

# Next, create a new variable to group the data by decades
filtered_data <- filtered_data %>%
  mutate(decade = (year(ikrafttredelsesdato) %/% 10) * 10)

# Then, summarize the data to count the number of observations for each decade
summary_data <- filtered_data %>%
  group_by(decade) %>%
  summarize(observations = n())

# Then, summarize the data to count the number of observations for each decade
summary_data <- filtered_data %>%
  group_by(decade) %>%
  summarize(observations = n())

# Finally, create a bar plot using ggplot2
ggplot(summary_data, aes(x = as.factor(decade), y = observations)) +
  geom_bar(stat = "identity", fill = "steelblue") +
	  geom_text(aes(label = observations), vjust = -0.5, size = 3) +  # Add text labels on top of bars
  labs(x = "Decade", y = "Number of Observations", title = "Observations by Decade") +
  theme_minimal()

# Filter out observations between 1900 and 1970 (inclusive)
filtered_data_1900_1970 <- data %>%
  filter(!is.na(ikrafttredelsesdato) & year(ikrafttredelsesdato) >= 1900 & year(ikrafttredelsesdato) <= 1970)

# Create a new variable to group the data by decades
filtered_data_1900_1970 <- filtered_data_1900_1970 %>%
  mutate(decade = (year(ikrafttredelsesdato) %/% 10) * 10)

# Summarize the data to count the number of observations for each decade
summary_data_1900_1970 <- filtered_data_1900_1970 %>%
  group_by(decade) %>%
  summarize(observations = n())

# Create a bar plot using ggplot2
ggplot(summary_data_1900_1970, aes(x = as.factor(decade), y = observations)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = observations), vjust = -0.5, size = 3) +  # Add text labels on top of bars
  labs(x = "Decade", y = "Number of Observations", title = "Observations from 1900 to 1970") +
  theme_minimal()

