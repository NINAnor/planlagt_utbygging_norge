# --------------------------------------------------------------------------- #
# Name: alder_planlagte_utbyggingsomraader_na_values.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Missing values and old plans, alder, planlagte utbyggingsområder

# Planlagt utbygging Norge
library(tidyverse)
library(summarytools)
library(sf)
Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen = 999)

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", 
                 locale = locale(encoding = "UTF-8")) |> as_tibble()

nrow(data)
head(data)
glimpse(data)

# Assuming data$ikrafttredelsesdato is a numeric variable, convert it to character
data$ikrafttredelsesdato <- as.character(data$ikrafttredelsesdato)

# Convert the character variable to Date format (assuming it's in the format: %Y%m%d)
data$ikrafttredelsesdato <- as.Date(data$ikrafttredelsesdato, format = "%Y%m%d")

# Assuming data is your tibble containing the column "ikrafttredelsesdato"
# First, filter out observations older than 1900 or with missing "ikrafttredelsesdato"
# filtered_data <- data %>%
#   filter(year(ikrafttredelsesdato) >= 1929)

old_data <- data %>%
  filter(year(ikrafttredelsesdato) <= 1929)

nrow(old_data)
nrow(old_data)+ nrow(filtered_data)
nrow(data)

old_current_plans <- old_data %>%
  filter(ikrafttredelsesdato > as.Date("1850-01-01") & ikrafttredelsesdato < as.Date("1929-12-31"))

old_current_plans <- old_data %>%
  filter(ikrafttredelsesdato > as.Date("1900-01-01") & ikrafttredelsesdato < as.Date("1929-12-31"))

# Convert the coordinates to a simple features dataset
# EPSG:25833 - ETRS89 / UTM zone 33N is the coordinate reference system (CRS)
old_current_plans_sf <- st_as_sf(old_current_plans, coords = c("center_x", "center_y"), crs = 25833)
plot(old_current_plans_sf$geometry)

summary(data$ikrafttredelsesdato)

# Next, create a new variable to group the data by decades
filtered_data <- filtered_data %>%
  mutate(decade = (year(ikrafttredelsesdato) %/% 10) * 10)

filtered_data
glimpse(filtered_data)

nrow(data)
sum_planned_development <- sum(data$planlagt_utbygd_areal_m2)
sum_planned_development
sum_planned_development/7140 #convert to soccer fields

summary(data$planlagt_utbygd_areal_m2)

sum_planned_development_km2 <- sum_planned_development/1000000
cat("Total area, planned development in km2:", round(sum_planned_development_km2,0), "\n")

# Aggregate to the individual plan level, based on planid  ----------------

# Count the number of unique planid values
num_unique_planids <- filtered_data %>% 
  distinct(planid) %>% 
  nrow()

# Print the results
cat("Number of unique planids:", num_unique_planids, "\n")

# Count the number of unique planid values
num_unique_planids <- old_data %>% 
  distinct(planid) %>% 
  nrow()

# Print the results
cat("Number of plans with missing or old ikrafttredelsesdato:", num_unique_planids, "\n")

# Print the results
cat("Number of planned development areas with missing or old ikrafttredelsesdato:", nrow(old_data))

summarytools::view(dfSummary(data))
summarytools::view(dfSummary(filtered_data))

library(dplyr)
library(lubridate)

# Filter the data to include only observations with ikrafttredelsesdato <= 1929
old_data <- data %>%
  filter(year(ikrafttredelsesdato) <= 1929)

# Count the number of unique planid in the filtered dataset
unique_planid_count <- old_data %>%
  summarise(n_distinct(planid))

# Calculate the percentage of unique observations (row numbers) that are so old
old_percentage <- (nrow(old_data) / nrow(data)) * 100

unique_planid_count
old_percentage

# Count the number of unique planid in the filtered dataset
unique_planid_count_data <- data %>%
  summarise(n_distinct(planid))

unique_planid_count_data
502/20394*100

old_data$ikrafttredelsesdato
summary(old_data$ikrafttredelsesdato)

head(old_data)
glimpse(old_data)

old_current_plans <- old_data |> filter(ikrafttredelsesdato >= 1900)
library(dplyr)

old_current_plans <- old_data %>%
  filter(ikrafttredelsesdato > as.Date("1900-01-01") & ikrafttredelsesdato < as.Date("1929-12-31"))

# You can now explore the filtered data
head(filtered_data)

# Load required libraries
library(sf)
library(rgdal)

# Assuming filtered_data is already loaded
# Rename the subset from filtered data to old_current_data
old_current_data <- filtered_data

# Convert the coordinates to a simple features dataset
# EPSG:25833 - ETRS89 / UTM zone 33N is the coordinate reference system (CRS)
old_current_data_sf <- st_as_sf(old_current_data, coords = c("center_x", "center_y"), crs = 25833)

old_current_data_sf <-old_current_data_sf |> select()
colnames(old_current_data_sf)

# Export the simple features object as a shapefile
# Replace the path with your desired location
getwd()
st_write(old_current_data_sf, "gamle_planer_qgis/old_current_data.shp")
plot(old_current_data_sf$geometry)

# Load required libraries
library(sf)

# Assuming old_current_data is already a data frame
# Rename the subset from filtered data to old_current_data

# Convert the coordinates to a simple features dataset
# EPSG:25833 - ETRS89 / UTM zone 33N is the coordinate reference system (CRS)
old_current_data_sf <- st_as_sf(old_current_data, coords = c("center_x", "center_y"), crs = 25833)

# Abbreviate field names if necessary (shapefiles have a 10-character limit for field names)
names(old_current_data_sf) <- sapply(names(old_current_data_sf), function(x) substr(x, 1, 10))

# Define the desired path
output_path <- "gamle_planer_qgis/old_current_data.shp"

# Check if the directory exists; if not, create it
output_dir <- dirname(output_path)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

st_write(old_current_data_sf, "gamle_planer_qgis/old_current_data2.shp")
plot(old_current_data_sf$geometry)

plot(old_current_plans$ikrafttredelsesdato, old_current_plans$planlagt_utbygd_areal_m2)


