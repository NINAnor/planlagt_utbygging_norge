# --------------------------------------------------------------------------- #
# Name: arealtype_pr_kommune.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

library(tidyverse)
library(readr)

# Set the correct locale
Sys.setlocale("LC_CTYPE", "norwegian")

# Load the data
#setwd("~/GIS_Trond/planlagt_utbygging")
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", 
                 locale = locale(encoding = "UTF-8")) |> as_tibble()

# Define the color palette for "arealformalsgruppe"
arealformalsgruppe_colors <- c(
  "11 Samferdselsanlegg" = "#CCCCCC",
  "05 Turistformål" = "#9999CC",
  "07 Råstoffutvinning" = "#CC9999",
  "02 Fritidsbebyggelse" = "#FFCC66",
  "06 Næringsvirksomhet" = "#FF1300",  
  "10 Andre formål" = "#CCCCCC",      
  "01 Bolig eller sentrumsformål" = "#FFFF99",
  "08 Kombinerte formål" = "red",
  "09 Idrettsanlegg" = "#99CC00",
  "16 Havner og småbåthavner" = "#99CCCC",
  "04 Handel" = "#CCCCFF",
  "03 Tjenesteyting" = "#FF9999",
  "13 Forsvaret" = "#CCCC99"
)

# Get the 10 kommune with the largest total planlagt_areal_m2
top_kommune <- data %>%
  group_by(kommune) %>%
  summarise(total_area_m2 = sum(planlagt_areal_m2)) %>%
  arrange(desc(total_area_m2)) %>%
  head(10) %>%
  pull(kommune)

# Filter the data to include only the top 10 kommune
data_top_kommune <- data %>%
  filter(kommune %in% top_kommune) 

# Summarize the data by kommune and arealformalsgruppe, and convert to square kilometers
summary_data <- data_top_kommune %>%
  group_by(kommune, arealformalsgruppe) %>%
  summarise(total_area_km2 = sum(planlagt_areal_m2) * 0.001)

# Create the plot
ggplot(summary_data, aes(x = reorder(kommune, -total_area_km2), y = total_area_km2, fill = arealformalsgruppe)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = arealformalsgruppe_colors) +
  labs(x = "Kommune", y = "Total Area (sq. km)", title = "Total Planned Area by Kommune and Arealformalsgruppe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
