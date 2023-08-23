# --------------------------------------------------------------------------- #
# Name: arealformaalsgruppe_pr_kommune.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Planlagt utbygging Norge
library(tidyverse)
library(readr)
Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen = 999)

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
# Replace 'your_file.csv' with the actual file path and filename
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", locale = locale(encoding = "UTF-8")) |> as_tibble()
head(data)

# Assuming data is your tibble containing the columns "planlagt_areal_m2" and "fylke"
# Summarize the data on planlagt_areal_m2 by fylke and convert to square kilometers
summary_data <- data %>%
  group_by(kommunenummer, kommune) %>%
  summarize(total_area_km2 = sum(planlagt_utbygd_areal_m2) * 0.000001) %>%
  arrange(desc(total_area_km2))  # Sort the bars in decreasing order

# Create a bar plot using ggplot2
ggplot(summary_data, aes(x = reorder(kommune, -total_area_km2), y = total_area_km2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Kommune", y = "Total Area (sq. km)", title = "Totalt planlagt utbygd pr kommune") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


unique(data$arealformalsgruppe)

# Assuming data is your tibble containing the columns "planlagt_areal_m2", "fylke", and "arealformalsgruppe"

# Define the correct colors for each "arealformalsgruppe"
arealformalsgruppe_colors <- c(
  "11 Samferdselsanlegg" = "#999999",
  "05 Turistformål" = "#666699",
  "07 Råstoffutvinning" = "#AC6668",
  "02 Fritidsbebyggelse" = "#FFCC33",
  "06 Næringsvirksomhet" = "#9966CC",  # Corrected color code for "06 Næringsvirksomhet"
  "10 Andre formål" = "#CCFFFF",      # Replace with the appropriate color code
  "01 Bolig eller sentrumsformål" = "#FFFF66",
  "08 Kombinerte formål" = "#996600",
  "09 Idrettsanlegg" = "#669900",
  "16 Havner og småbåthavner" = "#66B1B1",
  "04 Handel" = "#9999FF",
  "03 Tjenesteyting" = "#FF6699",
  "13 Forsvaret" = "#999966"
)


data

# Summarize the data on planlagt_areal_m2 by fylke and arealformalsgruppe and convert to square kilometers
summary_data <- data %>%
  group_by(fylke, arealformalsgruppe) %>%
  summarize(total_area_km2 = sum(planlagt_areal_m2) * 0.000001)

summary_data
glimpse(summary_data)
unique(summary_data$fylke)

# Create a stacked bar plot using ggplot2
ggplot(summary_data, aes(x = reorder(fylke, -total_area_km2), y = total_area_km2, fill = arealformalsgruppe)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = arealformalsgruppe_colors) +  # Set the colors for each "arealformalsgruppe"
  labs(x = "Fylke", y = "Samlet areal i kvadratkilometer", title = "Samlet areal fordelt på fylke og arealformalsgruppe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Specify the desired order of the fylke variable
fylke_order <- c("Agder", "Innlandet", "Møre og romsdal", "Nordland", "Rogaland", "Troms og finnmark", "Trøndelag", "Vestfold og telemark", "Vestland", "Viken")
fylke_order <- c("Innlandet", "Trøndelag", "Viken", "Vestland", "Troms og finnmark", "Nordland", "Agder", "Vestfold og telemark", "Møre og romsdal", "Rogaland")


# Convert the fylke variable to a factor with the specified order
summary_data$fylke <- factor(summary_data$fylke, levels = fylke_order)

# Create the plot
ggplot(summary_data, aes(x = fylke, y = total_area_km2, fill = arealformalsgruppe)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = arealformalsgruppe_colors) +  # Set the colors for each "arealformalsgruppe"
  labs(x = "Fylke", y = "Samlet areal i kvadratkilometer", title = "Samlet areal fordelt på fylke og arealformalsgruppe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Create the plot
my_plot <- ggplot(summary_data, aes(x = fylke, y = total_area_km2, fill = arealformalsgruppe)) +
  geom_bar(stat = "identity", position = "stack", color = "grey33") +  # Add a grey outline to the bars
  scale_fill_manual(values = arealformalsgruppe_colors) +  # Set the colors for each "arealformalsgruppe"
  labs(x = "Fylke", y = "Samlet areal i kvadratkilometer", title = "Samlet areal fordelt på fylke og arealformålsgruppe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

my_plot
# Calculate the width in inches (10 cm = 3.937 inches)
width_inches <- 25 / 2.54
height_inches <- 15 / 2.54

# Save the plot as a JPG file with 300 DPI and a width of 10 cm
ggsave("samlet_areal_fylke_arealformaalsgruppe.jpg", plot = my_plot, device = "jpeg", dpi = 300, width = width_inches, height = height_inches)


# Table -------------------------------------------------------------------

library(tidyverse)

summary_data

# Pivot the data to have a column for each arealformalsgruppe
summary_wide <- summary_data %>%
  pivot_wider(names_from = arealformalsgruppe, values_from = total_area_km2, values_fill = 0)

# Add a column with the row sums
summary_with_rowsum <- summary_wide %>%
  rowwise() %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = TRUE))

# Calculate the column sums
col_sums <- summary_with_rowsum %>%
  ungroup() %>%
  summarise(across(where(is.numeric), sum), .groups = "drop") %>%
  mutate(fylke = "Total") %>%
  select(fylke, everything())

col_sums

# Combine the data with the column sums
summary_with_colsum <- bind_rows(summary_with_rowsum, col_sums)

# Round the numeric columns to one digit after the decimal point
final_summary <- summary_with_colsum %>%
  mutate(across(where(is.numeric), ~ round(.x, 1)))

# Print the final summary
final_summary

# Write to an Excel-compatible CSV file
write_excel_csv(final_summary, "sum_arealformaalsgrupper_fylkesvis.csv")
