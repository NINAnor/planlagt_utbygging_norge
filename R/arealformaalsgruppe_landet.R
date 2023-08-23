# --------------------------------------------------------------------------- #
# Name: arealformaalsgruppe_landet.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Hele landet

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
  group_by(arealformalsgruppe) %>%
  summarize(total_area_km2 = sum(planlagt_utbygd_areal_m2) * 0.000001)

summary_data
glimpse(summary_data)
summary_data$total_area_km2 <- round(summary_data$total_area_km2, 1)

# Sort the data by total_area_km2 in descending order
sorted_data <- summary_data %>% 
  arrange(desc(total_area_km2))

# Calculate the Prosent column
sorted_data <- sorted_data %>% 
  mutate(Prosent = (total_area_km2 / sum(total_area_km2)) * 100)

# Round the Prosent column to one decimal place
sorted_data$Prosent <- round(sorted_data$Prosent, 1)

# Display the updated dataframe
print(sorted_data)


# Write to an Excel-compatible CSV file
write_excel_csv(sorted_data, "plots_and_tables/sum_arealformaalsgrupper_landet.csv")

# 1. Remove the specified rows
filtered_data <- summary_data %>% 
  filter(!arealformalsgruppe %in% c("11 Samferdselsanlegg", "09 Idrettsanlegg", "10 Andre formål"))

# 2. Group other categories into 'Næring'
grouped_data <- filtered_data %>% 
  mutate(arealformalsgruppe = case_when(
    arealformalsgruppe %in% c("02 Fritidsbebyggelse", "01 Bolig eller sentrumsformål") ~ arealformalsgruppe,
    TRUE ~ "Næring"
  ))

grouped_data <- filtered_data

# 3. Sum the areas for the grouped categories
summarized_data <- grouped_data %>% 
  group_by(arealformalsgruppe) %>% 
  summarize(total_area_km2 = sum(total_area_km2))

# 4. Calculate the percentage for each category
summarized_data <- summarized_data %>%
  mutate(Prosent = (total_area_km2 / sum(total_area_km2)) * 100) %>%
  # Round to one decimal place
  mutate(Prosent = round(Prosent, 1))

summarized_data <- summarized_data |> arrange(desc(total_area_km2))

# Display the new summary table
print(summarized_data)

sum(summarized_data$total_area_km2)
sum(summarized_data$Prosent)


# Write to an Excel-compatible CSV file
write_excel_csv(summarized_data, "plots_and_tables/sum_arealformaalsgrupper_bolig_fribol_naring_landet.csv")

# Add a new color column to the data
summary_data <- summary_data %>%
  mutate(color = arealformalsgruppe_colors[arealformalsgruppe])

summary_data

ggplot(summary_data) +
  aes(
    x = arealformalsgruppe,
    y = total_area_km2,
    fill = arealformalsgruppe
  ) +
  geom_col() +
  #scale_fill_hue(direction = 1) +
  scale_fill_manual(values = arealformalsgruppe_colors) +  # Set the colors for each "arealformalsgruppe"
  coord_flip() +
  theme_minimal()

# Reorder the arealformalsgruppe factor levels by total_area_km2 in ascending order
summary_data$arealformalsgruppe <- factor(summary_data$arealformalsgruppe, 
                                          levels = summary_data$arealformalsgruppe[order(summary_data$total_area_km2)])

# Plot the horizontal bar plot
my_plot <- ggplot(summary_data) +
  aes(x = arealformalsgruppe, y = total_area_km2, fill = arealformalsgruppe) +
  geom_col(color = "grey33") +
  geom_text(aes(label = format(round(total_area_km2), big.mark = " ")), hjust = -0.2, size = 3) + # Format with spaces as thousand separators
  scale_fill_manual(values = arealformalsgruppe_colors, guide = FALSE) + # Set the colors and suppress the legend
  coord_flip() +
  labs(x = "Arealformålsgruppe", y = "Totalt areal i kvadratkilometer") + # Add axis titles
  theme_void() +
  theme(axis.text.y = element_text(face = "plain")) +
  ylim(c(0, max(summary_data$total_area_km2) * 1.2)) # Extend the y-axis limit

my_plot

# Calculate the width and height in inches
width_inches <- 25 / 2.54
height_inches <- 15 / 2.54

# Save the plot as a JPG file with 300 DPI and the specified size
ggsave("plots_and_tables/samlet_areal_landet_arealformaalsgruppe.jpg", plot = my_plot, device = "jpeg", dpi = 300, width = width_inches, height = height_inches)

# Control
987+453+367+307+302+260+193+90+76+55+43+8+2

sum(summary_data$total_area_km2)
