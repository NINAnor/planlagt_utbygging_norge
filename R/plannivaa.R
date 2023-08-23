# --------------------------------------------------------------------------- #
# Name: plannivaa.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Planlagt utbygging Norge
library(tidyverse)
library(readr)
Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen=999)

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
# Replace 'your_file.csv' with the actual file path and filename
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", locale = locale(encoding = "UTF-8")) |> as_tibble()
head(data)
glimpse(data)

unique (data$arealformalsgruppe)

data_simple <- data |> 
  filter(arealformalsgruppe != "10 Andre formål" & arealformalsgruppe != "11 Samferdselsanlegg" & 
      arealformalsgruppe != "09 Idrettsanlegg" & arealformalsgruppe != "13 Forsvaret") 

unique(data_simple$arealformalsgruppe)



# Replace missing values with 0 for the relevant columns
data_cleaned <- data_simple %>%
  mutate_at(vars(planlagt_utbygd_myr_m2, planlagt_utbygd_skog_m2, planlagt_utbygd_jordbruk_m2,
                 planlagt_utbygd_aapen_fastmark_m2, planlagt_utbygd_annen_arealtype_m2),
            ~ replace_na(., 0))

# Summarize the data by the specified variables at the regional level
summary_data_vars_fylke <- data_cleaned %>%
  group_by(fylke) %>%
  summarize(
    total_myr_km2 = round(sum(planlagt_utbygd_myr_m2) * 0.000001, 1),
    total_skog_km2 = round(sum(planlagt_utbygd_skog_m2) * 0.000001, 1),
    total_jordbruk_km2 = round(sum(planlagt_utbygd_jordbruk_m2) * 0.000001, 1),
    total_aapen_fastmark_km2 = round(sum(planlagt_utbygd_aapen_fastmark_m2) * 0.000001, 1),
    total_annen_arealtype_km2 = round(sum(planlagt_utbygd_annen_arealtype_m2) * 0.000001, 1)
  )

summary_data_vars_fylke

summary_data_vars_fylke <- summary_data_vars_fylke %>%
  rowwise() %>%
  mutate(
    total_area_km2 = sum(total_myr_km2, total_skog_km2, total_jordbruk_km2, total_aapen_fastmark_km2, total_annen_arealtype_km2),
    percentage_myr = round(total_myr_km2 / total_area_km2 * 100, 1),
    percentage_skog = round(total_skog_km2 / total_area_km2 * 100, 1),
    percentage_jordbruk = round(total_jordbruk_km2 / total_area_km2 * 100, 1),
    percentage_aapen_fastmark = round(total_aapen_fastmark_km2 / total_area_km2 * 100, 1),
    percentage_annen_arealtype = round(total_annen_arealtype_km2 / total_area_km2 * 100, 1)
  ) %>%
  ungroup() %>%
  select(
    fylke,
    total_myr_km2,
    percentage_myr,
    total_skog_km2,
    percentage_skog,
    total_jordbruk_km2,
    percentage_jordbruk,
    total_aapen_fastmark_km2,
    percentage_aapen_fastmark,
    total_annen_arealtype_km2,
    percentage_annen_arealtype
  )


summary_data_vars_fylke

# Write to an Excel-compatible CSV file
write_excel_csv(summary_data_vars_fylke, "plots_and_tables/sum_arealtyper_fylkesvis.csv")

summary_data_vars <- data_cleaned %>%
  group_by() %>%
  summarize(
    total_myr_km2 = round(sum(planlagt_utbygd_myr_m2) * 0.000001, 1),
    total_skog_km2 = round(sum(planlagt_utbygd_skog_m2) * 0.000001, 1),
    total_jordbruk_km2 = round(sum(planlagt_utbygd_jordbruk_m2) * 0.000001, 1),
    total_aapen_fastmark_km2 = round(sum(planlagt_utbygd_aapen_fastmark_m2) * 0.000001, 1),
    total_annen_arealtype_km2 = round(sum(planlagt_utbygd_annen_arealtype_m2) * 0.000001, 1)
  )


# Print the summarized data
summary_data_vars


summary_data_vars <- summary_data_vars %>%
  mutate(
    total_area_km2 = total_myr_km2 + total_skog_km2 + total_jordbruk_km2 + total_aapen_fastmark_km2 + total_annen_arealtype_km2,
    percentage_myr = round(total_myr_km2 / total_area_km2 * 100, 1),
    percentage_skog = round(total_skog_km2 / total_area_km2 * 100, 1),
    percentage_jordbruk = round(total_jordbruk_km2 / total_area_km2 * 100, 1),
    percentage_aapen_fastmark = round(total_aapen_fastmark_km2 / total_area_km2 * 100, 1),
    percentage_annen_arealtype = round(total_annen_arealtype_km2 / total_area_km2 * 100, 1)
  ) %>%
  select(
    total_myr_km2,
    percentage_myr,
    total_skog_km2,
    percentage_skog,
    total_jordbruk_km2,
    percentage_jordbruk,
    total_aapen_fastmark_km2,
    percentage_aapen_fastmark,
    total_annen_arealtype_km2,
    percentage_annen_arealtype
  )

summary_data_vars

# Write to an Excel-compatible CSV file
write_excel_csv(summary_data_vars, "plots_and_tables/sum_arealtyper.csv")


data_long <- summary_data_vars %>%
  pivot_longer(
    cols = everything(),
    names_to = "arealformalstype",
    values_to = "value"
  )

sum(data_long$value)

data_long

# Create a vector of colors for each group
color_mapping <- c(
  "total_myr_km2" = "steelblue",
  "total_skog_km2" = "forestgreen",
  "total_jordbruk_km2" = "orange",
  "total_aapen_fastmark_km2" = "khaki",
  "total_annen_arealtype_km2" = "grey"
)

ggplot(data_long) +
  aes(x = arealformalstype, y = value) +
  geom_col(fill = "#112446") +
  theme_minimal()

# Calculate the total
total_value <- sum(data_long$value)

# Add a percentage column
data_long <- data_long %>%
  mutate(percentage = value / total_value * 100)

data_long

# Plot the data
ggplot(data_long) +
  aes(x = reorder(arealformalstype, -value), y = value, fill = arealformalstype) +
  geom_col() +
  #geom_text(aes(label = paste0(value))), vjust = -0.5) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Arealformålstype",
    y = "Value",
    title = "Distribution of Arealformål"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability


# Define the labels for the legend
legend_labels <- c(
  "total_myr_km2" = "Myr",
  "total_skog_km2" = "Skog",
  "total_jordbruk_km2" = "Jordbruksareal",
  "total_aapen_fastmark_km2" = "Åpen fastmark",
  "total_annen_arealtype_km2" = "Annen arealtype"
)

data_long

# Sort the data by 'value' in descending order
data_long <- data_long %>%
  arrange(desc(value)) %>%
  mutate(arealformalstype = fct_inorder(arealformalstype))


ggplot(data_long) +
  aes(x = reorder(arealformalstype, -value), y = value, fill = arealformalstype) +
  geom_col() +
  #geom_text(aes(label = paste0(value))), vjust = -0.5) +
  scale_fill_manual(values = color_mapping) +
  labs(
    x = "Arealformålstype",
    y = "Value",
    title = "Distribution of Arealformål"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability

data_long
glimpse(data_long)

# Plot the data
myplot <- ggplot(data_long) +
  aes(x = arealformalstype, y = value, fill = arealformalstype) +
  geom_col() +
geom_text(aes(label = paste0(round(value,0))), vjust = -0.5, size = 3)+
  scale_fill_manual(values = color_mapping) +
  scale_x_discrete(labels = legend_labels) + # Customize x-axis labels
  labs(
    x = "Arealtype i planlagt utbyggingsområder for bolig, fritidsbolig og næring",
    y = "Totalt areal i kvadratkilometer"#,
    #title = "Arealtyper i planlagte utbyggingsområder"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
    legend.position = "none" # Suppress the legend
  )


myplot

# myplot <- ggplot(data_long) +
#   aes(x = arealformalstype, y = value, fill = arealformalstype) +
#   geom_col() +
#   geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
#   scale_fill_manual(values = color_mapping) +
#   scale_x_discrete(labels = legend_labels) + # Customize x-axis labels
#   labs(
#     x = "Arealtype i planlagt utbyggingsområde",
#     y = "Totalt areal i kvadratkilometer"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for readability
#     legend.position = "none" # Suppress the legend
#   ) +
#   ylim(NA, 1600) # Adjust the y-axis limits
# 
#   
# myplot

# Calculate the width in inches (10 cm = 3.937 inches)
width_inches <- 15 / 2.54
height_inches <- 10 / 2.54

# Save the plot as a JPG file with 300 DPI and a width of 10 cm
ggsave("plots_and_tables/arealtyper_utbygd.jpg", plot = myplot, device = "jpeg", dpi = 300, width = width_inches, height = height_inches)



