# --------------------------------------------------------------------------- #
# Name: arealtype_pr_fylke.R
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


# Replace missing values with 0 for the relevant columns
data_cleaned <- data %>%
  mutate_at(vars(planlagt_utbygd_myr_m2, planlagt_utbygd_skog_m2, planlagt_utbygd_jordbruk_m2,
                 planlagt_utbygd_aapen_fastmark_m2, planlagt_utbygd_annen_arealtype_m2),
            ~ replace_na(., 0))

# Summarize the data by the specified variables
summary_data_vars <- data_cleaned %>%
  group_by(fylke) %>%
  summarize(
    total_myr_km2 = sum(planlagt_utbygd_myr_m2) * 0.000001,
    total_skog_km2 = sum(planlagt_utbygd_skog_m2) * 0.000001,
    total_jordbruk_km2 = sum(planlagt_utbygd_jordbruk_m2) * 0.000001,
    total_aapen_fastmark_km2 = sum(planlagt_utbygd_aapen_fastmark_m2) * 0.000001,
    total_annen_arealtype_km2 = sum(planlagt_utbygd_annen_arealtype_m2) * 0.000001
  )

# Print the summarized data
print(summary_data_vars)

library(ggplot2)

# Assuming your data is named summary_data_vars
long_data <- summary_data_vars %>% 
  pivot_longer(
    cols = -fylke, 
    names_to = "variable", 
    values_to = "value"
  )

# Now we can create the plot
ggplot(long_data, aes(fill=variable, y=value, x=fylke)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab("Total km2") + 
  ggtitle("Area distribution by land type")

# Create a vector of colors for each group
color_mapping <- c(
  "total_myr_km2" = "steelblue",
  "total_skog_km2" = "forestgreen",
  "total_jordbruk_km2" = "orange",
  "total_aapen_fastmark_km2" = "khaki",
  "total_annen_arealtype_km2" = "grey"
)

# Define the labels for the legend
legend_labels <- c(
  "total_myr_km2" = "Myr",
  "total_skog_km2" = "Skog",
  "total_jordbruk_km2" = "Jordbruksareal",
  "total_aapen_fastmark_km2" = "Åpen fastmark",
  "total_annen_arealtype_km2" = "Annen arealtype"
)

# Calculate the total area for each 'fylke'
total_area <- summary_data_vars %>%
  rowwise() %>%
  mutate(total_area = sum(c_across(starts_with("total"))))

# Create a factor with levels ordered by the total area
long_data <- long_data %>%
  mutate(fylke = factor(fylke, levels = total_area %>% arrange((total_area)) %>% pull(fylke)))

long_data

# Now we can create the plot

ggplot(long_data, aes(fill = variable, y = value, x = fylke)) + 
  geom_bar(position = "stack", stat = "identity", color = "grey33") + 
  scale_fill_manual(values = color_mapping, 
                    labels = legend_labels,
                    name = "Arealtype") + # Set the legend title
  xlab("Total km2") +   
  ggtitle("Area distribution by land type")

ggplot(long_data, aes(fill = variable, y = value, x = fylke)) + 
  geom_bar(position = "stack", stat = "identity", color = "grey33") + 
  scale_fill_manual(values = color_mapping, 
                    labels = legend_labels,
                    name = "Arealtype") + # Set the legend title
  xlab("Fylke") +   
  ylab("Totalt areal i kvadratkilometer") +   
  ggtitle("Area distribution by land type")+
  coord_flip()

ggplot(long_data, aes(fill = variable, y = value, x = fylke)) + 
  geom_bar(position = "stack", stat = "identity", color = "grey33") + 
  scale_fill_manual(values = color_mapping, 
                    labels = legend_labels,
                    name = "Arealtype") + # Set the legend title
  xlab("Total km2") +   
  theme_void()+
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) + 
  theme(axis.text.y = element_text(angle = 0, hjust = 1)) + 
  coord_flip() +
  ggtitle("Area distribution by land type")

# Calculate the width in inches (10 cm = 3.937 inches)
width_inches <- 15 / 2.54
height_inches <- 10 / 2.54

# Save the plot as a JPG file with 300 DPI and a width of 10 cm
ggsave("arealtyper_prosentvis.jpg", plot = myplot, device = "jpeg", dpi = 300, width = width_inches, height = height_inches)



