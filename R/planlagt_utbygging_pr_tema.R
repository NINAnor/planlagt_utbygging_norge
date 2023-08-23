# --------------------------------------------------------------------------- #
# Name: planlagt_utbygging_pr_tema.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

library(tidyverse)
library(readr)
library(scales)

# Set the correct locale
Sys.setlocale("LC_CTYPE", "norwegian")

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
# Replace 'your_file.csv' with the actual file path and filename
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", locale = locale(encoding = "UTF-8")) |> as_tibble()
head(data)
glimpse(data)

unique (data$arealformalsgruppe)

# Excluding arealformålsgrupper with uncertain results

data_simple <- data |> 
  filter(arealformalsgruppe != "10 Andre formål" & arealformalsgruppe != "11 Samferdselsanlegg" & 
      arealformalsgruppe != "09 Idrettsanlegg") 

unique(data_simple$arealformalsgruppe)


print(colnames(data))
cat(colnames(data), '\n')

# # Replace missing values with 0 for the relevant columns
# data_cleaned <- data_simple %>%
#   mutate_at(vars(
#     planlagt_utbygd_myr_m2, 
#     planlagt_utbygd_skog_m2, 
#     planlagt_utbygd_jordbruk_m2,
#     planlagt_utbygd_aapen_fastmark_m2, 
#     planlagt_utbygd_annen_arealtype_m2),
#             ~ replace_na(., 0))

data_cleaned <- data_simple %>%
  mutate_at(vars(
    planlagt_utbygd_myr_m2, 
    planlagt_utbygd_skog_m2, 
    planlagt_utbygd_jordbruk_m2,
    planlagt_utbygd_aapen_fastmark_m2, 
    planlagt_utbygd_annen_arealtype_m2,
    planlagt_utbygd_skredfaresone_m2, 
    planlagt_utbygd_flomsone_m2, 
    planlagt_utbygd_over_skoggrense_m2, 
    planlagt_utbygd_strandsone_m2, 
    planlagt_utbygd_villrein_m2, 
    planlagt_utbygd_iba_m2),
            ~ replace_na(., 0))

data_cleaned


# Summarize the data by the specified variables at the regional level
summary_data_vars_fylke <- data_cleaned %>%
  group_by(fylke) %>%
  summarize(
    total_myr_km2 = round(sum(planlagt_utbygd_myr_m2) * 0.000001, 1),
    total_skog_km2 = round(sum(planlagt_utbygd_skog_m2) * 0.000001, 1),
    total_jordbruk_km2 = round(sum(planlagt_utbygd_jordbruk_m2) * 0.000001, 1),
    total_aapen_fastmark_km2 = round(sum(planlagt_utbygd_aapen_fastmark_m2) * 0.000001, 1),
    total_annen_arealtype_km2 = round(sum(planlagt_utbygd_annen_arealtype_m2) * 0.000001, 1),
    total_skredfaresone_km2 = round(sum(planlagt_utbygd_skredfaresone_m2) * 0.000001, 1),
    total_flomsone_km2 = round(sum(planlagt_utbygd_flomsone_m2) * 0.000001, 1),
    total_over_skoggrense_km2 = round(sum(planlagt_utbygd_over_skoggrense_m2) * 0.000001, 1),
    total_strandsone_km2 = round(sum(planlagt_utbygd_strandsone_m2) * 0.000001, 1),
    total_villrein_km2 = round(sum(planlagt_utbygd_villrein_m2) * 0.000001, 1),
    total_iba_km2= round(sum(planlagt_utbygd_iba_m2) * 0.000001, 1),
  )

summary_data_vars_fylke

# Write to an Excel-compatible CSV file
write_excel_csv(summary_data_vars_fylke, "plots_and_tables/sum_arealtyper_og_milovariabler_fylkesvis.csv")

env_var_fylke <- summary_data_vars_fylke |> select(-total_myr_km2, -total_skog_km2, -total_jordbruk_km2, -total_aapen_fastmark_km2, - total_annen_arealtype_km2)
env_var_fylke

# Summarize the data by the specified variables at the regional level
summary_data_vars_landet <- data_cleaned %>%
  group_by() %>%
  summarize(
    total_myr_km2 = round(sum(planlagt_utbygd_myr_m2) * 0.000001, 1),
    total_skog_km2 = round(sum(planlagt_utbygd_skog_m2) * 0.000001, 1),
    total_jordbruk_km2 = round(sum(planlagt_utbygd_jordbruk_m2) * 0.000001, 1),
    total_aapen_fastmark_km2 = round(sum(planlagt_utbygd_aapen_fastmark_m2) * 0.000001, 1),
    total_annen_arealtype_km2 = round(sum(planlagt_utbygd_annen_arealtype_m2) * 0.000001, 1),
    total_skredfaresone_km2 = round(sum(planlagt_utbygd_skredfaresone_m2) * 0.000001, 1),
    total_flomsone_km2 = round(sum(planlagt_utbygd_flomsone_m2) * 0.000001, 1),
    total_over_skoggrense_km2 = round(sum(planlagt_utbygd_over_skoggrense_m2) * 0.000001, 1),
    total_strandsone_km2 = round(sum(planlagt_utbygd_strandsone_m2) * 0.000001, 1),
    total_villrein_km2 = round(sum(planlagt_utbygd_villrein_m2) * 0.000001, 1),
    total_iba_km2= round(sum(planlagt_utbygd_iba_m2) * 0.000001, 1),
  )

summary_data_vars_landet
# Write to an Excel-compatible CSV file
write_excel_csv(summary_data_vars_landet, "plots_and_tables/sum_arealtyper_og_milovariabler_landet.csv")

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


# # Fylkesvis plots ---------------------------------------------------------
# 
# 
# # Assuming summary_data_vars_fylke is your data
# summary_data_vars_fylke <- tibble(
#   fylke = c("Agder", "Innlandet", "Møre og romsdal", "Nordland", "Rogaland", "Troms og finnmark", "Trøndelag", "Vestfold og telemark", "Vestland", "Viken"),
#   total_myr_km2 = c(16.6, 24.1, 21.2, 16, 4.3, 10.8, 42.1, 6.3, 11.9, 10.8)
#   # include other variables here
# )
# 
# # Order the data by total_myr_km2, descending
# ordered_data <- summary_data_vars_fylke %>%
#   arrange(desc(total_myr_km2))
# 
# # Plot
# ggplot(ordered_data, aes(x = reorder(fylke, total_myr_km2), y = total_myr_km2)) +
#   geom_bar(stat = "identity", fill = "#0072B2") +
#   coord_flip() +
#   xlab("Fylke") +
#   ylab("Total Myr (km^2)") +
#   ggtitle("Total Myr Area by Fylke") +
#   theme_minimal()
# 
# # Order the data by total_myr_km2, descending
# ordered_data <- summary_data_vars_fylke %>%
#   arrange(desc(total_myr_km2))
# 
# # Plot
# ggplot(ordered_data, aes(x = reorder(fylke, -total_myr_km2), y = total_myr_km2)) +
#   geom_bar(stat = "identity", fill = "#0072B2") +
#   geom_text(aes(label = format(total_myr_km2, big.mark = "", decimal.mark = ",")), vjust = -0.5) +
#   xlab("Fylke") +
#   ylab("Total Myr (km^2)") +
#   ggtitle("Total Myr Area by Fylke") +
#   ylim(0, max(ordered_data$total_myr_km2) + 5) + # Adjust the y limit to make room for the numbers
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees
# 


# faceted plots -----------------------------------------------------------

fylkesdata <- summary_data_vars_fylke |> select(fylke, 
  total_myr_km2, 
  total_skog_km2, 
  total_jordbruk_km2, 
  total_skredfaresone_km2, 
  total_flomsone_km2, 
  total_over_skoggrense_km2, 
  total_strandsone_km2, 
  total_villrein_km2, 
  total_iba_km2)

colnames(summary_data_vars_fylke)

fylkesdata


c("Innlandet", "Viken", "Trøndelag", "Agder", "Vestfold og Telemark", "Vestland", "Nordland", "Troms og Finnmark", "Møre og Romsdal", "Rogaland")

fylkesdata

# Convert the data to a long format and create an ordering for each variable
long_data <- fylkesdata %>%
  pivot_longer(cols = -fylke,
               names_to = "variable",
               values_to = "value")

# Create a combined factor for ordering
long_data <- long_data %>%
  group_by(variable) %>%
  arrange(desc(value)) %>%
  mutate(fylke_variable = factor(paste(fylke, variable), levels = paste(fylke, variable)))

# Custom titles for each variable
titles <- c(
  total_myr_km2 = "Myr",
  total_skog_km2 = "Skog",
  total_jordbruk_km2 = "Jordbruksareal",
  total_skredfaresone_km2 = "Skredfaresone",
  total_flomsone_km2 = "Flomsone",
  total_over_skoggrense_km2 = "Over skoggrensen",
  total_strandsone_km2 = "Strandsone",
  total_villrein_km2 = "Villreinens leveområder",
  total_iba_km2 = "Fulgl, IBA"
)

long_data
glimpse(long_data)
titles

# Calculate the order of variables based on max value for each
ordering <- long_data %>%
  group_by(variable) %>%
  summarise(max_value = max(value)) %>%
  arrange(-max_value) %>%
  pull(variable)


long_data$variable <- factor(long_data$variable, levels = ordering)


# Create the faceted plot
p <- ggplot(long_data, aes(x = fylke_variable, y = value)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  geom_text(aes(label = format(value, big.mark = "", decimal.mark = ",")), vjust = -0.5, size = 2.5) +
  xlab("Fylke") +
  ylab("Areal i kvadratkilometer") +
  ggtitle("Planlagt utbyggingsareal for bolig, fritidsbolig og næringsformål pr. tema") +
  facet_wrap(~ variable, scales = "free", ncol = 3, labeller = labeller(variable = titles)) +
  scale_x_discrete(labels = function(x) {str_extract(x, "^\\S+")}) + # Extract the fylke names from the combined factor
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

# Print the plot
p

# Calculate the width and height in inches
width_in_inches <- 21 / 2.54
height_in_inches <- 28 / 2.54

# Save as before
ggsave("plots_and_tables/miljo_sammfunn_pr_fylke.jpg", plot = p, device = "jpeg", width = width_in_inches, height = height_in_inches, dpi = 300)




# Kommunevis --------------------------------------------------------------



# Exclude NA values when calculating the sum
top_myr_kommune <- summary_data_vars_fylke %>%
  group_by(fylke) %>%
  summarise(total_myr_m2 = sum(planlagt_utbygd_myr_m2, na.rm = TRUE)) %>%
  arrange(desc(total_myr_m2))


top_myr_kommune

# Select the top 10 kommune for planlagt_utbygd_myr_m2
top_myr_kommune <- top_myr_kommune |> head(10)

# Create a bar plot
ggplot(top_myr_kommune, aes(x = reorder(kommune, -total_myr_m2), y = total_myr_m2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = comma(total_myr_m2)), vjust = -0.5) +  # Annotate bars with total area
  labs(x = "Kommune", y = "Total Myr Area (m²)", title = "Top 10 Kommuner with Largest Planned Development on Myr") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

colnames(data)


# Function for all environmental variables --------------------------------


# Function to calculate top kommune by column
calculate_top_kommune <- function(data, column_name, title_suffix) {
  column_total <- paste0("total_", column_name)
  
  # Exclude NA values when calculating the sum
  top_kommune <- data_cleaned %>%
    group_by(kommune) %>%
    summarise(!!column_total := sum(!!sym(column_name), na.rm = TRUE)) %>%
    arrange(desc(!!sym(column_total)))
  
  # Select the top 10 kommune
  top_kommune <- top_kommune |> head(10)
  
  # Create a bar plot
  ggplot(top_kommune, aes(x = reorder(kommune, -!!sym(column_total)), y = !!sym(column_total))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = comma(!!sym(column_total))), vjust = -0.5) +  # Annotate bars with total area
    labs(x = "Kommune", y = paste0("Total ", title_suffix), 
         title = paste0("Top 10 Kommuner with Largest Planned Development on ", title_suffix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
}

# Apply the function to each of your columns
column_names <- c("planlagt_utbygd_jordbruk_m2", 
                  "planlagt_utbygd_myr_m2",  
                  "planlagt_utbygd_skog_m2",
                  "planlagt_utbygd_aapen_fastmark_m2",
                  "planlagt_utbygd_annen_arealtype_m2",
                  "planlagt_utbygd_skredfaresone_m2",   
                  "planlagt_utbygd_flomsone_m2",       
                  "planlagt_utbygd_over_skoggrense_m2", 
                  "planlagt_utbygd_strandsone_m2", 
                  "planlagt_utbygd_villrein_m2",
                  "planlagt_utbygd_iba_m2")

title_suffixes <- c("Jordbruk (m²)", 
                    "Myr (m²)",  
                    "Skog (m²)",
                    "Åpen Fastmark (m²)",
                    "Annen Arealtype (m²)",
                    "Skredfaresone (m²)",   
                    "Flomsone (m²)",       
                    "Over Skoggrense (m²)", 
                    "Strandsone (m²)", 
                    "Villrein (m²)",
                    "Iba (m²)")

# Loop through each column
for (i in seq_along(column_names)) {
  print(calculate_top_kommune(data, column_names[i], title_suffixes[i]))
}

su <- 164.1 +	1363.7 +139.5 +	353.7+139.6 
164.1/su*100
1363.7/su*100
139.5 /su*100
353.7/su*100
139.6 /su*100



# Square kilometers -------------------------------------------------------

data

# Function to calculate top kommune by column
calculate_top_kommune <- function(data, column_name, title_suffix) {
  column_total <- paste0("total_", column_name)
  
  # Exclude NA values when calculating the sum and convert to square kilometers
  top_kommune <- data %>%
    group_by(kommune) %>%
    summarise(!!column_total := round(sum(!!sym(column_name), na.rm = TRUE) / 1000000, 1)) %>%
    arrange(desc(!!sym(column_total)))
  
  # Select the top 10 kommune
  top_kommune <- top_kommune |> head(10)
  
  # Create a bar plot
  ggplot(top_kommune, aes(x = reorder(kommune, -!!sym(column_total)), y = !!sym(column_total))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = format(!!sym(column_total), big.mark = ",", decimal.mark = ".", nsmall = 1)), vjust = -0.5) +  # Annotate bars with total area
    labs(x = "Kommune", y = paste0("Totalt areal ", title_suffix), 
         title = paste0("Topp 10 kommuner med størst areal planlagt utbygging i areal med ", title_suffix)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
}

# Apply the function to each of your columns
column_names <- c("planlagt_utbygd_jordbruk_m2", 
                  "planlagt_utbygd_myr_m2",  
                  "planlagt_utbygd_skog_m2",
                  "planlagt_utbygd_aapen_fastmark_m2",
                  "planlagt_utbygd_annen_arealtype_m2",
                  "planlagt_utbygd_skredfaresone_m2",   
                  "planlagt_utbygd_flomsone_m2",       
                  "planlagt_utbygd_over_skoggrense_m2", 
                  "planlagt_utbygd_strandsone_m2", 
                  "planlagt_utbygd_villrein_m2",
                  "planlagt_utbygd_iba_m2")

title_suffixes <- c("jordbruk (km²)", 
                    "myr (km²)",  
                    "skog (km²)",
                    "åpen fastmark (km²)",
                    "annen arealtype (km²)",
                    "skredfaresone (km²)",   
                    "flomsone (km²)",       
                    "over skoggrense (km²)", 
                    "strandsone (km²)", 
                    "villrein (km²)",
                    "iba (km²)")

# Loop through each column
for (i in seq_along(column_names)) {
  print(calculate_top_kommune(data, column_names[i], title_suffixes[i]))
}

# Loop through each column
for (i in seq_along(column_names)) {
  plot <- calculate_top_kommune(data, column_names[i], title_suffixes[i])
  print(plot) # This will still print the plot to your R console

  # Define the file name based on the current column name
  file_name <- paste0("plot_", column_names[i], ".jpg")
  
  # Save the plot as a JPG file with your desired width, height, and DPI
  ggsave(file_name, plot = plot, device = "jpeg", dpi = 300, width = 10, height = 6)
}

