# --------------------------------------------------------------------------- #
# Name: plannivå_planlagte_utbyggingsomraader_detaljert.R
# Date: 2023-08-23
# Author: Trond Simensen
# --------------------------------------------------------------------------- #

# Alder, planlagte utbyggingsområder

# Planlagt utbygging Norge
library(tidyverse)
Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen = 999)

# Les inn csv-fil ---------------------------------------------------------
#setwd("~/GIS_Trond/planlagt_utbygging")
data <- read_csv("data/Planlagt-Utbygging2023_0000_Norge_25833_XY.csv", 
                 locale = locale(encoding = "UTF-8")) |> as_tibble()

nrow(data)

data <- data |> 
  filter(arealformalsgruppe != "10 Andre formål" & arealformalsgruppe != "11 Samferdselsanlegg" & 
      arealformalsgruppe != "09 Idrettsanlegg") 

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

filtered_data
glimpse(filtered_data)

nrow(data)
sum_planned_development <- sum(data$planlagt_utbygd_areal_m2)
sum_planned_development
sum_planned_development/7140 #convert to soccer fields

summary(data$planlagt_utbygd_areal_m2)

sum_planned_development_km2 <- sum_planned_development/1000000
cat("Total area, planned development in km2:", round(sum_planned_development_km2,0), "\n")

# Create a new dataset with aggregated observations
aggregated_data <- filtered_data %>%
  group_by(plantype) %>%
  summarise(plankilde = first(plankilde),
            planlagt_utbygd_areal = sum(planlagt_utbygd_areal_m2))

# Print the first few rows of the aggregated dataset
head(aggregated_data)
glimpse(aggregated_data)

aggregated_data <- aggregated_data |> mutate(planlagt_utbygd_km2 =planlagt_utbygd_areal/1000000)
aggregated_data$planlagt_utbygd_km2 <- round(aggregated_data$planlagt_utbygd_km2,1)

# Create the new column
aggregated_data <- aggregated_data %>%
  mutate(
    plan_type = case_when(
      plantype == 20 ~ "20 Kommuneplanens arealdel",
      plantype == 21 ~ "21 Kommunedelplan",
      plantype == 22 ~ "22 Mindre endring av kommuneplan",
      plantype == 30 ~ "30 Eldre reguleringsplan",
      plantype == 31 ~ "31 Mindre reguleringsendring",
      plantype == 32 ~ "32 Bebyggelsesplan iht. reguleringsplan",
      plantype == 33 ~ "33 Bebyggelsesplan iht. kommuneplanens arealdel",
      plantype == 34 ~ "34 Områderegulering",
      plantype == 35 ~ "35 Detaljregulering",
      TRUE ~ as.character(plantype) # default case if not one of the specified values
    )
  )

# View the updated data frame
head(aggregated_data)

aggregated_data <- aggregated_data |> select(plan_type, planlagt_utbygd_km2)
head(aggregated_data)
glimpse(aggregated_data)

# Create the new column
aggregated_data <- aggregated_data %>%
  mutate(
    Plannivaa = case_when(
      plan_type %in% c("20 Kommuneplanens arealdel", "21 Kommunedelplan", "22 Mindre endring av kommuneplan") ~ "Oversiktsplan",
      TRUE ~ "Detaljplan"
    )
  )

# View the updated data frame
head(aggregated_data)
glimpse(aggregated_data)


# Create the stacked bar plot
ggplot(aggregated_data, aes(x = Plannivaa, y = planlagt_utbygd_km2, fill = plan_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Stacked Barplot for Plannivaa",
    y = "Planlagt Utbygd (km^2)",
    x = "Plannivaa Type"
  ) +
  theme_minimal()

# Create the grouped bar plot
ggplot(aggregated_data, aes(x = Plannivaa, y = planlagt_utbygd_km2, fill = plan_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Grouped Barplot for Plannivaa",
    y = "Planlagt Utbygd (km^2)",
    x = "Plannivaa Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Load necessary packages
library(ggplot2)
library(dplyr)

# Reorder data for plotting
aggregated_data <- aggregated_data %>%
  arrange(Plannivaa, -desc(planlagt_utbygd_km2)) %>%
  mutate(plan_type = factor(plan_type, levels = unique(plan_type))) %>%
  mutate(Plannivaa = factor(Plannivaa, levels = c("Oversiktsplan", "Detaljplan")))

# Create the grouped bar plot
p <- ggplot(aggregated_data, aes(x = plan_type, y = planlagt_utbygd_km2, fill = Plannivaa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = format(planlagt_utbygd_km2, big.mark = ",", decimal.mark = ".")), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Grouped Barplot for Plannivaa",
    x = "Planlagt Utbygd (km^2)",
    y = "Plan Type"
  ) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

print(p)


# Load necessary packages
library(ggplot2)
library(dplyr)

# Reorder data for plotting
aggregated_data <- aggregated_data %>%
  arrange(Plannivaa, desc(planlagt_utbygd_km2)) %>%
  mutate(plan_type = factor(plan_type, levels = unique(plan_type))) %>%
  mutate(Plannivaa = factor(Plannivaa, levels = c("Oversiktsplan", "Detaljplan")))

# Create the grouped bar plot
p <- ggplot(aggregated_data, aes(x = plan_type, y = planlagt_utbygd_km2, fill = Plannivaa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = as.character(round(planlagt_utbygd_km2))), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5, 
            hjust = -0.2,
            size = 3.5 * 0.7) + # Adjusted text size to 70% of default size (default size is 5)
  labs(
    title = "Samlet planlagt utbyggingsplan fordelt på plantype",
    x = "Plantype",
    y = "Areal i kvadratkilometer",
    fill = "Plannivå"
  ) +
  scale_fill_manual(values = c("#0072B2", "#E69F00")) +
  scale_y_continuous(limits = c(0, max(aggregated_data$planlagt_utbygd_km2) * 1.15)) + # 15% more space on the right
  coord_flip() +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

print(p)



# Calculate the width and height in inches
width_in_inches <- 20 / 2.54
height_in_inches <- 10 / 2.54

# Save as before
ggsave("plots_and_tables/oversiktsplan_vs_detaljplan.jpg", plot = p, device = "jpeg", width = width_in_inches, height = height_in_inches, dpi = 300)


sum(aggregated_data$planlagt_utbygd_km2)


percentage_summary <- aggregated_data %>%
  group_by(Plannivaa) %>%
  summarize(total_km2 = sum(planlagt_utbygd_km2)) %>%
  mutate(percentage = (total_km2 / sum(total_km2)) * 100)

print(percentage_summary)


