# --------------------------------------------------------------------------- #
# Name: alder_planlagte_utbyggingsomraader_slett.R
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
head(data)
glimpse(data)

# Assuming data$ikrafttredelsesdato is a numeric variable, convert it to character
data$ikrafttredelsesdato <- as.character(data$ikrafttredelsesdato)

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

# Aggregate to the individual plan level, based on planid  ----------------

# Count the number of unique planid values
num_unique_planids <- filtered_data %>% 
  distinct(planid) %>% 
  nrow()

num_unique_planids

# Print the results
cat("Number of unique planids:", num_unique_planids, "\n")

# Find the maximum number of observations with the same planid
max_obs_per_planid <- filtered_data %>% 
  group_by(planid) %>% 
  summarise(num_observations = n()) %>% 
  pull(num_observations) %>% 
  max()

max_obs_per_planid

# Print the results
cat("Maximum number of observations with the same planid:", max_obs_per_planid, "\n")

filtered_data %>% 
  group_by(planid) %>% 
  summarise(num_observations = n()) %>% 
  pull(num_observations) %>% 
  min()

filtered_data %>% 
  group_by(planid) %>% 
  summarise(num_observations = n()) %>% 
  pull(num_observations) %>% 
  median()

# Create a new dataset with aggregated observations
aggregated_data <- filtered_data %>%
  group_by(planid) %>%
  summarise(plankilde = first(plankilde),
            planlagt_utbygd_areal = sum(planlagt_utbygd_areal_m2),
            plannavn = first(plannavn),
            plantype = first(plantype),
            planstatus = first(planstatus),
            ikrafttredelsesdato = first(ikrafttredelsesdato),
            kommunenummer = first(kommunenummer),
            kommune = first(kommune))

# Print the first few rows of the aggregated dataset
head(aggregated_data)
glimpse(aggregated_data)

unique(aggregated_data$ikrafttredelsesdato)
min(aggregated_data$ikrafttredelsesdato)
max(aggregated_data$ikrafttredelsesdato)
mean(aggregated_data$ikrafttredelsesdato)
median(aggregated_data$ikrafttredelsesdato)
summary(aggregated_data$ikrafttredelsesdato)


# Convert plantype to a factor
aggregated_data$plantype <- as.factor(aggregated_data$plantype)

# Create the ggplot
ggplot(aggregated_data, aes(x = plantype, y = ikrafttredelsesdato)) +
  geom_boxplot() +  # Create the boxplot
  geom_jitter(width = 0.2, alpha = 0.7, color = "blue") +  # Add jittered points
  labs(x = "Plantype", y = "Ikrafttredelsesdato") +  # Label axes
  theme_minimal()  # Use a minimalistic theme


# Filter out observations older than 1924
filtered_data <- aggregated_data[aggregated_data$ikrafttredelsesdato >= as.Date("1929-01-01"), ]

# Create the ggplot with updated aesthetics
ggplot(filtered_data, aes(x = plantype, y = ikrafttredelsesdato, color = factor(plantype))) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.7, color = "steelblue") +
  labs(x = "Plantype", y = "Ikrafttredelsesdato") +
  theme_minimal() +
  scale_color_manual(
    values = c(
      "20" = "red",
      "21" = "blue",
      "22" = "green",
      "30" = "purple",
      "31" = "orange",
      "32" = "yellow",
      "33" = "pink",
      "34" = "brown",
      "35" = "cyan"
    ),
    labels = c(
      "20" = "20 Kommuneplanens arealdel",
      "21" = "21 Kommunedelplan",
      "22" = "22 Mindre endring av kommune(del)plan",
      "30" = "30 Eldre reguleringsplan",
      "31" = "31 Mindre reguleringsendring",
      "32" = "32 Bebyggelsesplan iht. reguleringsplan",
      "33" = "33 Bebyggelsesplan iht. kommuneplanens arealdel",
      "34" = "34 Områderegulering",
      "35" = "35 Detaljregulering"
    )
  )

# Create the ggplot with updated aesthetics
ggplot(filtered_data, aes(x = factor(plantype), y = ikrafttredelsesdato, color = factor(plantype))) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "steelblue") +
	geom_boxplot(alpha = 0, position = position_dodge(width = 0.75)) +
  labs(x = "Plantype", y = "Ikrafttredelsesdato") +
  theme_minimal() +
  scale_color_manual(
    name = "Plantype",
    values = c(
      "20" = "red",
      "21" = "blue",
      "22" = "green",
      "30" = "purple",
      "31" = "orange",
      "32" = "yellow",
      "33" = "pink",
      "34" = "brown",
      "35" = "cyan"
    ),
    labels = c(
      "20" = "20 Kommuneplanens arealdel",
      "21" = "21 Kommunedelplan",
      "22" = "22 Mindre endring av kommune(del)plan",
      "30" = "30 Eldre reguleringsplan",
      "31" = "31 Mindre reguleringsendring",
      "32" = "32 Bebyggelsesplan iht. reguleringsplan",
      "33" = "33 Bebyggelsesplan iht. kommuneplanens arealdel",
      "34" = "34 Områderegulering",
      "35" = "35 Detaljregulering"
    )
  )

# Create the ggplot without colors for the boxes, dropped legend, and angled x-axis labels
ggplot(filtered_data, aes(x = factor(plantype), y = ikrafttredelsesdato)) +
  geom_jitter(width = 0.2, alpha = 0.1, color = "steelblue") +
	geom_boxplot(alpha = 0, fill = "white", color = "black", position = position_dodge(width = 0.75)) +
  labs(x = NULL, y = "Ikrafttredelsesdato") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = c(
      "20" = "20 Kommuneplanens arealdel",
      "21" = "21 Kommunedelplan",
      "22" = "22 Mindre endring av kommune(del)plan",
      "30" = "30 Eldre reguleringsplan",
      "31" = "31 Mindre reguleringsendring",
      "32" = "32 Bebyggelsesplan iht. reguleringsplan",
      "33" = "33 Bebyggelsesplan iht. kommuneplanens arealdel",
      "34" = "34 Områderegulering",
      "35" = "35 Detaljregulering"
    )
  ) +
  guides(color = FALSE)

filtered_data
glimpse(filtered_data)



# Barplots in facets ------------------------------------------------------

# Define the mapping for plantype to the desired facet titles
facet_labels <- c(
  "20" = "20 Kommuneplanens arealdel",
  "21" = "21 Kommunedelplan",
  "30" = "30 Eldre reguleringsplan",
  "31" = "31 Mindre reguleringsendring",
  "32" = "32 Bebyggelsesplan iht. reguleringsplan",
  "33" = "33 Bebyggelsesplan iht. kommuneplanens arealdel",
  "34" = "34 Områderegulering",
  "35" = "35 Detaljregulering"
)

# Assuming `filtered_data` has already filtered out plantype 22 and contains ikrafttredelsesdato
filtered_data <- filtered_data %>%
  filter(plantype != 22) %>%
  mutate(decade = year(ikrafttredelsesdato) %/% 10 * 10) %>%
  filter(decade >= 1920, decade <= 2030)

# Drop unused levels of plantype
filtered_data$plantype <- droplevels(as.factor(filtered_data$plantype))

# Define breaks and labels for x-axis
unique_decades <- sort(unique(filtered_data$decade))
breaks_x <- unique_decades
labels_x <- paste0(unique_decades, "–", c(tail(unique_decades, -1), max(unique_decades) + 10))

# Function for labeling facets
label_facets <- as_labeller(facet_labels)

plot <- ggplot(filtered_data, aes(x = as.factor(decade))) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3 * 0.8) + # Reduce size to 0.7 of the current size
  facet_wrap(~ plantype, labeller = label_facets, scales = "free_y", ncol = 2) +
  labs(
    x = "År",
    y = "Antall planer",
    title = "Antall arealplaner med arealreserve fra hvert tiår siden 1920-tallet fordelt på plantype"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = breaks_x, labels = labels_x) + # Defines the decade labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add 10% space on top of the tallest bar

plot

# Calculate the width and height in inches
width_in_inches <- 21 / 2.54
height_in_inches <- 28 / 2.54

# Save as before
ggsave("plots_and_tables/alder_planer_med_planreserver.jpg", plot = plot, device = "jpeg", width = width_in_inches, height = height_in_inches, dpi = 300)


# All plans ---------------------------------------------------------------

plot2 <- ggplot(filtered_data, aes(x = as.factor(decade))) +
  geom_bar(fill = "#0072B2") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3 * 0.8) + # Reduce size to 0.7 of the current size
  #facet_wrap(~ plantype, labeller = label_facets, scales = "free_y", ncol = 2) +
  labs(
    x = "År",
    y = "Antall planer",
    title = "Ikrafttredelsesdato for arealplaner med arealreserver for utbygging, alle plantyper fordelt på tiår"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(breaks = breaks_x, labels = labels_x) + # Defines the decade labels
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add 10% space on top of the tallest bar

plot2

# Calculate the width and height in inches
width_in_inches <- 25 / 2.54
height_in_inches <- 15 / 2.54

# Save as before
ggsave("plots_and_tables/alder_planer_med_planreserver_alle.jpg", plot = plot2, device = "jpeg", width = width_in_inches, height = height_in_inches, dpi = 300)


