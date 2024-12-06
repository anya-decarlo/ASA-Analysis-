# Load the data.table library
library(data.table)
library(dplyr)
library(gt)
library(tidytext)

# identify source of data 
source("~/ASA-Analysis-/preprocess_Data.R")

# read data into data table 
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

# Define the desired columns
desired_columns <- c(
  "Study_Title", "Study_Status", "Brief_Summary",
  "Conditions", "Interventions", "Primary_Outcome_Measures",
  "Sex", "Age", 
  "Enrollment", 
  "Study_Type", "Study_Design",
  "Sponsor", "Funder_Type",
  "Start_Date", "Completion_Date", "Results_First_Posted", "Last_Update_Posted",
  "Locations"
)

# Subset the dataset to include all studies but only the desired_columns
lf <- dat[, ..desired_columns]

#Convert character dates to Date object for time series analysis 
lf$Start_Date <- as.IDate(lf$Start_Date)
lf$Completion_Date <- as.IDate(lf$Completion_Date)

# Get Unque Values from Age 
print(unique(lf$Age))
print(unique(lf$Study_Status))

# Define the statuses to exclude
excluded_statuses <- c("TERMINATED" , "WITHDRAWN", "UNKNOWN", "SUSPENDED")

# Subset the data to only include studies that are active 
mf <- lf[!(Study_Status %in% excluded_statuses)]

##############################
#Conditions
#############################

bigrams <- mf  %>%
  select(Conditions) %>%
  unnest_tokens(bigram, Conditions, token = "ngrams", n = 1)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

bigrams <- mf  %>%
  select(Conditions) %>%
  unnest_tokens(bigram, Conditions, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

# Load necessary libraries
library(ggplot2)

# Prepare the data for visualization
bigram_counts_clean <- bigram_counts %>%
  filter(!is.na(bigram))  # Remove NA values if present

# Create the bar chart
bigram_plot <- ggplot(bigram_counts_clean, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Bigram Frequency of Conditions Studied in AI Clinical Trials",
    x = "Bigram",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot as PNG
ggsave("bigram_conditions.png", plot = bigram_plot, width = 10, height = 6, dpi = 300)







trigrams <- mf %>%
  select(Conditions) %>%
  unnest_tokens(trigram, Conditions, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)