library(data.table)
library(dplyr)
library(gt)

# identify source of data 
source("~/ASA-Analysis-/preprocess_Data.R")

# read data into data table 
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

# Define the desired columns
desired_columns <- c("NCT_Number",
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

library(ggplot2)

histogram_plot <- ggplot(mf, aes(x = Enrollment)) +
  geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Study Enrollment Numbers",
    x = "Enrollment Number",
    y = "Frequency"
  ) +
  xlim(0, 5000) + # Adjust this limit based on the range of your data
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
    axis.title = element_text(face = "bold")
  )

# Save as PNG with a white background
ggsave("enrollment_histogram.png", plot = histogram_plot, width = 8, height = 6, dpi = 300, bg = "white")
# Save as PNG
ggsave("enrollment_histogram.png", plot = histogram_plot, width = 8, height = 6, dpi = 300)


library(ggplot2)


library(dplyr)
library(gt)


library(dplyr)

# Categorize Enrollment into groups
mf <- mf %>%
  mutate(Enrollment_Group = case_when(
    Enrollment < 1000 ~ "1 -1000 Participants",
    Enrollment >= 1000 & Enrollment < 10000 ~ "1000+ Participants",
    Enrollment >= 10000 ~ "10,000+ Participants"
  ))


# Calculate counts and proportions
enrollment_by_type <- mf %>%
  group_by(Study_Type, Enrollment_Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Study_Type) %>%
  mutate(Proportion = Count / sum(Count))

library(gt)

# Create a formatted table
formatted_table <- enrollment_by_type %>%
  gt() %>%

  fmt_number(columns = "Proportion", decimals = 2) # Format proportions

# Save the table as a PNG
gtsave(formatted_table, filename = "enrollment_by_type_table.png")