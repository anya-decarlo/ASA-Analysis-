# Load the data.table library
library(data.table)
library(dplyr)
library(gt)


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

# Calculate counts and proportions of Study_Status 
study_counts <- lf %>%
  group_by(Study_Status) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Display the table
print(study_counts)
formatted_table <- study_counts %>%
  gt() %>%
  fmt_number(columns = "Proportion", decimals = 2) 
  
# Save the formatted gt table as a PNG
gtsave(formatted_table, filename = "study_counts_table.png")

#print(dim(lf))
#print(dim(mf))
print(unique(mf$Study_Status))

print(unique(mf$Age))

# Calculate counts and proportions of Study_Status 
age_counts <- mf %>%
  group_by(Age) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count))

# Display the table
print(age_counts)
formatted_table <- age_counts %>%
  gt() %>%
  fmt_number(columns = "Proportion", decimals = 2)
  

# Save the formatted gt table as a PNG
gtsave(formatted_table, filename = "age_counts_table.png")

print(dim(mf))