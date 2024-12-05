# Load the data.table library
library(data.table)

# identify source of data 
source("~/ASA-Analysis-/preprocess_Data.R")

# read data into data table 
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

# get structure of data 
#str(dat)

#Get Unique Values for Study Status 
print(unique(dat$Study_Status))

#Convert character dates to Date object for time series analysis 
dat$Start_Date <- as.IDate(dat$Start_Date)

# Sample data
dat$Completion_Date <- as.IDate(dat$Completion_Date)

View(dat$Study_Title)

View(dat$Primary_Outcome_Measures)

#Variables of interest 

#Study_Title Study_Status Brief_Summary 

#Conditions Interventions Primary_Outcome_Measures

#Sex Age 

#Enrollment 

#Study_Type Study_Design 

#Sponser Funder_Type

#Start_Date Completion_Date Results_First_Posted Last_Update_Posted 

#Locations

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

# Subset the dataset
subset_dat <- dat[, ..desired_columns]

str(subset_dat)