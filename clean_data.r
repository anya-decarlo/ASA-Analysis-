# Load the data.table library
library(data.table)

# identify source of data 
source("~/ASA-Analysis-/preprocess_Data.R")

# read data into data table 
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

# get structure of data 
str(dat)

#Get Unique Values for Study Status 
print(unique(dat$Study_Status))

#Convert character dates to Date object for time series analysis 
dat$Start_Date <- as.IDate(dat$Start_Date)

# Sample data
dat$Completion_Date <- as.IDate(dat$Completion_Date)

