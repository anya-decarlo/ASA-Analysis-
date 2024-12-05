# Load the data.table library
library(data.table)

source("~/ASA-Analysis-/preprocess_Data.R")

dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

str(dat)
(dat)
class(dat)

#Get Unique Values for Study Status 
print(unique(dat$Study_Status))

#Convert character dates to Date object for time series analysis 
dat$Start_Date <- as.IDate(dat$Start_Date)

# Sample data
dat$Completion_Date <- as.IDate(dat$Completion_Date)

# Get the frequency distribution
table(dat$Study_Status)

# For a more detailed table with proportions
print(prop.table(table(dat$Study_Status)))

# If you want a visualization
library(ggplot2)
 ggplot(dat, aes(x = Study_Status)) +
  geom_bar() +
  labs(title = "Distribution of Study Status", x = "Study Status", y = "Count") +
  theme_minimal()


# Subsetting data by $Study_Status withdrawn or suspended to examine trends 
hf <- dat[Study_Status %in% c("WITHDRAWN" , "SUSPENDED")]

# Verify hf has correct subet of data
#print(unique(hf$Study_Status))

print(unique(hf$Study_Type))

# Load data.table
library(data.table)

# Calculate the distribution of Study_Type in the subset
study_type_distribution <- hf[, .N, by = Study_Type]

# Rename columns for clarity
setnames(study_type_distribution, "N", "Count")

# Add proportions if needed
study_type_distribution[, Proportion := Count / sum(Count)]

# View the table
print(study_type_distribution)

View(hf)


print(dim(dat))

