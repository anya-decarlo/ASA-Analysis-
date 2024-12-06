# Load the data.table library
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
s_excluded_statuses <- c("COMPLETED")

# Subset the data to only include studies that are active 
mf <- lf[!(Study_Status %in% excluded_statuses)]
mfn <- mf[!(Study_Status %in% s_excluded_statuses)]

#Convert character dates to Date object for time series analysis 
mf$Start_Date <- as.IDate(mf$Start_Date)
mf$Completion_Date <- as.IDate(mf$Completion_Date)

library(ggplot2)
library(dplyr)
# Subset the data to only include studies that are active
mf <- lf[!(Study_Status %in% excluded_statuses)]
mfn <- mf[!(Study_Status %in% s_excluded_statuses)]

# Convert character dates to Date object for time series analysis
mf$Start_Date <- as.IDate(mf$Start_Date)
mf$Completion_Date <- as.IDate(mf$Completion_Date)

# Extract years from Start_Date
mf$Start_Year <- as.numeric(format(mf$Start_Date, "%Y"))

library(ggplot2)
library(dplyr)
library(gt)

# Calculate total counts for each period using Start_Year
before_2015_total <- sum(mf$Start_Year < 2015, na.rm = TRUE)
after_2015_total <- sum(mf$Start_Year >= 2015, na.rm = TRUE)

# Calculate proportions
total_studies <- before_2015_total + after_2015_total
prop_before <- before_2015_total / total_studies
prop_after <- after_2015_total / total_studies

# Calculate confidence intervals for proportions
ci_before <- prop.test(before_2015_total, total_studies)$conf.int
ci_after <- prop.test(after_2015_total, total_studies)$conf.int

# Create publication-ready table
stats_table <- data.frame(
  Metric = c(
    "Total Studies (n)",
    "Proportion of All Studies",
    "95% CI for Proportion"
  ),
  `Before 2015` = c(
    before_2015_total,
    sprintf("%.3f", prop_before),
    sprintf("(%.3f-%.3f)", ci_before[1], ci_before[2])
  ),
  `After 2015` = c(
    after_2015_total,
    sprintf("%.3f", prop_after),
    sprintf("(%.3f-%.3f)", ci_after[1], ci_after[2])
  )
)

# Format the table
formatted_table <- gt(stats_table) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.border.bottom.width = 2
  )

# Save the table as PNG
gtsave(
  formatted_table,
  filename = "statistical_comparison_table.png",
  path = getwd(),
  vwidth = 800,
  vheight = 300  # Reduced height since table is shorter
)

# Print intermediate results for debugging
print("Counts:")
print(paste("Before 2015:", before_2015_total))
print(paste("After 2015:", after_2015_total))
print(paste("Total:", total_studies))



library(dplyr)
library(gt)

# Calculate total counts for each period using Start_Year
before_2015_total <- sum(mf$Start_Year < 2015, na.rm = TRUE)
after_2015_total <- sum(mf$Start_Year >= 2015, na.rm = TRUE)

# Perform chi-square test
chi_result <- chisq.test(c(before_2015_total, after_2015_total))

# Function to format p-value
format_pvalue <- function(p) {
  if (p < 0.001) return("< 0.001")
  return(sprintf("%.3f", p))
}

# Create publication-ready table
stats_table <- data.frame(
  Metric = c(
    "Total Studies Before 2015",
    "Total Studies After 2015",
    "Chi-square statistic",
    "Degrees of Freedom",
    "p-value"
  ),
  Value = c(
    before_2015_total,
    after_2015_total,
    sprintf("%.2f", chi_result$statistic),
    chi_result$parameter,
    format_pvalue(chi_result$p.value)
  )
)

# Format the table
formatted_table <- gt(stats_table) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.border.bottom.width = 2
  )

# Save the table as PNG
gtsave(
  formatted_table,
  filename = "chi_square_results.png",
  path = getwd(),
  vwidth = 800,
  vheight = 300
)