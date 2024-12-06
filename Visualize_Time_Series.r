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

# Subset the data to only include studies that are active 
mf <- lf[!(Study_Status %in% excluded_statuses)]

#Convert character dates to Date object for time series analysis 
lf$Start_Date <- as.IDate(lf$Start_Date)
lf$Completion_Date <- as.IDate(lf$Completion_Date)

library(ggplot2)

# Ensure start dates are in date format
mf <- mf %>%
  mutate(Start_Date = as.Date(Start_Date))

# Create the time series plot
time_series_plot <- ggplot(mf, aes(x = Start_Date, y = NCT_Number)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  labs(
    title = "Time Series of Study Start Dates",
    x = "Start Date",
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

# Save the plot as PNG
ggsave("time_series_start_dates.png", plot = time_series_plot, width = 10, height = 6, dpi = 300, bg = "white")







library(ggplot2)
library(dplyr)

# Aggregate the data by Start_Date and Study_Type
aggregated_data <- mf %>%
  group_by(Start_Date, Study_Type) %>%
  summarise(Study_Count = n(), .groups = "drop")

# Create the time series plot
time_series_plot <- ggplot(aggregated_data, aes(x = Start_Date, y = Study_Count, color = Study_Type)) +
  geom_line(size = 1) +  # Add lines to show trends over time
  geom_point(size = 2, alpha = 0.8) +  # Points for each date
  scale_color_manual(values = c("steelblue", "darkorange")) +  # Customize colors
  labs(
    title = "Time Series of Study Start Dates by Study Type",
    x = "Start Date",
    y = "Number of Studies",
    color = "Study Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Save the plot as PNG
ggsave("time_series_start_dates_cleaned.png", plot = time_series_plot, width = 10, height = 6, dpi = 300, bg = "white")



library(ggplot2)
library(dplyr)

# Aggregate data by Start_Date and Study_Type
aggregated_data <- mf %>%
  group_by(Start_Date, Study_Type) %>%
  summarise(Study_Count = n(), .groups = "drop") 

# Create the time series plot with trends
time_series_trend_plot <- ggplot(aggregated_data, aes(x = Start_Date, y = Study_Count, color = Study_Type)) +
  geom_line(size = 1.2) +  # Add smooth line to show trends
  geom_point(size = 1.5, alpha = 0.7) +  # Points for each data entry
  scale_color_manual(values = c("steelblue", "darkorange")) +  # Customize colors
  labs(
    title = "Trends in Study Start Dates by Study Type",
    x = "Start Date",
    y = "Number of Studies",
    color = "Study Type"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# Save the plot as PNG
ggsave("study_start_date_trends.png", plot = time_series_trend_plot, width = 10, height = 6, dpi = 300, bg = "white")


library(ggplot2)
library(dplyr)

# Extract year from Start_Date and aggregate data
mf_yearly <- mf %>%
  mutate(Start_Year = format(as.Date(Start_Date), "%Y")) %>%  # Extract year
  group_by(Start_Year) %>%
  summarise(Study_Count = n(), .groups = "drop")  # Aggregate by year

# Convert Start_Year to numeric for proper ordering in the plot
mf_yearly$Start_Year <- as.numeric(mf_yearly$Start_Year)

# Create the time series plot
yearly_trend_plot <- ggplot(mf_yearly, aes(x = Start_Year, y = Study_Count)) +
  geom_line(color = "steelblue", size = 1.2) +  # Line to show trends
  geom_point(color = "darkblue", size = 2, alpha = 0.8) +  # Points for each year
  labs(
    title = "Yearly Trends in Study Start Dates",
    x = "Year",
    y = "Number of Studies"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the plot as PNG
ggsave("yearly_study_trends.png", plot = yearly_trend_plot, width = 10, height = 6, dpi = 300, bg = "white")



library(ggplot2)
library(dplyr)

# Extract year from Start_Date and aggregate data, filter for years up to 2024
mf_yearly <- mf %>%
  mutate(Start_Year = as.numeric(format(as.Date(Start_Date), "%Y"))) %>%  # Extract year as numeric
  filter(Start_Year <= 2024) %>%  # Filter years up to 2024
  group_by(Start_Year) %>%
  summarise(Study_Count = n(), .groups = "drop")  # Aggregate by year

# Create the time series plot
yearly_trend_plot <- ggplot(mf_yearly, aes(x = Start_Year, y = Study_Count)) +
  geom_line(color = "steelblue", size = 1.2) +  # Line to show trends
  geom_point(color = "darkblue", size = 2, alpha = 0.8) +  # Points for each year
  labs(
    title = "Yearly Trends in Clinical Trial Start Dates (Up to 2024)",
    x = "Year",
    y = "Number of Studies"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12)
  )

# Save the plot as PNG
ggsave("yearly_study_trends.png", plot = yearly_trend_plot, width = 10, height = 6, dpi = 300, bg = "white")