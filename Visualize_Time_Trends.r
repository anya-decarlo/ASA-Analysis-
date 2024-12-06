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
lf$Start_Date <- as.IDate(lf$Start_Date)
lf$Completion_Date <- as.IDate(lf$Completion_Date)

library(ggplot2)
library(dplyr)

library(ggplot2)

# Ensure Start_Date is in the correct date format
mf <- mf %>%
  mutate(Start_Date = as.Date(Start_Date))

# Create the time series plot
time_series_studies_plot <- ggplot(mf, aes(x = Start_Date)) +
  geom_point(aes(y = seq_along(Start_Date)), color = "steelblue", alpha = 0.6, size = 1.5) +  # Plot each study as a point
  labs(
    title = "Time Series of Study Start Dates",
    x = "Start Date",
    y = "Study Index"  # Index to separate points
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Center title, bold, and adjust size
    axis.title = element_text(face = "bold"),  # Bold axis labels
    axis.text = element_text(size = 12),  # Adjust axis text size
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),  # Subtle gridlines for readability
    panel.grid.minor = element_blank()
  )

# Save the plot as a high-quality PNG for publication
ggsave("time_series_studies_plot.png", plot = time_series_studies_plot, width = 10, height = 6, dpi = 300, bg = "white")




library(ggplot2)
library(dplyr)

# Extract year from Start_Date and ensure it's in the correct format
mf <- mf %>%
  mutate(Start_Year = as.numeric(format(as.Date(Start_Date), "%Y")))  # Extract year as numeric for proper plotting

# Create the time series plot
time_series_year_plot <- ggplot(mf, aes(x = Start_Year, y = seq_along(Start_Year))) +
  geom_point(color = "steelblue", alpha = 0.6, size = 1.5) +  # Add points for each study
  geom_jitter(width = 0.3, color = "steelblue", size = 1.5, alpha = 0.6) +  # Jitter to avoid overlapping
  labs(
    title = "Time Series of Study Start Dates by Year",
    x = "Year",
    y = "Study Index"
  ) +
  scale_x_continuous(breaks = seq(min(mf$Start_Year, na.rm = TRUE), max(mf$Start_Year, na.rm = TRUE), 5)) +  # Custom x-axis breaks
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank()
  )

# Save the plot as a high-quality PNG for publication
ggsave("time_series_studies_by_year_cleaned.png", plot = time_series_year_plot, width = 10, height = 6, dpi = 300, bg = "white")


library(gt)

# Calculate stats using your existing data
before_2015 <- table(mf$Start_Year[mf$Start_Year < 2015])
after_2015 <- table(mf$Start_Year[mf$Start_Year >= 2015])
before_counts <- as.numeric(before_2015)
after_counts <- as.numeric(after_2015)

# Perform statistical test
wilcox_result <- wilcox.test(before_counts, after_counts, 
                            alternative = "less", 
                            exact = FALSE)

# Create publication-ready table
stats_table <- data.frame(
  Metric = c(
    "Sample Size (n)",
    "Total Studies",
    "Mean Studies/Year (SD)",
    "Median Studies/Year",
    "Statistical Test",
    "Test Statistic (W)",
    "p-value"
  ),
  `Before 2015` = c(
    length(before_counts),
    sum(before_counts),
    paste0(round(mean(before_counts), 1), " (", round(sd(before_counts), 1), ")"),
    median(before_counts),
    "Wilcoxon rank-sum test",
    round(wilcox_result$statistic, 1),
    format.pval(wilcox_result$p.value, digits = 3)
  ),
  `After 2015` = c(
    length(after_counts),
    sum(after_counts),
    paste0(round(mean(after_counts), 1), " (", round(sd(after_counts), 1), ")"),
    median(after_counts),
    "",
    "",
    ""
  )
)

# Create formatted table
formatted_table <- gt(stats_table) %>%
  tab_header(
    title = "Statistical Analysis of Study Counts Before and After 2015"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Metric
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(style = "italic")
    ),
    locations = cells_body(
      rows = c(5),
      columns = Metric
    )
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = 12,
    heading.title.font.size = 14,
    heading.title.font.weight = "bold",
    column_labels.border.bottom.width = 2
  )

# Save as high-resolution PNG
gtsave(
  formatted_table,
  filename = "statistical_analysis_table.png",
  path = getwd(),
  vwidth = 800,
  vheight = 500
)



# Aggregate total study counts for the two groups
total_before_2015 <- sum(mf$Start_Year < 2015)
total_after_2015 <- sum(mf$Start_Year >= 2015)

# Combine totals into a vector for comparison
study_counts <- c(total_before_2015, total_after_2015)
group_labels <- c("Before 2015", "After 2015")

# Perform a Wilcoxon rank-sum test
wilcox_result <- wilcox.test(study_counts[1], study_counts[2], 
                             alternative = "two.sided", 
                             exact = FALSE)

# Create a publication-ready table
stats_table <- data.frame(
  Metric = c(
    "Group",
    "Total Studies",
    "Statistical Test",
    "Test Statistic (W)",
    "p-value"
  ),
  `Before 2015` = c(
    "Before 2015",
    total_before_2015,
    "Wilcoxon rank-sum test",
    round(wilcox_result$statistic, 1),
    format.pval(wilcox_result$p.value, digits = 3)
  ),
  `After 2015` = c(
    "After 2015",
    total_after_2015,
    "",
    "",
    ""
  )
)

# Format the table
library(gt)

formatted_table <- gt(stats_table) %>%
  tab_header(
    title = "Statistical Analysis of Study Counts Before and After 2015"
  ) %>%
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

# Save the table
gtsave(
  formatted_table,
  filename = "statistical_analysis_table.png",
  path = getwd(),
  vwidth = 800,
  vheight = 500
)

# View the table
print(formatted_table)





library(gt)
library(gt)

# Calculate total counts for each period
before_2015_total <- sum(mf$Start_Year < 2015)
after_2015_total <- sum(mf$Start_Year >= 2015)

# Calculate proportions
total_studies <- before_2015_total + after_2015_total
prop_before <- before_2015_total / total_studies
prop_after <- after_2015_total / total_studies

# Perform chi-square test
chi_result <- chisq.test(c(before_2015_total, after_2015_total))

# Calculate confidence intervals for proportions
ci_before <- prop.test(before_2015_total, total_studies)$conf.int
ci_after <- prop.test(after_2015_total, total_studies)$conf.int

# Calculate odds ratio and its confidence interval
odds_ratio <- (before_2015_total / (total_studies - before_2015_total)) / 
              (after_2015_total / (total_studies - after_2015_total))
odds_ratio_se <- sqrt(sum(1 / c(before_2015_total, after_2015_total, 
                                total_studies - before_2015_total, 
                                total_studies - after_2015_total)))
odds_ratio_ci <- exp(log(odds_ratio) + c(-1, 1) * 1.96 * odds_ratio_se)

# Create publication-ready table
stats_table <- data.frame(
  Metric = c(
    "Total Studies (n)",
    "Proportion of All Studies",
    "95% CI for Proportion",
    "Statistical Tests",
    "Chi-square statistic",
    "p-value",
    "Odds Ratio",
    "95% CI for Odds Ratio"
  ),
  `Before 2015` = c(
    before_2015_total,
    sprintf("%.3f", prop_before),
    sprintf("(%.3f-%.3f)", ci_before[1], ci_before[2]),
    "Chi-square test",
    sprintf("%.2f", chi_result$statistic),
    format.pval(chi_result$p.value, digits = 3),
    sprintf("%.2f", odds_ratio),
    sprintf("(%.2f-%.2f)", odds_ratio_ci[1], odds_ratio_ci[2])
  ),
  `After 2015` = c(
    after_2015_total,
    sprintf("%.3f", prop_after),
    sprintf("(%.3f-%.3f)", ci_after[1], ci_after[2]),
    "",
    "",
    "",
    "",
    ""
  )
)

# Format the table
formatted_table <- gt(stats_table) %>%
  tab_header(
    title = "Statistical Comparison of Studies Before and After 2015",
    subtitle = "Analysis of Total Study Counts"
  ) %>%
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

# Save the table
gtsave(
  formatted_table,
  filename = "complete_statistical_comparisons.png",
  path = getwd(),
  vwidth = 800,
  vheight = 500
)

# View the table in RStudio Viewer
print(formatted_table)