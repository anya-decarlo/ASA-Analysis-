library(data.table)
library(tidyverse)
library(tidytext)
library(ggplot2)

# Source preprocessing function
source("~/ASA-Analysis-/preprocess_Data.R")

# Read data into data table
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

# Subset the dataset
lf <- dat[, ..desired_columns]

# Convert dates
lf$Start_Date <- as.IDate(lf$Start_Date)
lf$Completion_Date <- as.IDate(lf$Completion_Date)

# Filter for withdrawn and terminated studies
wt_studies <- lf[Study_Status %in% c("TERMINATED", "WITHDRAWN")]

# Create results directory
dir.create("results/withdrawn_terminated_analysis", recursive = TRUE, showWarnings = FALSE)

# Define words to exclude
excluded_words <- c("by", "of", "the", "number", "to", "a", "on", "will", "be", "and", "in", "with", "or", "at", "for")

#############################
# Analyze Conditions
#############################
# Split pipe-delimited conditions and analyze
conditions_analysis <- wt_studies %>%
  select(Conditions) %>%
  separate_rows(Conditions, sep = "\\|") %>%
  filter(!is.na(Conditions), Conditions != "") %>%
  group_by(Conditions) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 20)

# Visualize conditions
ggplot(conditions_analysis, aes(x = reorder(Conditions, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Common Conditions in Withdrawn/Terminated Studies",
       x = "Condition",
       y = "Number of Studies") +
  theme(axis.text.y = element_text(size = 10))
ggsave("results/withdrawn_terminated_analysis/conditions.png", width = 12, height = 8)

#############################
# Analyze Interventions
#############################
# Split pipe-delimited interventions and analyze
interventions_analysis <- wt_studies %>%
  select(Interventions) %>%
  separate_rows(Interventions, sep = "\\|") %>%
  filter(!is.na(Interventions), Interventions != "") %>%
  # Extract intervention type and description
  mutate(
    intervention_type = str_extract(Interventions, "^[^:]+"),
    intervention_desc = str_extract(Interventions, "(?<=: ).+")
  ) %>%
  group_by(intervention_type, intervention_desc) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  slice_head(n = 20)

# Visualize interventions
ggplot(interventions_analysis, aes(x = reorder(intervention_type, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Common Intervention Types in Withdrawn/Terminated Studies",
       x = "Intervention Type",
       y = "Number of Studies") +
  theme(axis.text.y = element_text(size = 10))
ggsave("results/withdrawn_terminated_analysis/interventions.png", width = 12, height = 8)

#############################
# Analyze Primary Outcomes
#############################
# Split pipe-delimited outcomes and analyze
outcomes_analysis <- wt_studies %>%
  select(Primary_Outcome_Measures) %>%
  separate_rows(Primary_Outcome_Measures, sep = "\\|") %>%
  filter(!is.na(Primary_Outcome_Measures), Primary_Outcome_Measures != "") %>%
  # Clean and standardize outcome measures
  mutate(
    outcome_clean = str_replace_all(Primary_Outcome_Measures, "\\[Time Frame: .+\\]", ""),
    outcome_clean = str_trim(outcome_clean)
  ) %>%
  group_by(outcome_clean) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 20)

# Visualize outcomes
ggplot(outcomes_analysis, aes(x = reorder(outcome_clean, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Most Common Primary Outcome Measures in Withdrawn/Terminated Studies",
       x = "Primary Outcome Measure",
       y = "Number of Studies") +
  theme(axis.text.y = element_text(size = 8))
ggsave("results/withdrawn_terminated_analysis/outcomes.png", width = 14, height = 10)

# Generate comprehensive report
report <- c(
  "# Analysis of Withdrawn and Terminated Clinical Trials",
  "",
  paste("Total number of withdrawn/terminated studies analyzed:", nrow(wt_studies)),
  "",
  "## Most Common Medical Conditions",
  paste("1.", conditions_analysis$Conditions[1:15], "(", conditions_analysis$count[1:15], "studies )", collapse = "\n"),
  "",
  "## Most Common Interventions",
  paste("1.", paste(interventions_analysis$intervention_type[1:15], "-", interventions_analysis$intervention_desc[1:15]),
        "(", interventions_analysis$count[1:15], "studies )", collapse = "\n"),
  "",
  "## Most Common Primary Outcome Measures",
  paste("1.", outcomes_analysis$outcome_clean[1:15], "(", outcomes_analysis$count[1:15], "studies )", collapse = "\n")
)

writeLines(report, "results/withdrawn_terminated_analysis/analysis_report.md")

# Save processed data for further analysis if needed
save(conditions_analysis, interventions_analysis, outcomes_analysis,
     file = "results/withdrawn_terminated_analysis/processed_data.RData")

cat("Analysis complete. Results saved in results/withdrawn_terminated_analysis/\n")
