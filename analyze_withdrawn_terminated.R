library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

# Read the data
df <- fread("/Users/anyadecarlo/ctg_studies.csv", sep = ",", na.strings = "")

# Print column names and unique status values
print("Column names:")
print(names(df))
print("\nUnique Study_Status values:")
print(unique(df$Study_Status))

# Filter for withdrawn and terminated studies
wt_studies <- df[Study_Status %in% c("WITHDRAWN", "TERMINATED")]

# Analysis of conditions, outcomes, and interventions
condition_summary <- wt_studies[, .(
  count = .N,
  percentage = .N/nrow(wt_studies) * 100
), by = .(Conditions)]

intervention_summary <- wt_studies[, .(
  count = .N,
  percentage = .N/nrow(wt_studies) * 100
), by = .(Interventions)]

# Create summary tables
write.csv(condition_summary[order(-count)], "results/withdrawn_terminated_conditions.csv")
write.csv(intervention_summary[order(-count)], "results/withdrawn_terminated_interventions.csv")

# Create visualizations
ggplot(condition_summary[order(-count)][1:10], aes(x = reorder(Conditions, count), y = count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Conditions in Withdrawn/Terminated Studies",
       x = "Condition",
       y = "Count")
ggsave("results/top_conditions_wt.png")

# Analyze primary outcomes
outcome_summary <- wt_studies[, .(
  count = .N,
  percentage = .N/nrow(wt_studies) * 100
), by = .(`Primary Outcome Measures`)]

write.csv(outcome_summary[order(-count)], "results/withdrawn_terminated_outcomes.csv")

# Print summary statistics
cat("\nSummary of Withdrawn and Terminated Studies:\n")
cat("Total number of withdrawn/terminated studies:", nrow(wt_studies), "\n")
cat("Percentage of total studies:", (nrow(wt_studies)/nrow(df))*100, "%\n")

# Additional analysis of reasons for withdrawal/termination
if ("Why_Stopped" %in% names(df)) {
  reason_summary <- wt_studies[, .(
    count = .N,
    percentage = .N/nrow(wt_studies) * 100
  ), by = .(Why_Stopped)]
  
  write.csv(reason_summary[order(-count)], "results/withdrawal_termination_reasons.csv")
}
