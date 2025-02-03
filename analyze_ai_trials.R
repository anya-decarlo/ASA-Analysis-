library(data.table)
library(tidyverse)
library(survival)
library(networkD3)
library(cluster)
library(stats)

# Source preprocessing function
source("preprocess_Data.R")

# Read and preprocess data
dat <- read_data("ctg_studies.csv")

# Identify AI trials using keyword search in multiple fields
ai_keywords <- c("artificial intelligence", "machine learning", "deep learning", 
                 "neural network", "computer vision", "natural language processing",
                 "AI", "ML", "DL", "NLP")

dat[, is_ai := FALSE]
for(keyword in ai_keywords) {
  dat[, is_ai := is_ai | 
      grepl(keyword, Study_Title, ignore.case = TRUE) |
      grepl(keyword, Brief_Summary, ignore.case = TRUE) |
      grepl(keyword, Interventions, ignore.case = TRUE)]
}

# 1. Temporal Analysis
temporal_analysis <- function(data) {
  # Convert dates to proper format
  data[, Start_Date := as.Date(Start_Date)]
  data[, Completion_Date := as.Date(Completion_Date)]
  
  # Time series of trial registrations
  yearly_counts <- data[, .(
    ai_trials = sum(is_ai),
    total_trials = .N
  ), by = .(year = year(Start_Date))]
  
  # Plot time trends
  ggplot(yearly_counts, aes(x = year)) +
    geom_line(aes(y = ai_trials, color = "AI Trials")) +
    geom_line(aes(y = total_trials/10, color = "All Trials (scaled)")) +
    scale_y_continuous(
      name = "Number of AI Trials",
      sec.axis = sec_axis(~.*10, name = "Total Number of Trials")
    ) +
    labs(title = "Growth of AI Clinical Trials Over Time",
         x = "Year") +
    theme_minimal()
  ggsave("results/ai_analysis/temporal_trends.png")
  
  # Survival analysis
  surv_data <- data[!is.na(Start_Date) & !is.na(Completion_Date), ]
  surv_data[, duration := as.numeric(Completion_Date - Start_Date)]
  
  fit <- survfit(Surv(duration, Study_Status == "COMPLETED") ~ is_ai, 
                data = surv_data)
  
  # Log-rank test
  log_rank <- survdiff(Surv(duration, Study_Status == "COMPLETED") ~ is_ai, 
                      data = surv_data)
  
  return(list(yearly_counts = yearly_counts, 
              survival_fit = fit,
              log_rank = log_rank))
}

# 2. Network Analysis
network_analysis <- function(data) {
  # Extract unique institutions from Sponsor and Collaborators
  sponsors <- unique(data[is_ai == TRUE, .(Sponsor)])
  
  # Create collaboration network
  collab_data <- data[is_ai == TRUE & !is.na(Collaborators),
                     .(Sponsor, Collaborators)]
  
  # Network metrics
  institution_counts <- data[is_ai == TRUE, .N, by = Sponsor]
  setorder(institution_counts, -N)
  
  return(list(top_institutions = head(institution_counts, 20)))
}

# 3. Study Design Analysis
design_analysis <- function(data) {
  # Compare study types
  study_type_comp <- data[, .(
    count = .N,
    pct = .N/nrow(data)
  ), by = .(is_ai, Study_Type)]
  
  # Sample size comparison
  enrollment_stats <- data[, .(
    mean_size = mean(Enrollment, na.rm = TRUE),
    median_size = median(Enrollment, na.rm = TRUE),
    sd_size = sd(Enrollment, na.rm = TRUE)
  ), by = is_ai]
  
  # Wilcoxon test for enrollment differences
  wilcox_test <- wilcox.test(Enrollment ~ is_ai, data = data)
  
  return(list(study_types = study_type_comp,
              enrollment = enrollment_stats,
              enrollment_test = wilcox_test))
}

# 4. Clinical Domain Analysis
clinical_analysis <- function(data) {
  # Split conditions and create frequency table
  conditions <- data[is_ai == TRUE, .(NCT_Number, Conditions)]
  conditions[, Conditions := strsplit(Conditions, "\\|")]
  conditions <- conditions[, .(Condition = unlist(Conditions)), by = NCT_Number]
  
  condition_freq <- conditions[, .N, by = Condition]
  setorder(condition_freq, -N)
  
  # Cluster analysis of conditions
  # Create term-document matrix
  condition_matrix <- table(conditions$NCT_Number, conditions$Condition)
  
  # Perform hierarchical clustering
  dist_matrix <- dist(condition_matrix)
  hc <- hclust(dist_matrix, method = "ward.D2")
  
  return(list(top_conditions = head(condition_freq, 20),
              condition_clusters = hc))
}

# Run analyses
dir.create("results/ai_analysis", recursive = TRUE, showWarnings = FALSE)

temporal_results <- temporal_analysis(dat)
network_results <- network_analysis(dat)
design_results <- design_analysis(dat)
clinical_results <- clinical_analysis(dat)

# Generate comprehensive report
report <- c(
  "# Analysis of AI Clinical Trials",
  "",
  "## 1. Temporal Trends",
  sprintf("- Total number of AI trials identified: %d", sum(dat$is_ai)),
  sprintf("- Growth rate: %0.1f%% per year", 
          (tail(temporal_results$yearly_counts$ai_trials, 1) / 
             head(temporal_results$yearly_counts$ai_trials, 1) - 1) * 100),
  sprintf("- Log-rank test p-value: %0.3f", temporal_results$log_rank$pval),
  "",
  "## 2. Leading Institutions",
  paste("Top 5 institutions by number of AI trials:",
        paste(head(network_results$top_institutions$Sponsor, 5), collapse = "\n")),
  "",
  "## 3. Study Design Characteristics",
  sprintf("Mean enrollment in AI trials: %0.1f", 
          design_results$enrollment[is_ai == TRUE, mean_size]),
  sprintf("Mean enrollment in non-AI trials: %0.1f",
          design_results$enrollment[is_ai == FALSE, mean_size]),
  sprintf("Wilcoxon test p-value: %0.3f", 
          design_results$enrollment_test$p.value),
  "",
  "## 4. Clinical Domains",
  "Top 5 conditions studied in AI trials:",
  paste(head(clinical_results$top_conditions$Condition, 5), collapse = "\n")
)

writeLines(report, "results/ai_analysis/comprehensive_report.md")

# Save processed data for further analysis
save(temporal_results, network_results, design_results, clinical_results,
     file = "results/ai_analysis/processed_results.RData")

cat("Analysis complete. Results saved in results/ai_analysis/\n")
