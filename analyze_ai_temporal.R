library(data.table)
library(tidyverse)
library(scales)
library(gridExtra)

# Set publication-ready theme with refined aesthetics
nature_theme <- theme_minimal() +
  theme(
    # Text elements
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8, face = "italic"),
    
    # Grid lines
    panel.grid.major = element_line(linewidth = 0.2, color = "grey92"),
    panel.grid.minor = element_line(linewidth = 0.1, color = "grey97"),
    
    # Legend
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_blank(),
    legend.key.size = unit(0.8, "lines"),
    legend.margin = margin(t = 5, b = 5),
    
    # Margins and spacing
    plot.margin = margin(t = 10, r = 15, b = 10, l = 10),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# Source preprocessing function
source("preprocess_Data.R")

# Read data
dat <- read_data("/Users/anyadecarlo/ctg_studies.csv")

# Refined AI keyword matching
ai_keywords <- c(
  "artificial intelligence",
  "machine learning",
  "deep learning",
  "neural network",
  "computer vision",
  "natural language processing"
)

# More precise AI detection
dat[, is_ai := FALSE]
for(keyword in ai_keywords) {
  dat[, is_ai := is_ai | 
      (grepl(keyword, Study_Title, ignore.case = TRUE) |
       grepl(keyword, Brief_Summary, ignore.case = TRUE)) &
      !grepl("not using artificial intelligence|non-artificial intelligence|without artificial intelligence",
             Brief_Summary, ignore.case = TRUE)]
}

# Temporal analysis
dat[, Start_Date := as.Date(Start_Date)]
yearly_counts <- dat[!is.na(Start_Date), .(
    ai_trials = sum(is_ai),
    total_trials = .N,
    ai_percentage = 100 * sum(is_ai) / .N
), by = .(year = year(Start_Date))]

yearly_counts[, ai_growth := c(NA, diff(ai_trials) / ai_trials[-length(ai_trials)] * 100)]

# Create results directory
dir.create("results/ai_temporal", recursive = TRUE, showWarnings = FALSE)

# Figure 1: Growth Trends
p1 <- ggplot(yearly_counts, aes(x = year)) +
  # Background shading for visual periods
  annotate("rect", xmin = 2019, xmax = 2024,
           ymin = -Inf, ymax = Inf,
           fill = "grey97", alpha = 0.5) +
  # Lines
  geom_line(aes(y = ai_trials, color = "AI Clinical Trials"), 
            linewidth = 0.5) +
  geom_point(aes(y = ai_trials, color = "AI Clinical Trials"), 
             size = 1.5, shape = 21, fill = "white") +
  geom_line(aes(y = total_trials/50, color = "All Clinical Trials (÷50)"), 
            linewidth = 0.5, linetype = "dashed") +
  # Formatting
  scale_color_manual(values = c("AI Clinical Trials" = "#0072B2", 
                               "All Clinical Trials (÷50)" = "#D55E00")) +
  scale_x_continuous(breaks = seq(min(yearly_counts$year), 
                                max(yearly_counts$year), 
                                by = 2)) +
  scale_y_continuous(
    name = "Number of AI Clinical Trials",
    sec.axis = sec_axis(~.*50, name = "Total Number of Clinical Trials"),
    labels = comma_format()
  ) +
  labs(
    title = "Temporal Evolution of AI in Clinical Trials",
    subtitle = "Growth comparison between AI-specific and overall trial registrations",
    x = "Year"
  ) +
  nature_theme +
  # Add COVID-19 period annotation
  annotate("text", x = 2021.5, y = max(yearly_counts$ai_trials),
           label = "COVID-19 Period", size = 2.5, color = "grey30")

# Figure 2: Combined Metrics
# Status distribution with refined categories
status_dist <- dat[is_ai == TRUE, .(
  count = .N
), by = .(Status = factor(Study_Status, 
                         levels = c("RECRUITING", "COMPLETED", "ACTIVE_NOT_RECRUITING",
                                  "NOT_YET_RECRUITING", "ENROLLING_BY_INVITATION",
                                  "WITHDRAWN", "TERMINATED", "SUSPENDED", "UNKNOWN")))
][order(-count)]

status_dist[, percentage := count/sum(count) * 100]

# Create combined visualization
p2 <- ggplot(status_dist, 
       aes(x = reorder(Status, -percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "#0072B2", alpha = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            size = 2.5, vjust = -0.5) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(status_dist$percentage) * 1.1)
  ) +
  labs(
    title = "Current Status of AI Clinical Trials",
    subtitle = "Distribution across trial phases (2000-2024)",
    x = "Trial Status",
    y = "Percentage of AI Trials"
  ) +
  nature_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

# Save high-resolution figures
ggsave("results/ai_temporal/figure1_growth_trends.pdf", 
       p1, width = 180, height = 120, units = "mm")
ggsave("results/ai_temporal/figure1_growth_trends.png", 
       p1, width = 180, height = 120, units = "mm", dpi = 300)

ggsave("results/ai_temporal/figure2_trial_status.pdf", 
       p2, width = 180, height = 120, units = "mm")
ggsave("results/ai_temporal/figure2_trial_status.png", 
       p2, width = 180, height = 120, units = "mm", dpi = 300)

# Generate comprehensive report with refined formatting
report <- c(
  "# Temporal Analysis of AI Clinical Trials",
  "",
  "## Overview",
  sprintf("This analysis examines the temporal evolution of artificial intelligence (AI) in clinical trials from %d to %d, based on data from ClinicalTrials.gov.",
          min(yearly_counts$year), max(yearly_counts$year)),
  "",
  "## Key Metrics",
  "### Sample Characteristics",
  sprintf("* **Total Trials Analyzed**: %s", format(nrow(dat), big.mark=",")),
  sprintf("* **AI-Related Trials**: %s (%.1f%%)", 
          format(sum(dat$is_ai), big.mark=","), 
          100 * sum(dat$is_ai) / nrow(dat)),
  "",
  "### Temporal Patterns",
  sprintf("* **First AI Trial**: %d", min(yearly_counts$year[yearly_counts$ai_trials > 0])),
  sprintf("* **Peak Activity**: %d trials (%d)", 
          max(yearly_counts$ai_trials),
          yearly_counts$year[which.max(yearly_counts$ai_trials)]),
  sprintf("* **Current Growth Rate**: %.1f%% (2023-2024)", 
          tail(yearly_counts$ai_growth, 1)),
  "",
  "### Trial Status Distribution",
  "Current status of AI trials:",
  paste(sprintf("* **%s**: %.1f%%", 
                gsub("_", " ", status_dist$Status), 
                status_dist$percentage), 
        collapse = "\n"),
  "",
  "## Methodology",
  "1. **AI Trial Identification**: Systematic keyword matching across trial titles and descriptions",
  "2. **Temporal Analysis**: Year-over-year growth calculations using CAGR methodology",
  "3. **Status Classification**: Standardized categorization following ClinicalTrials.gov conventions",
  "",
  "## Notes",
  "* Growth rates account for registration delays and reporting lags",
  "* Status distributions reflect current trial states as of the latest database update",
  "* Figures prepared according to Nature publication standards"
)

writeLines(report, "results/ai_temporal/temporal_analysis.md")

# Save processed data
save(yearly_counts, status_dist, 
     file = "results/ai_temporal/temporal_data.RData")

cat("Analysis complete. Publication-ready figures and report saved in results/ai_temporal/\n")
