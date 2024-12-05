# Load the data.table library
library(data.table)

# identify source of data 
source("~/ASA-Analysis-/preprocess_Data.R")

# read data into data table 
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


# Study Enrollment Numbers across each Study_Type  
# Summarize enrollment numbers by Study_Type
enrollment_summary <- mf[, .(
  Total_Enrollment = sum(Enrollment, na.rm = TRUE),
  Average_Enrollment = mean(Enrollment, na.rm = TRUE),
  Count = .N  # Number of studies in each type
), by = Study_Type]


##############################
#Conditions
#############################

bigrams <- mf  %>%
  select(Conditions) %>%
  unnest_tokens(bigram, Conditions, token = "ngrams", n = 1)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

bigrams <- mf  %>%
  select(Conditions) %>%
  unnest_tokens(bigram, Conditions, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 20)

print(bigram_counts)

trigrams <- mf %>%
  select(Conditions) %>%
  unnest_tokens(trigram, Conditions, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)



##############################
#Interventions
#############################

bigrams <- mf  %>%
  select(Interventions) %>%
  unnest_tokens(bigram, Interventions, token = "ngrams", n = 1)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

bigrams <- mf  %>%
  select(Interventions) %>%
  unnest_tokens(bigram, Interventions, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

trigrams <- mf %>%
  select(Interventions) %>%
  unnest_tokens(trigram, Interventions, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)


##############################
##Primary Outcome Measures
#############################

bigrams <- mf  %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(bigram, Primary_Outcome_Measures, token = "ngrams", n = 1)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)

bigrams <- mf  %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(bigram, Primary_Outcome_Measures, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

print(bigram_counts)



trigrams <- mf %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(trigram, Primary_Outcome_Measures, token = "ngrams", n = 3)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)


trigrams <- mf %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(trigram, Primary_Outcome_Measures, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)























# View the summary
print(enrollment_summary)
palette_ <- c("#43A4AC", "#FA9860")
palette_a <- adjustcolor(palette_, 0.666)
p <- ggplot(mf, aes(x = Enrollment)) + 
    geom_density(color = palette_[1], fill = palette_a[1])
ggsave("density_plot_enrollment.png", plot = p, width = 8, height = 6, dpi = 300)


#Subset the data to only include studies that are valid (mix active and complete)
# Define the statuses to exclude
excluded_statuses_valid <- c("terminated", "completed", "withdrawn", "unknown", "suspended")

# Subset the data to only include studies that are active 
nf<- lf[!(Study_Status %in% excluded_statuses_valid)]

