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



# Define the words to exclude
excluded_words <- c("by", "of", "the", "number", "to", "a", "on", "will", "be")

# Tokenize into bigrams
bigrams <- mf %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(bigram, Primary_Outcome_Measures, token = "ngrams", n = 2)

# Filter out bigrams containing the excluded words
bigram_counts <- bigrams %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%  # Split bigrams into two columns
  filter(!word1 %in% excluded_words, !word2 %in% excluded_words) %>%  # Exclude rows with unwanted words
  unite(bigram, word1, word2, sep = " ") %>%  # Combine words back into bigrams
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 10)

# Print the filtered bigram counts
print(bigram_counts)

# Prepare the data for visualization
bigram_counts_clean <- bigram_counts %>%
  filter(!is.na(bigram))  # Remove NA values if present

# Create the bar chart
bigram_plot <- ggplot(bigram_counts_clean, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  labs(
    title = "Bigram Frequency in Primary Outcome Measures",
    x = "Bigram",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot as PNG
ggsave("bigram_outcomes_filtered.png", plot = bigram_plot, width = 10, height = 6, dpi = 300)



# Extract top 10 trigrams from Primary_Outcome_Measures
trigrams <- mf %>%
  select(Primary_Outcome_Measures) %>%
  unnest_tokens(trigram, Primary_Outcome_Measures, token = "ngrams", n = 10)  # n=3 for trigrams

trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 10)  # Select the top 10 trigrams

# View the result
print(trigram_counts)


# Load necessary libraries
library(data.table)
library(dplyr)
library(stringr)

# Define the desired words to search for in titles
search_words <- c("cardiac", "biomarkers" "")

# Count how many titles match each word
word_counts <- lapply(search_words, function(word) {
  count <- sum(str_detect(mf$Primary_Outcome_Meaures, fixed(word, ignore_case = TRUE)))
  data.frame(word = word, count = count)
})

# Combine results into a single data frame
word_counts_df <- bind_rows(word_counts)

# Sort the results by count in descending order
word_counts_df <- word_counts_df %>%
  arrange(desc(count))

# Print the results
print(word_counts_df)

# Create a bar plot to visualize the word frequencies
library(ggplot2)

word_plot <- ggplot(word_counts_df, aes(x = reorder(word, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Frequency of Words in Study Titles",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Save the plot
ggsave("word_frequency_in_titles.png", plot = word_plot, width = 10, height = 6, dpi = 300)