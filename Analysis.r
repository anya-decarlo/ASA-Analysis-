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

#Subsetting by Study Type observational or interventional
jf <- dat[Study_Type %in% c("OBSERVATIONAL")]
kf <- dat[Study_Type %in% c("INITERVENTIONAL")]

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

#Subsetting Studies by most common words used in Study Titles
# Load required libraries
library(data.table)
library(tidytext)
library(dplyr)

# Tokenize the text (split titles into words)
words <- dat %>%
  select(Study_Title) %>%          # Select the Study_Title column
  unnest_tokens(word, Study_Title) # Tokenize the titles into individual words

# Remove common stop words (like 'the', 'and', etc.)
data("stop_words")                 # Load a predefined list of stop words
clean_words <- words %>%
  anti_join(stop_words, by = "word")

# Count the 20 most common words
top_words <- clean_words %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 100)

# View the result
print(top_words)


# Most Common words used in Study_Design
# Tokenize the text (split titles into words)
words <- dat %>%
  select(Study_Design) %>%          # Select the Study_Title column
  unnest_tokens(word, Study_Design) # Tokenize the titles into individual words

# Remove common stop words (like 'the', 'and', etc.)
data("stop_words")                 # Load a predefined list of stop words
clean_words <- words %>%
  anti_join(stop_words, by = "word")

# Count the 20 most common words
top_words <- clean_words %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 100)

# View the result
print(top_words)

#######################################################################
#######################################################################


# Tokenize the text into bigrams (two-word phrases)
bigrams <- dat  %>%
  select(Study_Design) %>%
  unnest_tokens(bigram, Study_Design, token = "ngrams", n = 2)

# Count the most common bigrams
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(bigram_counts)

# Optional: For trigrams (three-word phrases)
trigrams <- dat %>%
  select(Study_Design) %>%
  unnest_tokens(trigram, Study_Design, token = "ngrams", n = 3)

# Count the most common trigrams
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)

################################
###############################
bigrams <- jf  %>%
  select(Study_Design) %>%
  unnest_tokens(bigram, Study_Design, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 20)

print(bigram_counts)

trigrams <- jf %>%
  select(Study_Design) %>%
  unnest_tokens(trigram, Study_Design, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)


##############################
Conditions
#############################
bigrams <- mf  %>%
  select(Conditions) %>%
  unnest_tokens(bigram, Conditions, token = "ngrams", n = 2)
bigram_counts <- bigrams %>%
  count(bigram, sort = TRUE) %>%
  slice_max(n, n = 20)

print(bigram_counts)

trigrams <- jf %>%
  select(Conditions) %>%
  unnest_tokens(trigram, Conditions, token = "ngrams", n = 5)
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE) %>%
  slice_max(n, n = 20)

# View the result
print(trigram_counts)
