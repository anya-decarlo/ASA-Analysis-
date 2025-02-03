library(data.table)
library(tidyverse)
library(igraph)
library(ggraph)

# Set publication-ready theme
nature_theme <- theme_minimal() +
  theme(
    text = element_text(size = 8),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8, face = "bold"),
    plot.title = element_text(size = 9, face = "bold"),
    plot.subtitle = element_text(size = 8, face = "italic"),
    legend.position = "bottom",
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 8),
    plot.margin = margin(10, 15, 10, 10)
  )

# Read and preprocess data
dat <- fread("/Users/anyadecarlo/ctg_studies.csv")

# Identify AI trials more precisely
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
      (grepl(keyword, `Study Title`, ignore.case = TRUE) |
       grepl(keyword, `Brief Summary`, ignore.case = TRUE)) &
      !grepl("not using artificial intelligence|non-artificial intelligence|without artificial intelligence",
             `Brief Summary`, ignore.case = TRUE)]
}

# Function to clean institution names
clean_institution <- function(name) {
  if(is.na(name)) return(NA)
  # Remove common suffixes and standardize names
  name <- gsub(" University Hospital", " University", name, ignore.case = TRUE)
  name <- gsub(" Medical Center", " Hospital", name, ignore.case = TRUE)
  name <- gsub(" Medical School", " University", name, ignore.case = TRUE)
  name <- gsub("\\s+", " ", name) # Remove extra spaces
  name <- trimws(name)
  return(name)
}

# Function to extract institutions from locations
extract_institutions <- function(locations) {
  if(all(is.na(locations))) return(character(0))
  # Split multiple locations
  all_locations <- unlist(strsplit(locations, "\\|"))
  # Extract institution names (assuming they're before the comma)
  institutions <- str_extract(all_locations, "^[^,]+")
  institutions <- institutions[!is.na(institutions)]
  # Clean institution names
  institutions <- sapply(institutions, clean_institution)
  institutions <- institutions[!is.na(institutions)]
  return(unique(institutions))
}

# Process locations and create network data
cat("Processing institution data...\n")
network_data <- dat[is_ai == TRUE, .(
  institutions = list(extract_institutions(`Locations`)),
  country = list(str_extract(`Locations`, "[^,]+$")),
  study_id = `NCT Number`
)]

# Create edges more efficiently
cat("Creating collaboration network...\n")
edges_list <- list()
for(i in 1:nrow(network_data)) {
  insts <- unlist(network_data$institutions[i])
  if(length(insts) > 1) {
    # Only create edges for institutions that appear in multiple studies
    combs <- combn(unique(insts), 2)
    edges_list[[i]] <- data.table(
      from = combs[1,],
      to = combs[2,],
      study_id = network_data$study_id[i]
    )
  }
  if(i %% 100 == 0) cat(sprintf("Processed %d/%d trials\n", i, nrow(network_data)))
}

# Combine edges and count frequencies
edges <- rbindlist(edges_list)
edges <- edges[, .N, by = .(from, to)]
setnames(edges, "N", "weight")

# Filter to keep only edges with sufficient weight
edges <- edges[weight >= 2]  # Keep only institutions that collaborated at least twice

# Create nodes with metrics
nodes <- data.table(
  institution = unique(c(edges$from, edges$to))
)[, `:=`(
  degree = .N
), by = institution]

# Create igraph object
cat("Calculating network metrics...\n")
g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Calculate network metrics
V(g)$betweenness <- betweenness(g, normalized = TRUE)
V(g)$eigenvector <- eigen_centrality(g)$vector

# Create collaboration network visualization
cat("Creating visualization...\n")
set.seed(42)  # For reproducible layout
p1 <- ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.2) +
  geom_node_point(aes(size = degree, color = betweenness)) +
  scale_edge_width(range = c(0.2, 2)) +
  scale_size_continuous(range = c(1, 8)) +
  scale_color_viridis_c() +
  labs(
    title = "AI Clinical Trial Collaboration Network",
    subtitle = "Institution collaboration patterns (minimum 2 joint trials)",
    size = "Number of Collaborations",
    color = "Betweenness Centrality",
    edge_width = "Joint Trials"
  ) +
  nature_theme +
  theme(legend.box = "vertical")

# Create results directory
dir.create("results/network_analysis", recursive = TRUE, showWarnings = FALSE)

# Save visualization
ggsave("results/network_analysis/collaboration_network.pdf",
       p1, width = 180, height = 180, units = "mm")
ggsave("results/network_analysis/collaboration_network.png",
       p1, width = 180, height = 180, units = "mm", dpi = 300)

# Generate top research hubs report
top_hubs <- nodes[order(-degree)][1:20]

# Create country-level summary
country_summary <- network_data[, .(
  trials = .N,
  institutions = length(unique(unlist(institutions)))
), by = .(country = unlist(country))][order(-trials)]

# Calculate additional network metrics
avg_path_length <- mean_distance(g)
clustering_coef <- transitivity(g)
components <- components(g)
largest_component <- max(components$csize)

report <- c(
  "# Network Analysis of AI Clinical Trials",
  "",
  "## Overview",
  sprintf("Analysis of %d institutions across %d AI clinical trials",
          nrow(nodes), nrow(dat[is_ai == TRUE])),
  "",
  "## Network Statistics",
  sprintf("* **Total Institutions**: %d", nrow(nodes)),
  sprintf("* **Total Collaborations**: %d", nrow(edges)),
  sprintf("* **Average Path Length**: %.2f", avg_path_length),
  sprintf("* **Clustering Coefficient**: %.3f", clustering_coef),
  sprintf("* **Number of Components**: %d", components$no),
  sprintf("* **Largest Component Size**: %d institutions", largest_component),
  "",
  "## Key Research Hubs",
  "### Top 20 Institutions by Number of Collaborations",
  paste(sprintf("%d. **%s** (%.0f collaborations, betweenness=%.3f)",
                1:20,
                top_hubs$institution,
                top_hubs$degree,
                V(g)$betweenness[match(top_hubs$institution, V(g)$name)]),
        collapse = "\n"),
  "",
  "## Geographical Distribution",
  "### Top 10 Countries by Trial Count",
  paste(sprintf("* **%s**: %d trials (%d institutions)",
                country_summary$country[1:10],
                country_summary$trials[1:10],
                country_summary$institutions[1:10]),
        collapse = "\n"),
  "",
  "## Methodology",
  "1. Network analysis based on shared trial participation (minimum 2 joint trials)",
  "2. Institution names standardized to account for variations",
  "3. Centrality measures include degree and betweenness centrality",
  "",
  "## Notes",
  "* Only collaborations with at least two joint trials are included",
  "* Institution names were standardized to account for variations in naming",
  "* Some institutions may be underrepresented due to incomplete location data",
  sprintf("* Analysis date: %s", format(Sys.Date(), "%B %d, %Y"))
)

writeLines(report, "results/network_analysis/network_analysis.md")

# Save processed data
save(edges, nodes, country_summary,
     file = "results/network_analysis/network_data.RData")

cat("Network analysis complete. Results saved in results/network_analysis/\n")
