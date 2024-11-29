# Load libraries
install.packages("seqinr")
library(seqinr)
library(dplyr)

# Function to count Ns in sequences
count_Ns <- function(fasta_file) {
  sequences <- read.fasta(fasta_file)  # Read the FASTA file
  Ns_count <- sapply(sequences, function(seq) sum(toupper(seq) == "N"))  # Count 'N's in each sequence
  return(Ns_count)
}

# Function to categorize sequences based on Ns count
categorize_quality <- function(Ns_count, high_threshold = 5, low_threshold = 100) {
  if (Ns_count <= high_threshold) {
    return("high")
  } else if (Ns_count <= low_threshold) {
    return("moderate")
  } else {
    return("low")
  }
}

# Load all downloaded FASTA files
fasta_files <- list.files(path = "/Users/pablo/Downloads", pattern = "*.fasta", full.names = TRUE)

# Initialize a list to store results
results <- list()

# Loop through each file and process
for (file in fasta_files) {
  Ns_count <- count_Ns(file)  # Get the count of 'N's in each sequence
  sequence_headers <- names(read.fasta(file))  # Get the headers (virus family info should be here)
  
  # Categorize sequences based on Ns count
  quality <- sapply(Ns_count, categorize_quality)
  
  # Extract viral family information from headers 
  family <- gsub(" .*", "", sequence_headers) 
  
  # Combine results into a data frame
  file_results <- data.frame(file = rep(file, length(Ns_count)),
                             family = family,
                             Ns_count = Ns_count,
                             quality = quality)
  
  # Append to results list
  results[[file]] <- file_results
}

# Combine all results into one data frame
all_data <- bind_rows(results)

# Summarize the data by family
family_summary <- all_data %>%
  group_by(family) %>%
  summarise(
    num_sequences = n(),
    mean_N_count = mean(Ns_count),
    quality_range = case_when(
      mean_N_count <= 5 ~ "high",
      mean_N_count <= 100 ~ "moderate",
      TRUE ~ "low"
    )
  )


View(family_summary)

# Filter out families with fewer than 20 sequences
filtered_data <- all_data %>%
  group_by(family) %>%
  filter(n() >= 20)

# For families with a large number of sequences, downsample to match the smallest family
min_family_size <- filtered_data %>%
  group_by(family) %>%
  summarise(num_sequences = n()) %>%
  pull(num_sequences) %>%
  min()

# Downsample the larger families
downsampled_data <- filtered_data %>%
  group_by(family) %>%
  sample_n(min_family_size, replace = FALSE)

family_summary_normalized <- downsampled_data %>%
  group_by(family) %>%
  summarise(
    num_sequences = n(),
    mean_N_count = mean(Ns_count),
    low_quality = sum(quality == "low"),
    moderate_quality = sum(quality == "moderate"),
    high_quality = sum(quality == "high")
  )

View(family_summary_normalized)

family_summary_normalized_numseq <- family_summary %>%
  select(family, num_sequences) %>%
  rename(num_sequences_original = num_sequences)

family_summary_normalized_merged <- family_summary_normalized %>%
  left_join(family_summary_normalized_numseq, by = "family")

View(family_summary_normalized_merged)

# Select the top 20 families with the highest normalized mean N count
top_20_families <- family_summary_normalized_merged %>%
  arrange(desc(mean_N_count)) %>%
  head(20)

# Create a bar plot for the top 20 families
ggplot(top_20_families, aes(x = reorder(family, -mean_N_count), y = mean_N_count)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Families with Highest Mean N Count",
       x = "Family",
       y = "Mean N Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate x-axis labels
  geom_text(aes(label = num_sequences_original), vjust = -0.5)  # Add sequence count as labels above the bars
