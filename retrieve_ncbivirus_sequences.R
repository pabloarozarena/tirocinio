install.packages("httr")
install.packages("xml2")
install.packages("tidyverse")

##library(tidyverse)
library(tidyverse)
ibrary(httr) #enables interaction with the NCBI E-utilities API
library(xml2) #enables to extract info from XML formats

##### SEARCH IN NCBI DATABASE - ESearch #####

search_ncbi <- function(query, db = "nuccore", retmax = 10) 
  {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi"
 
   response <- GET(base_url, query = list(
    db = db,
    term = query,
    retmax = retmax,
    retmode = "xml"
  ))
  
  #extract IDs
  xml <- content(response, as = "parsed")
  ids <- xml_find_all(xml, ".//Id") %>% xml_text()
  return(ids)
}

##### FETCH SEQUENCES INTO FASTA FILES - EFetch #####

fetch_sequences <- function(ids, db = "nuccore", rettype = "fasta") 
  {
  base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
  id_string <- paste(ids, collapse = ",")
  
  response <- GET(base_url, query = list(
    db = db,
    id = id_string,
    rettype = rettype,
    retmode = "text"
  ))
  
  return(content(response, as = "text"))
}

##### HOW TO USE #####

# Define a query for searching a specific organism or condition
query <- "<Your_Query_Here>"  # Replace with a specific query string like "Virus[Organism] AND Lenght[Sequence Length]"

# Use the search_ncbi function to fetch IDs based on the query
ids <- search_ncbi(query, retmax = "<Number_of_Records>")

# Display the IDs found
print(paste("Found IDs:", paste(ids, collapse = ", ")))

# Check if IDs were retrieved before attempting to fetch sequences
if (length(ids) > 0) {
  fasta_data <- fetch_sequences(ids)
  writeLines(fasta_data, "<Your_Output_File_Name.fasta>")
  
  # Notify the user that the sequences were successfully saved
  print("Sequences have been saved to '<Your_Output_File_Name.fasta>'")
} else {
 
   # Notify the user if no IDs were found
  print("No IDs were found for the query.")
}

##### EXAMPLE - INFLUENZA A VIRUS #####

query <- "Influenza A virus[ORGANISM]"
ids <- search_ncbi(query, retmax = 10)

print(paste("Found IDs:", paste(ids, collapse = ", ")))

if (length(ids) > 0) {
  fasta_data <- fetch_sequences(ids)
  writeLines(fasta_data, "influenza_a_virus.fasta")
  print("Sequences have been saved to 'influenza_a_virus.fasta'")
} else {
  print("No IDs were found for the query.")
}

#To avoid short sequences and fetch actual genes or genomes we can filter by sequence length

query <- "Influenza A virus[ORGANISM] AND 8000:20000[Sequence Length]"
ids <- search_ncbi(query, retmax = 10)

print(paste("Found IDs:", paste(ids, collapse = ", ")))

if (length(ids) > 0) {
  fasta_data <- fetch_sequences(ids)
  writeLines(fasta_data, "influenza_a_virus_genomes.fasta")
  print("Sequences have been saved to 'influenza_a_virus_genome.fasta'")
} else {
  print("No IDs were found for the query.")
}
