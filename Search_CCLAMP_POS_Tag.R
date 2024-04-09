# read metadata file
metadata <- read.delim("C-CLAMP_metadata_gender.txt", header = TRUE, sep = "\t", fill = FALSE)

# Set your working directory
#setwd("./corpus_article_pos_flatversion")
setwd("./Corpus_tagged")

# Define the pattern you want to search
pattern <- "\\b(?i)ik?\\[[^,]+, VNW[^\\]]+\\]"

# Install necessary packages if not already installed
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

# Load required libraries
library(stringr)
library(openxlsx)

extract_context <- function(file_path, pattern){
  # Read the file with UTF-8 encoding
  text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  
  # Join the lines into a single string with spaces
  text <- paste(text, collapse = " ")
  
  # Find positions of all matches
  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
  
  # Check if there are no matches, and skip processing if so
  if (length(matches) == 0 || all(matches == -1)) {
    return(NULL)
  }
  
  # Initialize a data frame to store results
  results <- data.frame(File = character(0), 
                        Left_Context = character(0), 
                        Pattern = character(0), 
                        Right_Context = character(0), 
                        stringsAsFactors = FALSE
  )
  
  for (match_start in matches) {
    match_length <- attr(matches, "match.length")
    match_end <- match_start + match_length - 1
    
    left_context_start <- max(1, match_start - 1000)
    left_context_end <- match_start - 1
    
    right_context_start <- match_end + 1
    right_context_end <- min(match_end + 1000, nchar(text))
    
    pattern_match <- substr(text, match_start, match_end)
    left_context <- substr(text, left_context_start, left_context_end)
    right_context <- substr(text, right_context_start, right_context_end)
    
    left_context <- gsub("\\[.*?\\]", "", left_context)
    right_context <- gsub("\\[.*?\\]", "", right_context)
    
    result <- data.frame(File = gsub(".txt", "", file_path), 
                         Left_Context = left_context, 
                         Pattern = pattern_match, 
                         Right_Context = right_context, 
                         stringsAsFactors = FALSE)
    
    results <- rbind(results, result)
  }
  
  return(results)
}

# List all .txt files in the directory
files <- list.files(pattern = "\\.txt")

# Initialize a data frame to store all results
all_results <- data.frame(File = character(0), 
                          Left_Context = character(0), 
                          Pattern = character(0), 
                          Right_Context = character(0), 
                          stringsAsFactors = FALSE)

# Loop through each file and extract results
for (file in files) {
  print(file)
  results <- extract_context(file, pattern)
  all_results <- rbind(all_results, results)
}


# merge metadata with results

all_results <- merge(all_results, metadata, by="File")


# Write the results to an Excel file
write.xlsx(all_results, "output.xlsx", rowNames = FALSE, encoding = "UTF-8")
write.csv(all_results, "output.txt")
