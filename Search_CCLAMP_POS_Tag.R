# read metadata file
metadata <- read.delim("C-CLAMP_metadata_gender.txt", header = TRUE, sep = "\t", fill = FALSE)

# Set your working directory
setwd("./Corpus_Tagged")

# Define the pattern you want to search
pattern <- "\\been<[^>]+> klein(e)*<[^>]+>"
#pattern <- "Als<als;VG\\(onder\\);0.997449>"

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
  
  # Initialize a data frame to store results
  results <- data.frame(File = character(0), 
                        Left_Context = character(0), 
                        Pattern = character(0), 
                        Right_Context = character(0), 
                        stringsAsFactors = FALSE
  )
  
  # Find positions of the pattern
  matches <- gregexpr(pattern, text, fixed = FALSE)[[1]]
  print(matches)
  for (match_start in matches) {
    if (match_start < 1) {
      next  # Skip if pattern is not found
    }
    match_end <- match_start + nchar(pattern) - 1
    print(nchar(pattern))
    
    # Adjust left context bounds
    left_context_start <- max(1, match_start - 1000)
    left_context_end <- match_start - 1
    
    # Adjust right context bounds
    right_context_start <- match_end + 1
    right_context_end <- min(match_end + 1000, nchar(text))
    
    pattern_match <- substr(text, match_start, match_end)
    left_context <- substr(text, left_context_start, left_context_end)
    right_context <- substr(text, right_context_start, right_context_end)
    
    
    # Remove tags from left and right context
    left_context <- gsub("<[^>]+>", "", left_context)
    right_context <- gsub("<[^>]+>", "", right_context)
    
    result <- data.frame(File = gsub("_pos.txt", "", file_path), 
                         Left_Context = left_context, 
                         Pattern = pattern_match, 
                         Right_Context = right_context, 
                         stringsAsFactors = FALSE)
    
    results <- rbind(results, result)
  }
  
  return(results)
}

# List all .txt files in the directory
files <- list.files(pattern = "\\.txt$")

# Initialize a data frame to store all results
all_results <- data.frame(File = character(0), 
                          Left_Context = character(0), 
                          Pattern = character(0), 
                          Right_Context = character(0), 
                          stringsAsFactors = FALSE)

# Loop through each file and extract results
for (file in files) {
  results <- extract_context(file, pattern)
  all_results <- rbind(all_results, results)
}


# merge metadata with results

all_results <- merge(all_results, metadata, by="File")


# Write the results to an Excel file
write.xlsx(all_results, "output.xlsx", rowNames = FALSE, encoding = "UTF-8")
