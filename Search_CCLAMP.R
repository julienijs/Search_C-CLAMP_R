# Install necessary packages if not already installed
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")

# Load required libraries
library(stringr)
library(openxlsx)

# Function to extract patterns and contexts
extract_context <- function(file_path, pattern){
  # Read the file
  text <- tolower(readLines(file_path))
  
  # Find positions of the pattern
  matches <- str_locate_all(text, pattern, ignore_case = TRUE)[[1]]
  
  # Initialize a data frame to store results
  results <- data.frame(Filename = character(0), 
                        Left_Context = character(0), 
                        Pattern = character(0), 
                        Right_Context = character(0), 
                        stringsAsFactors = FALSE)
  
  # Iterate through matches
  for (match in matches) {
    start <- match[1]
    end <- match[2]
    
    left_context_start <- max(1, start - 10)
    left_context_end <- start - 1
    
    right_context_start <- end + 1
    right_context_end <- min(length(text), end + 10)
    
    left_context <- paste(text[left_context_start:left_context_end], collapse = " ")
    right_context <- paste(text[right_context_start:right_context_end], collapse = " ")
    
    result <- data.frame(Filename = file_path, 
                         Left_Context = left_context, 
                         Pattern = substr(text[start:end], start - left_context_start + 1, end - left_context_start + 1), 
                         Right_Context = right_context, 
                         stringsAsFactors = FALSE)
    
    results <- rbind(results, result)
  }
  
  return(results)
}

# Define the pattern you want to search
pattern <- "woord"

# Define the directory containing your .txt files
setwd("./Datasets")

# List all .txt files in the directory
files <- list.files(pattern = "\\.txt$", full.names = TRUE)

# Initialize a data frame to store all results
all_results <- data.frame(Filename = character(0), 
                          Left_Context = character(0), 
                          Pattern = character(0), 
                          Right_Context = character(0), 
                          stringsAsFactors = FALSE)

# Loop through each file and extract results
for (file in files) {
  results <- extract_context(file, pattern)
  all_results <- rbind(all_results, results)
}

# Write the results to an Excel file
write.xlsx(all_results, "output.xlsx", row.names = FALSE)


