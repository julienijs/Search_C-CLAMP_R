# Set your working directory
setwd("./Corpus")

# Get a list of all .txt files in the directory
files <- list.files(pattern = "\\.txt$")

# Initialize a data frame to store results
results_df <- data.frame(file = character(), 
                         left_context = character(), 
                         pattern = character(), 
                         right_context = character(), 
                         stringsAsFactors = FALSE)

# Define the regular expression pattern
pattern <- "\\bwoord\\b"

# Function to extract context
extract_context <- function(content, pattern_position) {
  left_start <- max(1, pattern_position - 100)
  left_context <- paste(content[left_start:pattern_position], collapse = " ")
  
  right_end <- min(length(content), pattern_position + 100)
  right_context <- paste(content[(pattern_position + 1):right_end], collapse = " ")
  
  return(list(left_context = left_context, right_context = right_context))
}

# Loop through each file and search using regex
for (i in seq_along(files)) {
  # Read the file
  content <- readLines(files[i])
  
  # Check if the last line ends with a newline character
  if (!grepl("\\n$", content[length(content)])) {
    # If not, add a newline character
    content[length(content)] <- paste(content[length(content)], "\n")
  }
  
  # Search using regex
  occurrences <- grep(pattern, content, ignore.case = TRUE)
  
  # Process occurrences
  for (position in occurrences) {
    context <- extract_context(content, position)
    result_row <- data.frame(file = files[i], 
                             left_context = context$left_context, 
                             pattern = content[position], 
                             right_context = context$right_context,
                             stringsAsFactors = FALSE)
    results_df <- rbind(results_df, result_row)
  }
}

# Save the results to a CSV file
write.csv(results_df, "results.csv", row.names = FALSE)

