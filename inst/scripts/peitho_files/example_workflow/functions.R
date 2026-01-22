library(dplyr)

# FUNCTION: split strings

split_strings <- function(strings, separator) {
  # Initialize results list
  text_results <- list()
  
  # Check if input is a single string and convert to list if needed
  if (is.character(strings) && length(strings) == 1) {
    strings <- list(strings)
  }
  
  # Get the length of the input
  n <- length(strings)
  
  # Loop through each string in the input
  for (i in 1:n) {
    current_string <- strings[[i]]
    
    # Split the string using the separator
    # strsplit handles escape sequences like \n automatically
    split_result <- strsplit(current_string, separator, fixed = FALSE)[[1]]
    
    # Add the split result to our results list
    text_results[[i]] <- split_result
  }
  
  # Set result labeling variable
  result_labeling <- ""
  
  # Return both outputs as a list
  return(list(
    text_results = text_results,
    result_labeling = result_labeling
  ))
}

simple_split <- function(x, split, ...) {
  strsplit(x, split, ...)[[1]]
}