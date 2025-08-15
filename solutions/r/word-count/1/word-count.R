word_count <- function(input) {
  # Step 1: split the statement into separate words (space or comma)
  words <- strsplit(input, "[ ,\n]+")[[1]]  
  # Step 2: convert to lowercase
  words <- tolower(words)
  
  # Step 3: remove all non-alphanumeric, non-space, non-apostrophe characters
  words <- gsub("[^A-Za-z0-9\\s']", "", words, perl = TRUE)
  
  # Step 4: remove apostrophes not between letters
  words <- gsub("^'+|'+$", "", words)
  
  # Step 6: remove any empty strings created by cleaning
  words <- words[words != ""]
  
  # Step 7: count occurrences
  count <- table(words)
  
  # Return as list
  return(as.list(count))
}
