anagram <- function(subject, candidates) {
  # Helper: sort letters of a word
  sort_letters <- function(word) {
    paste0(sort(strsplit(word, "")[[1]]), collapse = "")
  }
  
  # Lowercase the subject for sorting
  subject_lower <- tolower(subject)
  subject_sorted <- sort_letters(subject_lower)
  
  # Exclude candidates that are the same word (ignoring case)
  candidates_filtered <- candidates[tolower(candidates) != subject_lower]
  
  # Check candidates
  matches <- sapply(candidates_filtered, function(word) {
    tolower(sort_letters(word)) == subject_sorted
  })
  
  # Return matching anagrams or empty character vector if none
  # Always return a character vector
  if (any(matches)) {
    candidates_filtered[matches]
  } else {
    return(NULL)  # ensures type is character
  }

}