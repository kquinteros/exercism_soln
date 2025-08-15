# This is a stub function to take two strings
# and calculate the hamming distance
hamming <- function(strand1, strand2) {
  
  #check that input class
  stopifnot(
    "'strand1' must be character" = is.character(strand1),
    "'strand2' must be character" = is.character(strand2)
  )

  #vectorize
  s1_chars <- strsplit(strand1, "")[[1]]
  s2_chars <- strsplit(strand2, "")[[1]]
  
  #check that input class
stopifnot(
      "ERROR: DNA sequences are not equal length" = length(s1_chars) == length(s2_chars)
  )
  
  #return Hamming distance
  return(length(which(s1_chars != s2_chars)))
    
}
