rotate <- function(text, key, alphabet = letters) {
  #' Rotate (Caesar cipher) a string by a specified key
  #'
  #' This function applies a Caesar cipher to a text string, shifting
  #' letters in the specified alphabet by a given key. Letter case
  #' is preserved, and non-letter characters (spaces, punctuation)
  #' are left unchanged.
  #'
  #' @param text Character. The input string to be encoded.
  #' @param key Integer. Number of positions to shift the letters.
  #'            Positive shifts rotate forward; negative shifts rotate backward.
  #' @param alphabet Character vector. The set of letters to use for the cipher.
  #'                 Default is `letters` (a-z).
  #'
  #' @return Character. The encoded text string with the same length as input.
  #'
  #' @examples
  #' rotate("Hello World!", 3)
  #' output: "Khoor Zruog!"
  
  # Number of letters in the alphabet
  n <- length(alphabet)
  
  # Generate the cipher mapping
  cipher <- setNames(alphabet[((1:n + key - 1) %% n) + 1], alphabet[1:n])
  
  # Find all letters in the text (case-insensitive)
  matches <- gregexpr("[A-Za-z]", text)
  chars <- regmatches(text, matches)[[1]]
  
  # Vectorized replacement with case preservation
  lower <- tolower(chars)          # convert letters to lowercase for lookup
  out <- cipher[lower]             # map letters using cipher
  out[chars != lower] <- toupper(out[chars != lower]) # preserve original case
  
  # Put replaced letters back into the text
  regmatches(text, matches) <- list(out)
  
  return(text)
}