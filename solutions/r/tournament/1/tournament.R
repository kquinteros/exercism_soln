#libraries
library(dplyr)
library(tibble)
library(stringr)

# this function take the input with errors in lines and corrects them.
# then outputs the results.
# bad instructions on the exercism problem.
# instructions don't say to ignore lines with errors

tournament <- function(input) {
  
  # Remove empty lines
  input <- str_trim(input)
  input <- input[input != ""]
  
  #drop line with errors 
  input <- input[!grepl("[^a-zA-Z0-9 ;]+", input)]
  input <- input[sapply(gregexpr(";", input), function(m) sum(m > 0) == 2)]
  input <- input[grepl("\\b(win|loss|draw)\\b", input, ignore.case = TRUE)]
  
  #edit lines
  input <- gsub(";[wW][^;]*$", ";win,loss", input, perl = TRUE) %>%
    gsub(";[lL][^;]*$", ";loss,win", ., perl = TRUE) %>%
    gsub(";[dD][^;]*$", ";draw,draw", ., perl = TRUE) 
  
  # Split each line and take only first 3 fields
  input <- strsplit(input, ";")
  input <- lapply(input, function(x) {
    if(length(x) < 3) stop("Each line must have at least 3 fields: team1;team2;results")
    x[1:3]
  }) %>%
    unlist()
  
  # Separate Team and results
  indices <- seq(3, length(input), by = 3)
  Team <- input[-indices]
  results <- unlist(strsplit(input[indices], split = ","))
  
  # Create tibble
  tally <- tibble(Team, results)
  
  #Calculate stats
  tally <- tally %>%
    group_by(Team) %>%
    summarise(
      MP = n(),
      W  = sum(results == "win"),
      D  = sum(results == "draw"),
      L  = sum(results == "loss"),
      P  = W*3 + D*1
    ) %>%
    arrange(desc(P))
  
  #return dataframe
  as.data.frame(tally)
 
}
