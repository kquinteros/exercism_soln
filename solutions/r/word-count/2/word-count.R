word_count <- function(input)
  regmatches(tolower(input), gregexpr("[a-z]|[a-z]+'?[a-z]+|[0-9]+", input, ignore.case=T)) %>% table %>% as.list
