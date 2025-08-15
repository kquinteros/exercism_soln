raindrops <- function(number) {
  divisors <- c(3,5,7)
  sounds <- c("Pling","Plang","Plong")
  
  hits <- number %% divisors == 0
  if(any(hits)) {
    return(paste0(sounds[hits], collapse = ""))
  } else {
    return(as.character(number))
  }
}
