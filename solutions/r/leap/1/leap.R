leap <- function(year) {
  cond1 = year %% 4 == 0
  cond2 = year %% 100 != 0
  cond3 = year %% 400 == 0
  if( cond1 & cond2 || cond3){
    return(TRUE)
  } 
  else{
    return(FALSE)
  }
}
