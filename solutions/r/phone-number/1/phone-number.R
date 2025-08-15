parse_phone_number <- function(number_string) {
  
  #remove spaces and non-digit characters
  number_string <- gsub("[^0-9]", "", number_string)
  
  if(nchar(number_string) == 11 & grepl("^[1][2-9].{2}[2-9]", number_string) ){
    
    #format number without country code
    formatted_number_string <- gsub("^[1]","", number_string) 
    
    # return formatted number
    return(formatted_number_string)
  }
  else if (nchar(number_string) == 10 & grepl("^[2-9].{2}[2-9]", number_string)){
    
    #if conditions are true, then the number should be correctly formatted.
    return(number_string)
  }
  else(
    #number is not valid 
    message("ERROR: invalid number")
  )
}
