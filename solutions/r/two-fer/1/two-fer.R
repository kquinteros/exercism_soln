two_fer <- function(name) {
 if (missing(name) || name == "") {
    return("One for you, one for me.")
  } else {
    return(paste0("One for ", name, ", one for me."))
  }
}
