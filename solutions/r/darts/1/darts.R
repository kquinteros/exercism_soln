score <- function(x, y) {
  z = x^2 + y^2
  # Breaks: [0, 1], (1, 25], (25, 100], (100, Inf)
  bin <- .bincode(z, breaks = c(0, 1, 25, 100, Inf),
           right = TRUE, include.lowest = TRUE)
  
  # Map bins: 1 = 10, 2 = 5, 3 = 1, 4 = 0
  points <- c(10, 5, 1, 0)
  
  #return point
  return(points[bin])
}
