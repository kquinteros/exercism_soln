# this is a stub function that takes a natural_number
# and should return the difference-of-squares as described
# in the README.md
difference_of_squares <- function(natural_number) {
  n <- natural_number
  
  square_of_sums <- ((n * (n + 1)) / 2)^2
  sum_of_squares <- (n * (n + 1) * (2 * n + 1)) / 6
  
  square_of_sums - sum_of_squares
}