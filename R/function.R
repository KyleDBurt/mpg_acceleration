
#' Engine Performance Estimation
#' 
#' This function estimates fuel efficiency (MPG) and acceleration (0-60 mph time)
#' based on engine cylinders, horsepower, and vehicle weight.
#' 
#' @param cylinders Number of engine cylinders (2, 4, 6, or 8)
#' @param horsepower Horsepower of the engine
#' @param weight Vehicle weight (lbs)
#' 
#' @return A list with estimated MPG and acceleration time

engine_performance <- function(cylinders, horsepower, weight) {
  
  if (any(horsepower < 0)) {
    stop("Horsepower must be greater than 0")
  }
  
  if (any(weight > 10000)) {
    warning("Unrealistically high vehicle weight (lbs)")
  }
  
  mpg_value <- 500 / (cylinders * horsepower * (weight / 1000))
  acceleration <- (weight * cylinders) / (horsepower * 2)
  
  return(list(mpg = mpg_value, acceleration = acceleration))
}
