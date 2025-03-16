
#' Engine Performance Estimation
#' 
#' This function estimates fuel efficiency (MPG) and acceleration (0-60 mph time)
#' based on engine cylinders, horsepower, and vehicle weight.
#' 
#' @param cylinders Number of engine cylinders (2, 4, 6, or 8)
#' @param horsepower Horsepower of the engine
#' @param weight Vehicle weight (lbs)
#' @param 500 constant value for scaling mpg range
#' @param 1000 constant value for scaling weight
#' @param 2 constant value for scaling acceleration
#' 
#' @return A list with estimated MPG and acceleration time

engine_performance <- function(cylinders, horsepower, weight) {
  
  # Exit the function prematurely based on invalid inputs
  if (any(horsepower < 0)) {
    stop("Horsepower must be greater than 0")
  }
  
  # It is known that vehicle weight greater than 10,000 lbs is very unlikely 
  # so if we get that something is probably wrong so we warn the user 
  # (but we let the function continue just in case)
  if (any(weight > 10000)) {
    warning("Unrealistically high vehicle weight (kg)")
  }
  
  # Fuel efficiency calculation
  mpg = 500 / (cylinders * horsepower * (weight / 1000))
  
  # Acceleration calculation
  acceleration = (weight * cylinders) / (horsepower * 2)
  
  return(mpg, acceleration)
}
