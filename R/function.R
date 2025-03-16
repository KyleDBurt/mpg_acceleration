
#' Engine Performance Estimator
#' 
#' This function estimates fuel efficiency (MPG) and acceleration (0-60 mph time)
#' based on engine cylinders, horsepower, and vehicle weight.
#' 
#' @param cylinders Integer (2, 4, 6, or 8) - Number of engine cylinders
#' @param horsepower Horsepower of the engine
#' @param weight Vehicle weight (kg)
#' 
#' @return A list with estimated MPG and acceleration time

engine_performance <- function(cylinders, horsepower, weight) {
  
  # Exit the function prematurely based on invalid inputs
  if (any(horsepower < 0)) {
    stop("Horsepower must be greater than 0")
  }
  
  # It is known that vehicle weight greater than 100,000 kilograms is very unlikely 
  # so if we get that something is probably wrong so we warn the user 
  # (but we let the function continue just in case)
  if (any(weight > 100000)) {
    warning("Unrealistically high vehicle weight (kg)")
  }
  
  # Fuel Efficiency Calculation
  mpg <- 500 / (cylinders * horsepower * (weight / 1000))
  
  # Acceleration Calculation
  acceleration <- (weight * cylinders) / (horsepower * 2)
  
  return(list(MPG = round(mpg, 2), Acceleration = round(acceleration, 2)))
}
