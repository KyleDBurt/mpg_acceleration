source("R/function.R")
test_that("function_works", {

result <- engine_performance(cylinders = 4, horsepower = 150, weight = 3000)
  
expect_true(is.list(result))
expect_named(result, c("mpg", "acceleration"))

expect_error(engine_performance(cylinders = 4, horsepower = -10, weight = 3000), 
               "Horsepower must be greater than 0")
})
