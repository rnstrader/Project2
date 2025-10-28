library(tidyverse)

#importing dataset
bike <- read_csv("SeoulBikeData.csv", locale=locale(encoding="latin1"))

bike <- bike |> mutate(across(where(is.character), as.factor))

#sorting numeric variables
numeric_vars <- names(bike)[sapply(bike, is.numeric)]

#Season Values
SeasVals <- c(
  "1" = "Winter",
  "2" = "Autumn",
  "3" = "Spring",
  "4" = "Summer"
)

#Holiday Values
HolVals <- c(
  "1" = "Holiday",
  "2" = "No Holiday"
)      
