library(tidyverse)

#importing dataset
bike <- read_csv("SeoulBikeData.csv", locale=locale(encoding="latin1"))

#sorting categorical and numeric vectors
numeric_vars <- names(bike)[sapply(bike, is.numeric)]
categorical_vars <- names(bike)[sapply(bike, is.character)]

#Season Values
SeasVals <- c( #"Seasons",
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
