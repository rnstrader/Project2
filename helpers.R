library(tidyverse)

#importing dataset
bike <- read_csv("SeoulBikeData.csv", locale=locale(encoding="latin1"))

#sorting categorical and numeric vectors
numeric_vars <- names(bike)[sapply(bike, is.numeric)]
categorical_vars <- names(bike)[sapply(bike, is.character)]
