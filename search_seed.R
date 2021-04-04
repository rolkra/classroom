library(tidyverse)
library(tidydice)

max_success = -1

for (i in 1:1000) {
  
  data <- roll_dice(6, agg = TRUE, seed = i)
  success <- data$success[1]
  
  if (success > max_success) {
    max_success <- success
    cat("seed =", i, "-> success =", data$success[1], "\n")
  }

}
