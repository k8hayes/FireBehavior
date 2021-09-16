# Point process models

library(tidyverse)




x <- round(runif(15, 0, 20))
y <- round(runif(15, 0, 20))

plot(x,y)

loc <- cbind(x,y)
loc
