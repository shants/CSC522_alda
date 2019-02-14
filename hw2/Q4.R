rm(list = ls())
m <- matrix(c(35.0,15.0,2.5,11.0,10.5,12.5,44.0,11.0,1.5,13.0,48.0,11.0,45.0,13.0,38.0,10.0,7.5,13.5), nrow = 9, ncol = 2, byrow = TRUE)
m
dm <- as.matrix(dist(m))
dm