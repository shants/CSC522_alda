#######################
# ALDA: hw0.R
# Instructor: Dr. Thomas Price
# 
# Group 15 
# Darpan Dodiya - dpdodiya
# Shantanu Sharma - ssharm34
# Shrijeet Joshi - sjoshi22	
#
#########################

require(ggplot2)
set.seed(123)

intro_to_r <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: a  list:  [generated vector (type: double), mean of vector (type: double), median of the vector (type: double), max value of vector (type: double), min value of the vector (type: double)]
  
  # create random vector 
  new_vector <- c(rnorm(n=num_values))
  
  # calculate required values
  c_mean <- mean(new_vector)
  c_meadian <- median(new_vector)
  c_max <- max(new_vector)
  c_min <- min(new_vector)
  
  # create and return list
  output <- list(new_vector=new_vector, new_mean=c_mean, new_median=c_meadian, new_max=c_max, new_min=c_min)
  return (output)
}


intro_to_plotting <- function(num_values){
  # input: num_values type: integer, specifies the sample size of the random vector you are to generate.
  # output: two plots (saved to disk, no return value), descriptions for which have been provided in the hw0 document. 
  
  # create random vector
  new_vector <- c(rnorm(n=num_values))

  # create and save first plot  
  pdf("G15_plot01.pdf")
  d1 <- data.frame(a=new_vector, b=new_vector)
  p1 <- ggplot(d1, aes(x=a, y=b)) + geom_point()
  print(p1)
  dev.off()
  
  # create and save second plot
  pdf("G15_plot02.pdf")
  d2 <- data.frame(a=new_vector, b=new_vector * new_vector)
  p2 <- ggplot(d2, aes(x=a, y=b)) + geom_point()
  print(p2)
  dev.off()
}