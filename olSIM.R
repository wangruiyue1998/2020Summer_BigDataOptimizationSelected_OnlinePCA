olSIM <- function(d, n, m=100){
  ################# Parameters Discription ####################
  # d is row-number of data 
  # n is col-number of data
  # q is number of golden PCs
  #############################################################
  # "olSIM" returns simulation data x and golden PCs Ugo
  
  # Here golden PCs will generate randomly
  #############################################################
  if (missing(d)) 
    d <- 50
  if (missing(n)) 
    n <- 100
  # Missing Setting

  
  A <- matrix(rbinom(d * m, 1, 0.4), nrow = d, ncol = m)
  z <- matrix(rnorm(m*n, mean = 0, sd = 1),nrow = m, ncol = n)
  w <- matrix(rnorm(m*n, mean = 0, sd = 0.05),nrow = d, ncol = n)
  x <- A%*%z + w
  # generate data
  
  for(i in 1:n){
    x[,i] <-scale(x[,i], center = T, scale = T)
  }
  # normalization
  
  return(x)
}