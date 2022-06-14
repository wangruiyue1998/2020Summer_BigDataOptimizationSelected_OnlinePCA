olPCA<-function (x, q, U, B, center, byrow = FALSE) {
  ################# Parameters Discription ####################
  # x is dataframe
  # q is PCs' number to compute
  # U is matrix of initial PCs in columns (optional)
  # B is size of block updates (optional)
  # center is centralization(TRUE/FALSE,optional)
  # byrow is x stored in rows (TRUE) or columns (FALSE)
  #############################################################
  # "olPCA" return PCs in columns
  
  # Note1: The default value of B is floor(n/nblock),n the number ofvectors in x,
  # d the number of variables, and nblock = ceiling(log(d)) the number of blocks.
  
  # Note2: If U is specified, q defaults to ncol(U); otherwise the initial PCs 
  # are computed from the first block of data and q must be specified explicitly.
  
  # Note3:The function does not give eigenvalues, they can easily be estimated 
  # by computing the variance of the data along the PCs.
  #############################################################
  
  if (!is.matrix(x)) 
    x <- as.matrix(x)
  # check data type
  
  if (byrow == TRUE) 
    x <- t(x)
  # select data margin(SET DATA BY COLUME)
  
  if (!missing(center)) 
    x <- x - center
  # select centralization
  
  n <- ncol(x)
  d <- nrow(x)
  # load dimension
  
  if (missing(q) && !missing(U)) 
    q <- NCOL(U)
  # take Note2
  
  if (missing(U)) 
    U <- qr.Q(qr(matrix(rnorm(d * q), d, q)))
  # qr() computes the QR decomposition of a matrix
  # qr.Q()returns the Q matrix
  
  stopifnot(NROW(U) == d)
  
  if (missing(B)) {
    nblock <- ceiling(log(d))
    B <- floor(n/nblock)
  }
  else {
    nblock <- n%/%B
  }
  # take Note1
  
  for (i in 1:nblock) {
    ind <- seq.int((i - 1) * B + 1, i * B)
    S <- x[, ind] %*% crossprod(x[, ind], U)
    U <- qr.Q(qr(S/B))
  }
  # take Note3
  
  return(U)
}