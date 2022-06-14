data_sim <- olSIM(d = 50, n = 1000)
x <- data_sim

d = nrow(x)
n = ncol(x)

q <- 2
# number of PC to compute
B <- 20
# block size

Uout <- olPCA(x=x, q=q, B=B, byrow = FALSE)
lambda <- apply(t(x) %*% Uout, 2, var)

# y = Uout %*% x

