n <- 100000
b <- 100
N <- rnorm(n)
B <- sample(N,b)

mean1 <- function(B,n){
X <- sample(B, n, replace = TRUE)
return(mean(X))
}

mean2 <- function(B,n,b){
  X <- as.numeric( rmultinom(1,n,rep(1/b,b)) )
  return(sum(X*B)/n)
}

mean1(B,n)
mean2(B,n,b)
microbenchmark::microbenchmark(mean1(B,n), mean2(B,n,b))

