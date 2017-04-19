n <- 100000
b <- 100
N <- rnorm(n)
B <- sample(N,b)

mean0 <- function(N,n){
  X <- sample(N, n, replace = TRUE)
  return(mean(X)) 
}

mean1 <- function(B,n){
X <- sample(B, n, replace = TRUE)
return(mean(X))
}

mean2 <- function(B,n,b){
  X <- as.numeric( rmultinom(1,n,rep(1/b,b)) )
  return(sum(X*B)/n)
}

mean0(N,n)
mean1(B,n)
mean2(B,n,b)
microbenchmark::microbenchmark(mean0(N,n),mean1(B,n), mean2(B,n,b))

