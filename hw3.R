###(2)
#(c)

Rg <- function(n = 1, theta = 1){
  
  f<-function(x,a){
    x^(a-1) * exp(-x)
  }
  
  c1 <- integrate(f, a = theta, lower = 0, upper = Inf)[[1]]
  c2 <- integrate(f, a = theta+1/2, lower = 0, upper = Inf)[[1]]
  C <- 2*c1/(2*c1+c2)
  
  U <- runif(n)
  index <- ( U < C )
  m <- sum(index)
  X <- rep(0,n)
  X1 <- rgamma(m, theta, 1)
  X2 <- rgamma(n-m, theta+1/2, 1)
  X[index] <- X1
  X[!index] <- X2

  return(X)
}

#####theta=4
library(ggplot2)
ggplot(data = data.frame(X = Rg(100000, 4) )   , aes(x = X, ..density..) )+
  geom_histogram(bins = 200)
#####theta=1
ggplot(data = data.frame(X = Rg(100000, 1) )   , aes(x = X, ..density..) )+
  geom_histogram(bins = 200)
#####theta=3/4
ggplot(data = data.frame(X = Rg(100000, 3/4) )   , aes(x = X, ..density..) )+
  geom_histogram(bins = 200)


#(d)
U <- runif(100000)
X <- Rg(100000)
Index <- (   U <= sqrt(4+X)/(sqrt(X)+2)   )
Xf <- X[Index]
ggplot(data = data.frame(X = Xf )   , aes(x = X, ..density..) )+
  geom_histogram(bins = 200)