###1
##(a)
set.seed(580580)
Y <- rpois(100,2)
R <- sample(c(0,1), size = 100, replace = TRUE, prob = c(0.7,0.3))
X <- Y*R

#####The MLE of the lambda & p.
meanL_mle <- mean(Y)
meanP_mle <- mean(R)
meanL_mle
meanP_mle

##(c)
P <- c()
Lambda <- c()
a <- 1
b <- 1
p <- 0.5
l <- 1
label <- X==0
n <- sum(label)
sum_X <- sum(X)

N = 100000
for (i in 1:N){
  r <- rep(1,100)
  p0 <- (p*exp(-l))/(p*exp(-l)+1-p)
  r[label] <- sample(c(0,1), size = n, replace = TRUE, prob = c(1-p0,p0))
  sum_r <- sum(r)
  l <- rgamma(1, a+sum_X, b+sum_r)
  Lambda <- c(Lambda,l)
  p <- rbeta(1, 1+sum_r, 101-sum_r)
  P <- c(P,p)
}

M = 30000
d = 10
P1 <- P[-(1:M)]
P2 <- P1[(1:(N-M)) %% d == 0 ]
Lambda1 <- Lambda[-(1:M)]
Lambda2 <- Lambda1[(1:(N-M)) %% d == 0 ]

meanL <- mean(Lambda2)
L_95 <- quantile(Lambda2,c(0.025,0.975))
meanP <- mean(P2)
P_95 <- quantile(P2,c(0.025,0.975))
meanL
L_95
meanP
P_95

d <- data.frame(prob = P2, lambda = Lambda2)
library(ggplot2)
ggplot(data = d, aes(x = prob,..density..))+
  geom_histogram(bins = 100, fill = "blue", alpha = 0.4, colour = "grey")+
  geom_vline( xintercept = meanP, colour = "red")+
  geom_vline( xintercept = meanP_mle, colour = "blue")+
  geom_vline( xintercept = P_95, colour = "red", alpha = 0.4)
  
  
  

ggplot(data = d, aes(x = lambda,..density..))+
  geom_histogram(bins = 100, fill = "blue", alpha = 0.4, colour = "grey")+
  geom_vline( xintercept = meanL, colour = "red")+
  geom_vline( xintercept = meanL_mle, colour = "blue")+
  geom_vline( xintercept = L_95, colour = "red", alpha = 0.4)






###2
set.seed(521521)
a1 <- 1.5
a2 <- 2
f <- function(z,a1,a2) z^(-3/2)*exp( -a1*z -a2/z )

a <-1
b <-1
N <- 20000
x <- 1
X <- c(x)
for (i in 1:N){
  y <- rgamma(1,a,b)
  r0 <- ( f(y,a1,a2)/f(x,a1,a2) ) * ( dgamma(x,a,b)/dgamma(y,a,b) )
  r <- min(r0,1)
  u <- runif(1)
  if (u <= r){
    x <- y
  }
  X <- c(X,x)
}

M = 10000
d = 10
X1 <- X[-(1:M)]
X2 <- X1[(1:(N-M)) %% d == 0 ]


mean(X2)
sqrt(a2/a1)

mean(1/X2)
sqrt(a1/a2)+1/(2*a2)

### TRUE density function
Int <- integrate(f,0,Inf,a1=a1,a2=a2)$value
f1 <- function(z,a1,a2){
  (1/Int)*z^(-3/2)*exp( -a1*z -a2/z )
}
########################
library(ggplot2)
d <- data.frame( x = X2 )
ggplot(data = d)+
  stat_function(fun = f1, args = list(a1=a1, a2=a2), colour = "red")+
  geom_histogram(aes(x = x,..density..),bins = 100, 
                 fill = "green", alpha = 0.4, colour = "grey")










