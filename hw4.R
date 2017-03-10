###2
n <- 100000
#(a) 
###i X~exp(1)

X <- rexp(n,1)
h_X <- (X^2 +5)*X
I1 <- mean(h_X)
I1
var1 <- var(h_X)/n
var1

###ii X~Gamma(2,1)
X <- rgamma(n,2,1)
h_X <- (X^2 +5)
I2 <- mean(h_X)
I2
var2 <- var(h_X)/n
var2

#(b) X~N(0,1/2), Y~U(0,1), X&Y independent
X <- rnorm(n,0,sqrt(1/2))
Y <- runif(n)
h_XY <- sqrt(pi)*cos(X*Y)
I <- mean(h_XY)
I

#(c)
X <- rexp(n,1)
h_X <- (3/4)*(X^4)*exp( X - (X^3)/4 )
I <- mean(h_X)
I
var1 <- var(h_X)/n
var1


###3
###################
# f(x) = 1/sqrt(2*pi) exp(-x^2/2) h(x)=I(1<x<2)
###################
N <- 100000
e <- 0.1
X <- rnorm(N,1.5,e)
H <- (X>1 & X<2)
W <- (e)*exp( -X^2/2 + (X-3/2)^2/(2*e^2) )
I <- mean(H*W)
I
var1 <- var(H*W)/N
var1



###4
#(a)
U <- runif(1500)
I_mc <- mean( 1/(1+U) )
I_mc
var1 <- var(1/(1+U))/1500
var1

#(b)
theta_mc <- mean(1+U)
theta <- 3/2
Y <- 1/(1+U)
X <- 1+U
b <- as.numeric( coef(lm(Y~X))[2] )
I_cv <- I_mc - b*(theta_mc - theta)
I_cv

var2 <- var1 * (1-cor(X,Y)^2)
var2


#(c)
library(ggplot2)
y1 <- U
y2 <- U^(3/4)
y3 <- sqrt(U)
y4 <- U^(1/4)
Y <- c(y1,y2,y3,y4)
x <- 1/(1+U)
X <- rep(x, 4)
class <- rep(c("1/(U+1)~U","1/(U+1)~U^(3/4)","1/(U+1)~U^(1/2)","1/(U+1)~U^(1/4)"), each=1500)
d <- data.frame(X = X, Y = Y, class = as.factor(class) )

ggplot(data = d) + 
  geom_point(aes(x = X, y = Y, colour = class),  size = 0.2) +
  ylab("U^(1/a)") +
  xlab("1/(1+U)")

cor(y1,x)
cor(y2,x)
cor(y3,x)
cor(y4,x)

##### Find that sqrt(U) has the greatest correlation with 1/(1+U).
 

theta_mc1 <- mean( sqrt(U) )
theta1 <- 2/3
Y <- 1/(1+U)
X <- sqrt(U)
b <- as.numeric( coef(lm(Y~X))[2] )
I_cv1 <- I_mc - b*(theta_mc1 - theta1)
I_cv1

var3 <- var1 * (1-cor(X,Y)^2)
var3


