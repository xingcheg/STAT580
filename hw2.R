###(2)
#######################
#
# F(x) = log(X)/log(10)
#
# X = exp( log(10) * U )
#
#######################

X <- exp(log(10)*runif(5000))
rX <-as.data.frame(X)
library(ggplot2)
ggplot(aes(x = X, ..density..), data=rX) +
  geom_histogram(binwidth = 0.025) + 
  geom_density(colour = "blue", fill="blue", alpha=0.2)+
  ylim(0,0.5)


###(3)
Q <- function(x){
  out <- exp(-x)/(1 + x^2)
  return(out)
}

### g1~exp(1); g2~abs(cauchy(0,1))
a1 <- 1
a2 <- pi/2
x <- seq(0, 5, length.out=500)
X <- rep(x,times = 3)
q <- Q(x)
g1 <- a1*dexp(x, 1)
g2 <- a2*2*dcauchy(x,0,1)
FX <- c(q,g1,g2)
type <- factor(rep(c("q(x)","a1*g1(x)","a2*g2(x)"), each=500))
df <- data.frame(X,FX,type)

ggplot(data = df,aes(x = X, y = FX)) +
  geom_line(aes(group = type, colour = type)) 

### for g1
U <- runif(30000)
X1 <- rexp(30000,1)
not_rej <- (   U <= Q(X1)/(a1*dexp(X1,1))   )
X_g1<-X1[not_rej][1:5000]
ggplot(data = as.data.frame(X_g1), aes(x = X_g1, ..density..)) +
  geom_histogram(binwidth = 0.02) + 
  geom_density(colour = "blue", fill="blue", alpha=0.2)+
  xlim(0,5)

### for g2
U <- runif(30000)
X2 <- abs(rcauchy(30000,0,1))
not_rej <- (   U <= Q(X2)/(a2*2*dcauchy(X2,0,1))   )
X_g2<-X2[not_rej][1:5000]
ggplot(data = as.data.frame(X_g2), aes(x = X_g2, ..density..)) +
  geom_histogram(binwidth = 0.02) + 
  geom_density(colour = "red", fill="red", alpha=0.2)+
  xlim(0,5)
