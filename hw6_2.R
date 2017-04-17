####2
x <- rep(c(0.02,0.06,0.11,0.22,0.56,1.10), each = 2)
y <- c(47,76,97,107,123,139,152,159,191,201,200,207)
#(a)
z <- 1/y
u <- 1/x
beta0 <- coef(lm(z~u))
theta1_0 <- as.numeric( 1/beta0[1] )
theta2_0 <- as.numeric( beta0[2]*theta1_0 )
theta0 <- c(theta1_0,theta2_0)
theta0

#(b)
f1 <- function(a){
  p1 <- (-2)*sum( ( y - (a[1]*x)/(x+a[2]) ) * (x/(x+a[2]))  )
  p2 <- 2*sum( ( y - (a[1]*x)/(x+a[2]) ) * ( (a[1]*x)/(x+a[2])^2 )  )
  out <- matrix(c(p1,p2),nrow=2)
  return(out)
}

f2 <- function(a){
  p11 <- 2*sum( (x/(x+a[2]))^2   )
  p12 <- 2*sum(   (x*y)/(x+a[2])^2  -  (2*a[1]*x^2)/(x+a[2])^3  )
  p22 <- 2*sum(  3*(x*a[1])^2/(x+a[2])^4  -  (2*a[1]*x*y)/(x+a[2])^3   )
  out <- matrix(c(p11,p12,p12,p22),nrow=2)
  return(out)
}

NR_MM <- function(p0, e = 1e-6){
  while(1){
    p <- p0 - solve(f2(p0))%*%f1(p0)
    if( norm(p-p0,"F") < e || norm(f1(p),"F")<1e-12 ) break
    p0 <- p
  }
  return(p)
}

theta1 <- NR_MM(theta0)
theta1


# evalution
f <- function(a){
  out <- sum( ( y - a[1]*x/(x+a[2]) )^2 )
  return(out)
}

f(theta0)
f(theta1)

#(c)
SD_MM <- function(p0, a, e = 1e-6, max_i = 100000000){
  b <- a
  for(i in 1:max_i){
    p <- p0 - b*f1(p0)
    while(f(p)>f(p0)){
      b <- b/2
      p <- p0 - b*f1(p0)
    }
    if( norm(p-p0,"F") < e || norm(f1(p),"F")<1e-8 ) break
    p0 <- p
    b <- a
  }
  if(i == max_i) cat("exceed the maximal iteration times, didn't find the minimum value.")
  return(p)
}


theta2 <- SD_MM(theta0, a = 0.1, max_i = 3000000)
theta2
f(theta2)

#(d)
Z <- function(a){
  out <- y - a[1]*x/(x+a[2])
  out <- as.matrix(out,ncol=1)
  return(out)
}

A <- function(a){
  out1 <- x/(x+a[2])
  out2 <- -a[1]*x/(x+a[2])^2
  out <- cbind(out1,out2)
  return(out)
}

GN_MM <- function(p0, e = 1e-6, max_i = 100000000){
  for(i in 1:max_i){
    p <- p0 + solve( t(A(p0)) %*% A(p0) ) %*% t(A(p0)) %*% Z(p0)
    if( norm(p-p0,"F") < e) break
    p0 <- p
    if(i == max_i) cat("exceed the maximal iteration times, didn't find the minimum value.")
  }
  return(p)
}

theta3 <- GN_MM(theta0)
theta3
f(theta3)


