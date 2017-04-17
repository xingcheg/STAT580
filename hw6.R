###1
#(c)
log_cauchy <- function(a){
  X <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 
         0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  n <- length(X)
  out <- -n*log(pi) - sum( log( 1 + (a-X)^2 )  )
  return(out)
}

library(ggplot2)

theta <- seq(-50,50,length.out=1000)
f_theta <- do.call("c",  lapply(theta, log_cauchy) )
d <- data.frame(x = theta, y = f_theta)
ggplot(data = d, aes(x = x, y = y)) + geom_line(color = "blue") +theme_light()

theta <- seq(-5,5,length.out=1000)
f_theta <- do.call("c",  lapply(theta, log_cauchy) )
d <- data.frame(x = theta, y = f_theta)
ggplot(data = d, aes(x = x, y = y)) + geom_line(color = "blue") +theme_light()

theta <- seq(-0.25,-0.15,length.out=1000)
f_theta <- do.call("c",  lapply(theta, log_cauchy) )
d <- data.frame(x = theta, y = f_theta)
ggplot(data = d, aes(x = x, y = y)) + geom_line(color = "blue") +theme_light()


#(d)
NR_cauchy <- function(p0, e = 1e-6){
  X <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 
         0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  f1 <- function(a){
    out <- sum(  (a-X)/( 1+(a-X)^2 )  )
    return(out)
  }
  f2 <- function(a){
    out <- sum(  (1-(a-X)^2)/( 1+(a-X)^2 )^2  )
    return(out)
  }
  
  while(1){
    p <- p0 - f1(p0)/f2(p0)
    if( abs(p-p0) < e || f1(p)<1e-12 ) break
    p0 <- p
  }
  
  return(p)
  
}

theta1_0 <- c(-11,-1,0,1.4,4.1,4.8,7,8,38)
theta1 <- do.call("c",  lapply(theta1_0, NR_cauchy) )
f_theta1 <- do.call("c",  lapply(theta1, log_cauchy) )
theta1
f_theta1


#(e)
FS_NR_cauchy <- function(p0, e = 1e-8, fisher = 200){
  X <- c(-13.87, -2.53, -2.44, -2.40, -1.75, -1.34, -1.05, -0.23, -0.07, 
         0.27, 1.77, 2.76, 3.29, 3.47, 3.71, 3.80, 4.24, 4.53, 43.21, 56.75)
  f1 <- function(a){
    out <- sum(  (a-X)/( 1+(a-X)^2 )  )
    return(out)
  }
  f2 <- function(a){
    out <- sum(  (1-(a-X)^2)/( 1+(a-X)^2 )^2  )
    return(out)
  }
  I <- length(X)/2
  for (i in 1:fisher){
    p <- p0 - 2*f1(p0)/I
    p0 <- p
  }
  
  while(1){
    p <- p0 - f1(p0)/f2(p0)
    if( abs(p-p0) < e || f1(p)<1e-12 ) break
    p0 <- p
  }
  return(p)
}

theta2_0 <- c(-11,-1,0,1.4,4.1,4.8,7,8,38)
theta2 <- do.call("c",  lapply(theta2_0, FS_NR_cauchy) )
f_theta2 <- do.call("c",  lapply(theta2, log_cauchy) )
theta2
f_theta2

optimise(log_cauchy, c(-1,1), maximum = TRUE)






