rc_int_Gibbs <- function(X,a,b,p,l,N,M,d){
  dyn.load("/Users/apple/Desktop/ISU 2017 spring/STAT580/Homework/hw5/RC_gibbs.so")
  Rout <- .Call("RCGibbs", X,a,b,N,p,l )
  Rout <- cbind(Rout[[1]],Rout[[2]])
  Rout1 <- Rout[-(1:M),]
  Rout2 <- Rout1[(1:(N-M)) %% d == 0 , ]
  Out <- data.frame(P = Rout2[,1], Lambda = Rout2[,2])
  apply(Out,2,quantile,c(0.025,0.5,0.975))
}



set.seed(580580)
Y <- rpois(100,2)
R <- sample(c(0,1), size = 100, replace = TRUE, prob = c(0.7,0.3))
X <- Y*R


a <- 1
b <- 1
p <- 0.5
l <- 1
N <- 100000
M = 30000
d = 10

rc_int_Gibbs(X,a,b,p,l,N,M,d)
