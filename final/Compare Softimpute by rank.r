#########################################################
### Generate random matrix of rank r (most likely)

randU <- matrix(sample(x=0:5, size=150*100, replace = TRUE, prob = c(0.5,rep(0.1,5))), ncol = 150)
randV <- matrix(sample(x=0:5, size=100*100, replace = TRUE,prob = c(0.5,rep(0.1,5))), ncol = 100)

## Simulate random matrix U and V, with elements in U and V follow a discrete distribution
## that 


#randU <- matrix(rnorm(150*100, mean = 1, sd = 1),ncol = 150)
#randV <- matrix(rnorm(100*100, mean = 1, sd = 1),ncol = 100)

library(Matrix)

 # rankMatrix(randZ, method = "tolNorm2")
Rseq <- seq(5, 100, 5)
R <- c()
for(k in Rseq){
  set.seed(580580)
  randZ <- randU[,1:k] %*% randV[1:k,]
  o <- L.valid(randZ, train=0.6, valid = 0.2, test = 0.2, 
             l.grid = seq(0,200,10), method = 4, m.valid = 4)
  R <- c(R, o[[4]])
}

library(ggplot2)
ggplot(data.frame(rank = Rseq, RMSE = R),aes(x=rank, y = RMSE)) + 
  xlab("True Rank of Target Matrix") + 
  geom_point(colour = "blue") + geom_line() + theme_light()




