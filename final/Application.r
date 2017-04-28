
##################For Lena256

###################################################################
#lena256 data

lena <- scan( "/Users/apple/Desktop/ISU 2017 spring/STAT580/Finalproject/Rcode/lena256")
lena <- lena[-c(1,2)]
lena <- matrix(data = lena, nrow = 256)
lena <- lena[,256:1]

image(z=lena, col=gray(1:256/256), axes=FALSE)



##################################################################### 
# Valid & Test


eval.l <- function(Orig.M, Est.M, test){
  # test is the flag that label all the test or validation data
  RMSE <- sqrt(sum((Orig.M[test] - Est.M[test])^2)/length(test))
  return(RMSE)
}



L.valid <- function(data, train, valid, test, l.grid, method=c(1,2,3,4,5),
                    r.valid = min(ncol(data),nrow(data)), m.valid=c(1,2,3)){
  N <- length(data)
  q <- length(l.grid)
  flag <- sample(1:N)
  n.miss <- round(N * test)
  n.valid <- round(N * valid)
  n.train <- N - (n.miss + n.valid)
  f.miss <- flag[1:n.miss]
  f.valid <- flag[n.miss+1:n.valid]
  f.train <- flag[(N-n.train+1): N]
  tr.M <- data
  tr.M[c(f.miss, f.valid)] <- NA 
  # tr.M is the training matrix
  Train.out <- SoftImpute.para(tr.M, l.grid, r = r.valid, svd.method = m.valid) 
  Valid.RMSE <- lapply(Train.out, FUN = eval.l, Orig.M = data, test = f.valid)
  Valid.RMSE <- do.call(Valid.RMSE, what = c)
  k <- which.min(Valid.RMSE)
  lambda <- l.grid[k]
  test.M <- data
  test.M[f.miss] <- NA
#  image(z=test.M, col=gray(1:256/256), axes=FALSE)
  Out <- SoftImpute.np(test.M, lambda, svd.method = method)
#  image(Out[[1]], col=gray(1:256/256), axes=FALSE)
  RMSE <- eval.l(data, Out[[1]], f.miss)
  cat("The lambda we choose from:", l.grid, "\n")
  cat("The RMSE of Validation Set:", Valid.RMSE, "\n")
  return(list(lambda=lambda, Missing = test.M, Estimated_Matrix= Out[[1]], RMSE = RMSE))
}




##################################################################################
# Experiment

set.seed(580580)
O <- L.valid(lena, train=0.42, valid = 0.18, test = 0.4, 
             l.grid = seq(0,1000,50), method = 4, m.valid = 3)
O[[1]]   # Chosen Lambda
O[[4]]   # Final RMSE


set.seed(580580)
O <- L.valid(lena, train=0.42, valid = 0.18, test = 0.4, 
             l.grid = seq(100,200,5), method = 4, m.valid = 3)
O[[1]]   # Chosen Lambda
O[[4]]   # Final RMSE



image(lena,  col=gray(1:256/256), axes=FALSE)
image(O[[2]],  col=gray(1:256/256), axes=FALSE)
image(O[[3]],  col=gray(1:256/256), axes=FALSE)





