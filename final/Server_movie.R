




library(Rcpp)
library(RcppArmadillo)

code_full <- 'List armasvd(arma::mat & X){
List out;
arma::mat U, V;
arma::vec D;
arma::svd(U, D, V, X);
out["u"] = U;
out["d"] = D;
out["v"] = V;
return out;
}'

cppFunction(code=code_full, depends="RcppArmadillo")

code_econ <- 'List armasvd_econ(arma::mat & X){
List out;
arma::mat U, V;
arma::vec D;
arma::svd_econ(U, D, V, X);
out["u"] = U;
out["d"] = D;
out["v"] = V;
return out;
}'

cppFunction(code=code_econ, depends="RcppArmadillo")



SoftImpute.np <- function(Z, Lambda, r = min(ncol(Z),nrow(Z)), e = 0.001, 
                          svd.method = c(1,2,3,4,5), empty_is_na = TRUE){
  Soft_thres<-function(x,lambda) {
    do.call( what = c,  args = lapply(x-lambda, function(x){max(x,0)}) )
  }
  Soft <- function(X, r, lambda, svd.method = c(1, 2, 3, 4, 5)){
    if (svd.method == 1){
      s <- svd(X, nu = r, nv = r)
    }
    if (svd.method == 2){
      s <- irlba::irlba(X, nu = r, nv = r)
    }
    if (svd.method == 3){
      s <- svd::propack.svd(X, neig = r)
      r <- length(s$d)
    }
    if (svd.method == 4){
      s <- armasvd(X)
      s$d <- as.vector(s$d)
      s$u <- s$u[,1:r]
      s$v <- s$v[,1:r]
    }
    if (svd.method == 5){
      s <- armasvd_econ(X)
      s$d <- as.vector(s$d)
      s$u <- s$u[,1:r]
      s$v <- s$v[,1:r]
    }
    U <- s$u
    V <- s$v
    D_lambda <- Soft_thres(s$d[1:r], lambda)
    X_lambda <- U %*% diag(D_lambda, nrow=r) %*% t(V)
    return(X_lambda)
  } 
  
  if (empty_is_na){
    P_obs1 <- function(X, Z){
      X[is.na(Z)] <- 0
      return(X)
    }
  }
  else
  {
    P_obs1 <- function(X, Z){
      X[Z==0] <- 0
      return(X)
    }
  }
  m <- nrow(Z)
  n <- ncol(Z)
  Z_old <- matrix(0,nrow = m, ncol = n)
  k <- length(Lambda)
  Z_hat <- list()
  Z_orig <- P_obs1(Z,Z)
  for (i in 1:k){
    lambda <- Lambda[i]
    while (1){
      Z_new <- Soft( Z_orig-P_obs1(Z_old, Z)+Z_old , r , lambda , svd.method)
      if (norm(Z_new-Z_old, type = "F")/norm(Z_old, type = "F") < e)
        break;
      Z_old <- Z_new
    }
    Z_hat <- c(Z_hat, list(Z_new))
  }
  return(Z_hat)
}











SoftImpute.para <- function(Z, Lambda, r = min(ncol(Z),nrow(Z)), e = 0.001, 
                            svd.method = c(1,2,3), empty_is_na = TRUE){
  ######Set parallel computing
  library(doParallel)
  cores <- detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  ###########################
  
  
  
  ########## (Function) SVD Algorithm & Soft-thresholded SVD  
  Soft_thres<-function(x,lambda) {
    do.call( what = c,  args = lapply(x-lambda, function(x){max(x,0)}) )
  }
  
  
  Soft <- function(X, r, lambda, method = c(1, 2, 3, 4)){
    if (method == 1){
      s <- svd(X, nu = r, nv = r)
    }
    if (method == 2){
      s <- irlba::irlba(X, nu = r, nv = r)
    }
    if (method == 3){
      s <- svd::propack.svd(X, neig = r)
    }
    U <- s$u
    V <- s$v
    D_lambda <- Soft_thres(s$d[1:r], lambda)
    X_lambda <- U %*% diag(D_lambda, nrow=r) %*% t(V)
    return(X_lambda)
  } 
  ########################################################
  
  
  
  
  ###### (Function) Assign the matrix element to 0 at the missing positions.
  if (empty_is_na){
    P_obs1 <- function(X, Z){
      X[is.na(Z)] <- 0
      return(X)
    }
  }
  else
  {
    P_obs1 <- function(X, Z){
      X[Z==0] <- 0
      return(X)
    }
  }
  #########################################################################
  
  
  
  ###### Soft-Impute Algorithm
  m <- nrow(Z)
  n <- ncol(Z)
  k <- length(Lambda)
  Z_old <- matrix(0,nrow = m, ncol = n)
  Z_orig <- P_obs1(Z,Z)
  
  
  Z_hat <- foreach(i = 1:k) %dopar%{
    lambda <- Lambda[i]
    while (1){
      Z_new <- Soft( Z_orig-P_obs1(Z_old, Z)+Z_old , r , lambda , method = svd.method)
      if (norm(Z_new-Z_old, type = "F")/norm(Z_old, type = "F") < e)
        break;
      Z_old <- Z_new
    }
    return(Z_new)
  }
  ############################
  
  
  ######End Parallel Computing
  stopCluster(cl)
  ###########################
  
  return(Z_hat)
}



####################################################################################
# Valid & Test

########
eval.m <- function(Orig.M, Est.M, test){
  # test is the flag that label all the test or validation data
  RMSE <- sqrt(sum((Orig.M[test] - Est.M[test])^2)/length(test))
  return(RMSE)
}







#########################################
# Test and Validation 
M.valid <- function(data, valid, test, l.grid, method=c(1,2,3,4,5),m.valid=c(1,2,3),
                    r.valid, r.test, e = 1e-3){
  
  N <- nrow(data)
  q <- length(l.grid)
  flag <- sample(1:N)
  n.test <- round(N * test)
  n.valid <- round(N * valid)
  n.train <- N - (n.test + n.valid)
  f.test <- flag[1:n.test]
  f.valid <- flag[n.test+1:n.valid]
  f.train <- flag[(N-n.train+1): N]
  
  data.o <- Matrix::sparseMatrix(i = data[,1], j = data[,2],
                                 x = data[,3])
  data.o <- as.matrix(data.o)
  # data.o is the original matrix
  
  data1 <- data
  data1[c(f.test,f.valid),3] <- 0
  data.v <- Matrix::sparseMatrix(i = data1[,1], j = data1[,2],
                                 x = data1[,3])
  data.v <- as.matrix(data.v)
  # data.v is the training matrix
  
  data2 <- data
  data2[f.test,3] <- 0
  data.t <- Matrix::sparseMatrix(i = data2[,1], j = data2[,2],
                                 x = data2[,3])
  data.t <- as.matrix(data.t)
  # data.t is the testing matrix
  
  test.ij <- which(data.o != data.t)
  test_valid.ij <- which(data.o != data.v)
  valid.ij <- setdiff(test_valid.ij,test.ij)
  
  Train.out <- SoftImpute.para(data.v, l.grid, svd.method = m.valid, 
                               r = r.valid, empty_is_na = FALSE) 
  Valid.RMSE <- lapply(Train.out, FUN = eval.m, Orig.M = data.o, test = valid.ij)
  Valid.RMSE <- do.call(Valid.RMSE, what = c)
  k <- which.min(Valid.RMSE)
  lambda <- l.grid[k]
  
  
  Out <- SoftImpute.np(data.t, lambda, svd.method = method, 
                       r = r.test, empty_is_na = FALSE, e = e)
  RMSE <- eval.m(data.o, Out[[1]], test.ij)
  cat("The lambda we choose from:", l.grid, "\n")
  cat("The RMSE of Validation Set:", Valid.RMSE, "\n")
  return(list(lambda=lambda,  RMSE = RMSE,  Estimated_Matrix= Out[[1]],  l.grid = l.grid,  Valid.RMSE = Valid.RMSE ))
}





###################################################################################
#For 10k movie
movie_raw_data <- read.table(file = "movie.txt")
movie_data <- movie_raw_data[,1:3]
names(movie_data) <- c("user","item","score")


GRID <- (1:256)[(1:256)%%16 == 0] # Z
(1:256)[(1:256)%%16 == 1] # G
(1:256)[(1:256)%%16 == 2] # M
(1:256)[(1:256)%%16 == 3]
(1:256)[(1:256)%%16 == 4] #Z
(1:256)[(1:256)%%16 == 5] #G
(1:256)[(1:256)%%16 == 6] # M
(1:256)[(1:256)%%16 == 7]
(1:256)[(1:256)%%16 == 8]#Z
(1:256)[(1:256)%%16 == 9]#G
(1:256)[(1:256)%%16 == 10]#M
(1:256)[(1:256)%%16 == 11]
(1:256)[(1:256)%%16 == 12]#Z
(1:256)[(1:256)%%16 == 13]#G
(1:256)[(1:256)%%16 == 14]#M
(1:256)[(1:256)%%16 == 15]






set.seed(580580)
OO <- M.valid(movie_data, test = 0.15, valid = 0.15,
               l.grid  = GRID, method = 1, r.test = 580, 
               r.valid = 580, m.valid = 1, e = 1e-5)
