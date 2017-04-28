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







