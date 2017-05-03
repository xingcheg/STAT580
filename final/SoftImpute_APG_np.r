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





SoftImpute.APG.np <- function(Z, Lambda, r = min(ncol(Z),nrow(Z)), e = 0.001, 
                          svd.method = c(1,2,3,4,5), empty_is_na = TRUE){
  ####  Lambda should be a descending sequence
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
  Z_0 <- matrix(0,nrow = m, ncol = n)
  Z_old <- Z_0
  k <- length(Lambda)
  Z_hat <- list()
  Z_orig <- P_obs1(Z,Z)
  A <- Z_0
  for (i in 1:k){
    lambda <- Lambda[i]
    t_old <- 1
    A <- Z_0
    Z_old <- Z_0
    while (1){
      Z_new <- Soft( Z_orig-P_obs1(A, Z)+A , r , lambda , svd.method)
      t_new <- (1 + sqrt(1 + 4* t_old^2))/2
      A <- Z_new + ((t_old - 1)/t_new) * (Z_new - Z_old)
      if (norm(Z_new-Z_old, type = "F")/norm(Z_old, type = "F") < e)
        break;
      Z_old <- Z_new
    }
    Z_hat <- c(Z_hat, list(Z_new))
    Z_0 <- Z_new
  }
  return(Z_hat)
}
















