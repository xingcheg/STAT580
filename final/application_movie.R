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
  return(list(lambda=lambda, RMSE = RMSE, Estimated_Matrix= Out[[1]]))
}


#####################################
# Just validation

M.valid_quick <- function(data, valid, test, l.grid, m.valid=c(1,2,3),
                    r.valid){
  
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
  
  
  cat("The lambda we choose from:", l.grid, "\n")
  cat("The RMSE of Validation Set:", Valid.RMSE, "\n")
  cat("The best lambda is:", lambda, "\n")
  return(lambda)
}


###########################
# Just test
M.test_quick <- function(data, test, lambda, method=c(1,2,3,4,5), r.test, e.test){
  
  N <- nrow(data)
  flag <- sample(1:N)
  n.test <- round(N * test)
  n.train <- N - n.test 
  f.test <- flag[1:n.test]
  f.train <- flag[(N-n.test+1): N]
  
  data.o <- Matrix::sparseMatrix(i = data[,1], j = data[,2],
                                 x = data[,3])
  data.o <- as.matrix(data.o)
  # data.o is the original matrix
  
  data1 <- data
  data1[f.test,3] <- 0
  data.t <- Matrix::sparseMatrix(i = data1[,1], j = data1[,2],
                                 x = data1[,3])
  data.t <- as.matrix(data.t)
  # data.t is the testing matrix
  
  test.ij <- which(data.o != data.t)
  Out <- SoftImpute.np(data.t, lambda, svd.method = method, e = e.test,
                       r = r.test, empty_is_na = FALSE)
  RMSE <- eval.m(data.o, Out[[1]], test.ij)

  return(RMSE = RMSE)
}







