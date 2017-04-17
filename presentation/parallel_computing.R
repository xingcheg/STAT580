library(parallel)

f <- function(x) x^(0.6)



### calculate cores
no_cores <- detectCores() - 1
### initial clusters
cl <- makeCluster(no_cores)

system.time( lapply(1:1000000,f) )
system.time ( parLapply(cl, 1:1000000, f) )

system.time( sapply(1:1000000,f) )
system.time ( parSapply(cl, 1:1000000, f) )

###close clusters
stopCluster(cl)


#####################
# Simulation Example
#####################

##EX1 parametric bootstrap

###don't use parallel computing
library(boot)

no_para <- function(...){
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)

cd4.boot <- boot(cd4, corr, R = 200000, sim = "parametric",
                 ran.gen = cd4.rg, mle = cd4.mle)

boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)
}

###use parallel computing (mclapply)

para <- function(...){
  
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v) 
run1 <- function(...) boot(cd4, corr, R = 500, sim = "parametric",
                           ran.gen = cd4.rg, mle = cd4.mle)

mc <- 400 # set as appropriate for your hardware
cd4.boot <- do.call(c, mclapply(seq_len(mc), run1) ) 
#do.call() 是告诉list一个函数，然后list里的所有元素来执行这个函数
boot.ci(cd4.boot, type = c("norm", "basic", "perc"), 
        conf = 0.9, h = atanh, hinv = tanh)
}

###use parallel computing (Parlapply)
para1<-function(...){
  
run1 <- function(...)  {
  library(boot)
  cd4.mle <- list(m = colMeans(cd4), v = var(cd4)) 
  cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
  boot(cd4, corr, R = 500, sim = "parametric", 
                            ran.gen = cd4.rg, mle = cd4.mle)
}

cl <- makeCluster(6)
library(boot)
cd4.boot <- do.call(c, parLapply(cl, seq_len(400), run1) ) 
stopCluster(cl)
boot.ci(cd4.boot, type = c("norm", "basic", "perc"),conf = 0.9, h = atanh, hinv = tanh)
}



###doParallel & foreach

library(doParallel)
# Create cluster with desired number of cores
cl <- makeCluster(3)
# Register cluster
registerDoParallel(cl)
# Find out how many cores are being used
getDoParWorkers()
# use foreach package
result <- foreach(i=1:10) %dopar% {
  max(svd( matrix(rnorm(100000), nrow=100) )$d)
}

stopCluster(cl)

###use doParallel (foreach) to do bootstrap
para2<-function(...){
  
  library(doParallel)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  
  
  library(boot)
  cd4.mle <- list(m = colMeans(cd4), v = var(cd4)) 
  cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)
  
  cd4.boot <- foreach(i=1:400, .combine = c) %dopar% {
  boot::boot(boot::cd4, boot::corr, R = 500, sim = "parametric", 
         ran.gen = cd4.rg, mle = cd4.mle)
  }
  stopCluster(cl)
  boot::boot.ci(cd4.boot, type = c("norm", "basic", "perc"),conf = 0.9, h = atanh, hinv = tanh)
}




system.time(no_para())
system.time(para())
system.time(para1())
system.time(para2())


