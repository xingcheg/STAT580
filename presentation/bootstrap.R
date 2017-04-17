library(boot)
library(ggplot2)

###ex.1
x = 1:100

samplemean <- function(x, d) {
  return(mean(x[d]))
}

b <- boot(x, samplemean, R=10000)
D <- data.frame(mean = b$t)

ggplot(data = D) + 
  geom_histogram(aes(x = mean), bins = 100)



###ex.2
diff_mean <- function(X,d) {
  return(mean(X[d,2]-X[d,1]))
}

b <- boot(cd4, diff_mean, R=50000)
ci <- boot.ci(b, conf = 0.95, type = "basic")
cat("95% CI from ", ci$basic[1,4], " - ", ci$basic[1,5], "\n")



###ex.3 parameter bootstrap
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v)

cd4.boot <- boot(cd4, corr, R = 999, sim = "parametric",
                 ran.gen = cd4.rg, mle = cd4.mle)

boot.ci(cd4.boot,  type = c("norm", "basic", "perc"),
        conf = 0.9, h = atanh, hinv = tanh)



###ex.4 parameter bootstrap using parallel computing
library(parallel)
cd4.mle <- list(m = colMeans(cd4), v = var(cd4))
cd4.rg <- function(data, mle) MASS::mvrnorm(nrow(data), mle$m, mle$v) 
run1 <- function(...) boot(cd4, corr, R = 500, sim = "parametric",
                           ran.gen = cd4.rg, mle = cd4.mle)

mc <- 2 # set as appropriate for your hardware
cd4.boot <- do.call(c, mclapply(seq_len(mc), run1) ) 
#do.call() 是告诉list一个函数，然后list里的所有元素来执行这个函数
boot.ci(cd4.boot, type = c("norm", "basic", "perc"), 
        conf = 0.9, h = atanh, hinv = tanh)



