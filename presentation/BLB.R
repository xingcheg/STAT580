library(datadr)
### divide
bySpecies <- divide(iris, by = "Species",update = TRUE)
bySpecies
summary(bySpecies)
bySpecies[[1]]
bySpecies[[2]]

irisRR <- divide(iris, by = rrDiv(30),update = TRUE)
irisRR
irisRR[[1]]
irisRR[[2]]
irisRR[[3]]

### addTransform
lmCoef <- function(x)
  coef(lm(Sepal.Length ~ Sepal.Width, data = x))[2]

bySpecies_coef <- addTransform(bySpecies,lmCoef)
bySpecies_coef[[1]]
bySpecies_coef[[2]]
bySpecies_coef[[3]]

### recombine
iris_coef <- recombine(bySpecies_coef,combine = combRbind)
iris_coef
iris_coef1 <- recombine(bySpecies_coef,combine = combMean)
iris_coef1


###BLB --- drBLB
rrAdult <- divide(adult, by = rrDiv(1000), update = TRUE)
BLB <- function(x) {
  drBLB(x,
        statistic = function(x, weights)
          coef(glm(incomebin ~ educationnum,
                   data = x, weights = weights, family = binomial()))[2],
        metric = function(x)
          quantile(x, c(0.05, 0.95)),
        R = 100,
        n = nrow(rrAdult)
  )
}
adultBlb <- addTransform(rrAdult, BLB)
coefs <- recombine(adultBlb, combMean)
matrix(coefs, ncol = 2, byrow = TRUE)


### compared with bootstrap
library(boot)
coef_adult <- function(x,d){
  coef(glm(incomebin ~ educationnum,
           data = x[d,], family = binomial()))[2]
}

BOOT <- boot(adult, coef_adult, 100)
CI <- boot.ci(BOOT, conf = 0.90, type = "basic")
CI




