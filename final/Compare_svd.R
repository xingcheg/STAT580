######For testing speed
set.seed(580580)
library(microbenchmark)

## small matrix
A1 <- matrix(rep(0,400), ncol = 20)
for (i in 1:5){
  a <- rnorm(20)
  b <- rnorm(20)
  A1 <- A1 + a%*%t(b)
}

## armasvd  <= armasvd_econ <= svd < propack.svd < irlba  (full rank)
microbenchmark( svd(A1,nu=20,nv=20),
                svd::propack.svd(A1, neig=20),
                armasvd(A1),
                armasvd_econ(A1),
                irlba::irlba(A1,nu=19, nv=19)
                )

#svd <= irlba << propack.svd
microbenchmark( svd(A1,nu=5,nv=5),
                svd::propack.svd(A1, neig=5),
                irlba::irlba(A1,nu=5, nv=5)
)


## middle matrix
A2 <- matrix(rep(0,6400), ncol = 80)
for (i in 1:15){
  a <- rnorm(80)
  b <- rnorm(80)
  A2 <- A2 + a%*%t(b)
}

## armasvd_econ  <= armasvd <= svd << irlba << propack.svd (full rank)
microbenchmark( svd(A2,nu=80,nv=80),
                svd::propack.svd(A2, neig=80),
                armasvd(A2),
                armasvd_econ(A2),
                irlba::irlba(A2,nu=79,nv=79))

## irlba << svd << propack.svd
microbenchmark( svd(A2,nu=15,nv=15),
                svd::propack.svd(A2, neig=15),
                irlba::irlba(A2,nu=15,nv=15))


## large matrix
A3 <- matrix(rep(0,90000), ncol = 300)
for (i in 1:60){
  a <- rnorm(300)
  b <- rnorm(300)
  A3 <- A3 + a%*%t(b)
}

## armasvd <= armasvd_econ  <= svd << propack.svd (full rank)
microbenchmark( svd(A3,nu=300,nv=300),
                svd::propack.svd(A3, neig=300),
                armasvd(A3),
                armasvd_econ(A3))



## irlba << svd <= propack.svd
microbenchmark( svd(A3,nu=30,nv=30),
                svd::propack.svd(A3, neig=30),
                irlba::irlba(A3,nu=30,nv=30))

## irlba < propack.svd < svd
microbenchmark( svd(A3,nu=15,nv=15),
                svd::propack.svd(A3, neig=15),
                irlba::irlba(A3,nu=15,nv=15))

## irlba <= propack.svd << svd
microbenchmark( svd(A3,nu=7,nv=7),
                svd::propack.svd(A3, neig=7),
                irlba::irlba(A3,nu=7,nv=7))