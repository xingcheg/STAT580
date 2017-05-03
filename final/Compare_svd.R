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









###################################################
set.seed(580580)
library(microbenchmark)
A3 <- matrix(rep(0,90000), ncol = 300)
for (i in 1:60){
  a <- rnorm(300)
  b <- rnorm(300)
  A3 <- A3 + a%*%t(b)
}


### svd
svd_speed <- microbenchmark( svd(A3,nu=5,nv=5),
                             svd(A3,nu=10,nv=10),
                svd(A3,nu=20,nv=20),
                svd(A3,nu=30,nv=30),
                svd(A3,nu=50,nv=50),
                svd(A3,nu=100,nv=100),
                svd(A3,nu=200,nv=200),
                svd(A3,nu=300,nv=300))

svd_speed_mean <- tapply(svd_speed$time, svd_speed$expr,mean)
names(svd_speed_mean)<-c(5,10,20,30,50,100,200,300)

### propack_svd
propack_speed <- microbenchmark( svd::propack.svd(A3, neig=5),
                                 svd::propack.svd(A3, neig=10),
                             svd::propack.svd(A3, neig=20),
                             svd::propack.svd(A3, neig=30),
                             svd::propack.svd(A3, neig=50),
                             svd::propack.svd(A3, neig=100),
                             svd::propack.svd(A3, neig=200),
                             svd::propack.svd(A3, neig=300))

propack_speed_mean <- tapply(propack_speed$time, propack_speed$expr,mean)
names(propack_speed_mean)<-c(5,10,20,30,50,100,200,300)

### irlba
irlba_speed <- microbenchmark(irlba::irlba(A3,nu=5,nv=5),
                                irlba::irlba(A3,nu=10,nv=10),
                                irlba::irlba(A3,nu=20,nv=20),
                              irlba::irlba(A3,nu=30,nv=30),
                                irlba::irlba(A3,nu=50,nv=50),
                                irlba::irlba(A3,nu=100,nv=100)
                                )

irlba_speed_mean <- tapply(irlba_speed$time, irlba_speed$expr,mean)
names(irlba_speed_mean)<-c(5,10,20,30,50,100)

### arma
arma_speed <- microbenchmark(armasvd(A3))
arma_speed_mean <- mean(arma_speed[,2])
names(arma_speed_mean)<-300

### arma_econ
arma_econ_speed <- microbenchmark(armasvd_econ(A3))
arma_econ_speed_mean <- mean(arma_econ_speed[,2])
names(arma_econ_speed_mean)<-300



x <- c(c(5,10,20,30,50,100,200,300),c(5,10,20,30,50,100,200,300),c(5,10,20,30,50,100),300,300)
z <- as.factor(  c(  rep("svd",8),rep("propack_svd",8),rep("irlba",6),"arma","arma_econ"  )  )
y <- c(svd_speed_mean, propack_speed_mean,irlba_speed_mean, 
       arma_speed_mean, arma_econ_speed_mean)
d <- data.frame(x = x, y = y, z = z)


library(ggplot2)
ggplot(data = d, aes(x = x, y = y, colour = z))+
  geom_point(size = 4, alpha = 0.8)+
  geom_line()+
  xlab("rank")+
  ylab("time")+
  theme_bw()
  

