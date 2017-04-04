library(Rcpp)

cppFunction('NumericMatrix Gibbs(NumericVector X, double a, double b, int N, double p, double l) {

NumericMatrix mat(N,2);
int n, m = X.size();
LogicalVector label(m);
NumericVector r(m);
int sum_x, sum_r, i,j;
double p0, u;

sum_x = sum(X);
mat(0,0) = l;
mat(0,1) = p;
label = (X==0);
n = sum(label);

GetRNGstate();
for (i = 1; i < N; i++){
  r = rep(1,m);
  p0 = (p*exp(-l))/(p*exp(-l)+1-p);

  for (j = 0; j < m; j++){
    if ( label(j) == TRUE){
      u = runif(1)[0];
      if ( u < 1-p0 ) r(j) = 0;
    }
  }

  sum_r = sum(r);
  l = rgamma(1, a+sum_x,  1/(b+sum_r) )[0];
  mat(i,0) = l;
  p = rbeta(1, 1+sum_r, m+1-sum_r)[0];
  mat(i,1) = p;
}
PutRNGstate();

return(mat);

}')


rcppGibbs <- function(X,a,b,p,l,N,M,d){
  Rout <- Gibbs(X,a,b,N,p,l)
  Rout1 <- Rout[-(1:M),]
  Rout2 <- Rout1[(1:(N-M)) %% d == 0 , ]
  Out <- data.frame(Lambda = Rout2[,1], P = Rout2[,2])
  apply(Out,2,quantile,c(0.025,0.5,0.975))
}




set.seed(580580)
Y <- rpois(100,2)
R <- sample(c(0,1), size = 100, replace = TRUE, prob = c(0.7,0.3))
X <- Y*R


a <- 1
b <- 1
p <- 0.5
l <- 1
N <- 100000
M = 30000
d = 10

rcppGibbs(X,a,b,p,l,N,M,d)




