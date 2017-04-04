#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


SEXP RCGibbs(SEXP X, SEXP a_, SEXP b_, SEXP N_, SEXP p_, SEXP l_){

double a,b;
double p,l;
int N, n, m = length(X);
int r[m], label[m];
int sum_x, sum_r, i,j,k;
double p0, u;

a = asReal(a_);
b = asReal(b_);
N = asInteger(N_);
p = REAL(p_)[0];
l = REAL(l_)[0];

SEXP P = PROTECT(allocVector(REALSXP, N)); 
SEXP Lambda = PROTECT(allocVector(REALSXP, N)); 
SEXP Rout = PROTECT(allocVector(VECSXP, 2)); 
SET_VECTOR_ELT(Rout, 0, P); 
SET_VECTOR_ELT(Rout, 1, Lambda); 


sum_x = 0;
for (k=0; k<m; k++){
sum_x = sum_x + REAL(X)[k];
}

REAL(Lambda)[0] = l;
REAL(P)[0] = p;

for (k=0; k<m; k++){
if (REAL(X)[k] == 0){
label[k] = 1;
}
else{
label[k] = 0;
}
}

n = 0;
for (k=0; k<m; k++){
n = n + label[k];
}


GetRNGstate();

for (i = 1; i < N; i++){

for (j=0; j<m; j++){
r[j] = 1;
}
                     p0 = (p*exp(-l))/(p*exp(-l)+1-p);
                     
                     for (j = 0; j < m; j++){
                     if ( label[j] == 1){
                     u = runif(0,1);
                     if ( u < 1-p0 ) r[j] = 0;
                     }
                     }
                     
                     
                     sum_r = 0;
                     for (j=0; j<m; j++){
                       sum_r = sum_r + r[j];
                     }
                     l = rgamma(a+sum_x,  1/ (b+sum_r) );
                     REAL(Lambda)[i] = l;
                     p = rbeta(1+sum_r, m+1-sum_r);
                     REAL(P)[i] = p;
}

PutRNGstate();
 
                  
UNPROTECT(3);
return Rout;
}
