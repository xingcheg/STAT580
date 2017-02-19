#include<stdio.h>
#define n 16  /* number of observations */
#define p 2   /* number of predictors */

void dgesv_(int *N, int *NRHS, double *A, int *LDA, int *IPIV,
                 double *B, int *LDB, int *INFO);

void dgemm_(char *TRANSA, char *TRANSB, int *M, int *N, int *K,
		double *ALPHA, double *A, int *LDA, double *B, int *LDB,
		double *BETA, double *C, int *LDC);

	
int main(){
/* longley dataset from R: Employed (Y) GNP.deflator and Population (X) */
    double Y[n] = {60.323,61.122,60.171,61.187,63.221,63.639,64.989,
                   63.761,66.019,67.857,68.169,66.513,68.655,69.564,
                   69.331,70.551};
    double X[n][p] =
    {{83,107.608},
    {88.5,108.632},
    {88.2,109.773},
    {89.5,110.929},
    {96.2,112.075},
    {98.1,113.27},
    {99,115.094},
    {100,116.219},
    {101.2,117.388},
    {104.6,118.734},
    {108.4,120.445},
    {110.8,121.95},
    {112.6,123.366},
    {114.2,125.368},
    {115.7,127.852},
    {116.9,130.081}};

    int i,j,k;
	double X1[n*(p+1)];
    char trans = 'T', no_trans = 'N';
    double alpha = 1, beta = 0;
	int K = n;
	int N_X = p+1;
	int N_Y = 1;
	double XX[(p+1)*(p+1)], XY[(p+1)];
	int info;
	int ipiv[p+1];


	for (k=0; k<n; k++){
		X1[k] = 1;
	}

	for (i=1; i<p+1; i++){
		for (j=0; j<n; j++){
			X1[i*n+j] = X[j][i-1];
		}
	}


	dgemm_(&trans, &no_trans, &N_X, &N_X, &K, &alpha, X1, &K, X1,
			&K, &beta, XX, &N_X);

	dgemm_(&trans, &no_trans, &N_X, &N_Y, &K, &alpha, X1, &K, Y,
			&K, &beta, XY, &N_X);

	dgesv_(&N_X, &N_Y, XX, &N_X, ipiv, XY, &N_X, &info);

	if (info == 0){
		printf("The regression coefficients:\t");
		for (i = 0; i<N_X; i++){
			printf("%f\t", XY[i]);
		}
      printf("\n");
	}
	else printf("dgesv error %d\n", info);

return (0);

}
