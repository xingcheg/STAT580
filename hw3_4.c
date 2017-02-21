#include<stdio.h>
#include<stdlib.h>
#include<assert.h>

#define n 16  /* number of observations */
#define p 2   /* number of predictors */

void dgesvd_(char *JOBU, char *JOBVT, int *M, int *N, double *A,
		int *LDA, double *S, double *U, int *LDU, double *VT,
		int *LDVT, double *WORK, int *LWORK, int *INFO);


int main(){
	
    /* longley dataset from R */
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

	int i, j, info;
	double X_sum[p], Z[n*p];
	char jobu = 'S';
	char jobvt = 'A';
	int MA = n;
	int NA = p;
	double S[p], U[n*p], VT[p*p];
    int lwork = -1;	
	double wkopt;
	double *work;
	double UD[n][p];

	for (j=0; j<p; j++){
		X_sum[j] = 0;
		for (i=0; i<n; i++){
			X_sum[j] = X_sum[j] + X[i][j];
		}
	}

	for (j=0; j<p; j++){
		for (i=0; i<n; i++){
			Z[i+n*j] = X[i][j] - X_sum[j]/n;
		}
	}

	dgesvd_(&jobu, &jobvt, &MA, &NA, Z, &MA, S, U, &MA, VT, &NA, &wkopt,
			&lwork, &info);

	 lwork = (int)wkopt;
     work = (double*)malloc( lwork*sizeof(double) );
	 assert(work != NULL);

	 dgesvd_(&jobu, &jobvt, &MA, &NA, Z, &MA, S, U, &MA, VT, &NA, work,
			&lwork, &info);


	if (info == 0){
		printf("The principal component scores:\n");
		for (i=0; i<n; i++){
	        for (j=0; j<p; j++){
		        UD[i][j] = U[i+j*n]*S[j];
	     	    printf("%f\t", UD[i][j]);
	        }
	        printf("\n");
        }

	}
	else printf("dgesvd error %d\n", info);

     return (0);

}
