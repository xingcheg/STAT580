#include<stdio.h>
#include<stdlib.h>
#include<assert.h>
#include <string.h>

void dgesv_(int *N, int *NRHS, double *A, int *LDA, int *IPIV,
                 double *B, int *LDB, int *INFO);

void dgemm_(char *TRANSA, char *TRANSB, int *M, int *N, int *K,
		double *ALPHA, double *A, int *LDA, double *B, int *LDB,
		double *BETA, double *C, int *LDC);

int main(int argc, char *argv[]){
	int i, j;
	int n = 0;
	int p = 0;
	int c;
	FILE *f;
	double *Y, *X, *X1, *XX, *XY;
	int *ipiv;
	char trans = 'T', no_trans = 'N';
    double alpha = 1, beta = 0;
	int K, N_X, N_Y, info;


	if (argc != 3){
         printf("This program is designed to calculate the regression coefficients.\n");
         printf("You need to:\n");
         printf("1. input the name of data file.\n");
         printf("2. input 1 or 0------(1=intercept, 0=no intercept). \n");
         return (1);
       }


	f = fopen(argv[1],"r");

	if (f==NULL) {
		printf("Error opening file.\n");
		return (1);
	}

      c = fgetc(f);                       
	  while (c != EOF){
		  if (c == ' ') p++;
		  if (c == '\n') n++;
		  c = fgetc(f);
	  }

	  p = p/n;     /*line 42-49 used to calculate ncol & nrow: ncol = p; nrow = n. */
      Y = (double*) malloc(sizeof(double) * n);
	  assert(Y != NULL);
	  X = (double*) malloc(sizeof(double) * (n*(p+1)) );
	  assert(X != NULL);

	  rewind(f);  /* reset the file pointer.*/

	  for (i=0; i<n; i++){
		  X[i] = 1;
		  fscanf(f, "%lf ", &(Y[i]) );
		  for (j=1; j<p; j++){
			  fscanf(f, "%lf ", &(X[j*n+i]) );
		  }
		  fscanf(f, "%lf\n", &(X[p*n+i]) );
	  }           /*assign the first column of the data to Y; assign the rest of the columns to X.*/

	K = n;
	N_Y = 1;

if (atoi(argv[2]) == 1){   /*solve the normal equation(with intercept):X'Xb=X'y  */
      N_X = p+1;
      XX = (double*) malloc(sizeof(double) * (p+1)*(p+1));
	  assert(XX != NULL);
      XY = (double*) malloc(sizeof(double) * (p+1));
	  assert(XY != NULL);
	  ipiv = (int*) malloc(sizeof(int) * (p+1));
	  assert(ipiv != NULL);

	dgemm_(&trans, &no_trans, &N_X, &N_X, &K, &alpha, X, &K, X,
			&K, &beta, XX, &N_X);

	dgemm_(&trans, &no_trans, &N_X, &N_Y, &K, &alpha, X, &K, Y,
			&K, &beta, XY, &N_X);

	dgesv_(&N_X, &N_Y, XX, &N_X, ipiv, XY, &N_X, &info);

	if (info == 0){
		printf("The regression coefficients (with intercept):\t");
		for (i = 0; i<N_X; i++){
			printf("%f\t", XY[i]);
		}
      printf("\n");
	}
	else printf("dgesv error %d\n", info);
}
else if (atoi(argv[2]) == 0){  /*solve the normal equation(without intercept):X'Xb=X'y  */
	  N_X = p;
	  XX = (double*) malloc(sizeof(double) * p*p);
	  assert(XX != NULL);
      XY = (double*) malloc(sizeof(double) * p);
	  assert(XY != NULL);
	  ipiv = (int*) malloc(sizeof(int) * p);
	  assert(ipiv != NULL);
	  X1 = (double*) malloc(sizeof(double) * n*p);
	  assert(X1 != NULL);

	for (i=0; i<n*p; i++){
		X1[i] = X[i+n];
	}

	dgemm_(&trans, &no_trans, &N_X, &N_X, &K, &alpha, X1, &K, X1,
			&K, &beta, XX, &N_X);

	dgemm_(&trans, &no_trans, &N_X, &N_Y, &K, &alpha, X1, &K, Y,
			&K, &beta, XY, &N_X);

	dgesv_(&N_X, &N_Y, XX, &N_X, ipiv, XY, &N_X, &info);

	if (info == 0){
		printf("The regression coefficients (without intercept):\t");
		for (i = 0; i<N_X; i++){
			printf("%f\t", XY[i]);
		}
      printf("\n");
	}
	else printf("dgesv error %d\n", info);
}
else{
	printf("You should input 1/0 to calculate the regression coefficients with/without intercept.\n");
}

fclose(f);
return (0);
}


