#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPS 0.001

void dgesvd_(char *JOBU, char *JOBVT, int *M, int *n, double *A, int *LDA,
double *S, double *U, int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *INFO);	


int main(int argc, char *argv[]){
	FILE *f;
	
	if (argc != 3){
		printf("Error in Format.\n");
		printf(" data: data file\n");
		printf(" lambda: a tuning parameter\n");
		return 1;
	}
	
	/* open the file */
	if((f = fopen(argv[1], "r")) == NULL){
		printf("The data file cannot be opened.\n");
		return 1;
	}
	
	/* get dimensions of the data, set number of rows by N, columns by P */
	int N, P;
	fscanf(f, "%d", &N);
	fscanf(f, "%d\n", &P);

	/* read the data */
	int i=0,j,k;
	double X[N*P];
	while( fscanf(f, "%lf", X+i)==1 ){
		i++;
	}

	/* get lambda to conduct soft-impute */
	char *lambda1 = argv[2];
	double lambda = atof(lambda1);
	
	/* set initial Zold */
	double Zold[N*P], Zpro[N*P], Zlam[N*P], Znew[N*P];
	for(i=0; i<N*P; i++){
		Zold[i] = 0;
		Zpro[i]	= 0;	
	}
	
	double ratio = 1, Fdiff, Fold;
	char *jobu;
	char *jobvt;
	jobu = "A";
	jobvt ="A";
	int m = N, n=P, lwork=5*N, info;
	double s[P], u[N*P], vt[P*P], work[lwork];
	
	while(ratio > EPS){	
		/* get the orthogonal projection matrix on missing value */
		for(i=0; i<N*P; i++){
			if(fabs(X[i])< 1.0E-6){
				Zpro[i] = Zold[i];
			}
		}
		
		/* add two projection matrix together */
		for(i=0; i<N*P; i++){
			Zlam[i] = X[i] +Zpro[i];
		}
		
		/* get the SVD of Zlam */
		dgesvd_(jobu, jobvt, &m, &n, Zlam, &m, s, u, &m, vt, &n, work, &lwork, &info);	
        	
        for(i=0; i<P; i++){
			if(s[i]-lambda > 1.0E-6){
				s[i] = s[i] -lambda;
			}
			else{
				s[i] = 0;
			}
		}

		
		/* the principal component scores U(S-lambda)  */
		for (j=0; j<P; j++){
			for (i=0; i<N; i++){
				u[j*N+i] = u[j*N+i] * s[j];
			}
		} 
		
		/* calculate U%*%S%*%VT  */
		for(i=0; i<N*P; i++){
			Znew[i] = 0;
		}
		for (j=0; j<P; j++){
		for (i=0; i<N; i++){
		for(k=0; k<P; k++){
			Znew[j*N+i] += u[k*N+i] * vt[j*P+k];
		}
		}
		}
		
		/* get the squared F-norm of the difference between Znew and Zold */	
		Fdiff = 0, Fold = 0;
		for(i=0; i<N*P; i++){
			Fdiff += (Znew[i]-Zold[i])*(Znew[i]-Zold[i]);
			Fold += Zold[i]*Zold[i];
		}
		ratio = Fdiff/Fold;
		
		/* replace Zold by Znew */
		for(i=0; i<N*P; i++){
			Zold[i] = Znew[i];
		}	
	}
	
	for(i=0; i<N; i++){
	for(j=0; j<P; j++){
	printf("%lf\t", Znew[i*P+j]);	
	}
	printf("\n");
	}
	
	return 0;
}
	



	