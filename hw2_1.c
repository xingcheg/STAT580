#include<stdio.h>
#include<math.h>
#define P0 0.01
#define P1 0.5
#define PLEN 10
#define N 5

int factorial(int x){
	int i, factorial;
	factorial=1;
	for (i=1; i<=x; i++){
		factorial = factorial * i;
	}
	return (factorial);
}

int choose(int n, int x){
	int c;
	c = factorial(n)/( factorial(x)*factorial(n-x) );
		return (c);
}

double bin_pmf(int n, int x, double p){
	double pmf;
	pmf = choose(n,x) * pow(p,x) * pow(1-p,n-x);
	return (pmf);
}


int main(){
	int x, i, j;
	double p, q, pmf;
	q = (P1 - P0)/(PLEN - 1);
	printf("x\\p\t");
	for (i=0; i<=9; i++){
		printf("%f  ", P0+q*i);
	}
	printf("\n\n");

	for (x=0; x<=N; x++ ){
		printf("%d\t",x);
		for (j=0; j<=9; j++){
			p = P0 + q*j;
			pmf = bin_pmf(N,x,p);
			printf("%f  ",pmf);
		}
		printf("\n");
	}
	return 0;
}







	
