#include<stdio.h>
#define N 10

int main(){
	
	double x[N] = {3.1, -1.2, 5.3, 1, 4.4, 21, 3, 7, -1.2, 3.2};
	double *p, *q, *r;
	int i;
	double temp;

	printf("Original data:\n");
	for (i=0; i<N; i++){
		printf("%f\n",x[i]);
	}


	for (i=1; i<N; i++){
		r = &x[i];
        p = r;
	    q = r-1;
		while (*p < *q){
				temp = *p;
				*p = *q;
				*q = temp;
				if (q == x) break;
				p--;
				q--;
		}
	}

	printf("Sorted data:\n");
	for (i=0; i<N; i++){
		printf("%f\n",x[i]);
	}
	printf("\n");

	return (0);
}








