#include <stdio.h>
#include <time.h>
#define MATHLIB_STANDALONE
#include <Rmath.h>

int main(){
	double U, X;

	set_seed(time(NULL),580580);
	U = unif_rand();
	X = exp( log(10)*U );
	printf("%f\n", X);
	return (0);
}
