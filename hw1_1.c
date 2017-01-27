#include<stdio.h>
int main()
{
	int lower, upper, num;
	double c, f, step;
	lower = 0;
	upper = 200;
	printf("input the number of row\n");
	scanf("%d",&num);
	if (num < 2)
	{
		printf("The number of row should be at least 2.\n");
	}
	else
	{
		step=200/((double)num-1);
		f=lower;
		printf("F\tC\n");
		while (f<=upper + 0.0001)
		{
			c= 5 * (f - 32) / 9;
			printf("%f\t%f\n",f,c);
			f = f + step;
		}
	}
	return 0;
}


