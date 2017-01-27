#include<stdio.h>
int main()
{
	int c;
	c=getchar();
	while (c != EOF)
	{
		if (c>=48 && c<=57)
		{
			putchar(c);
		}
		c=getchar();
	}
	return 0;
}
