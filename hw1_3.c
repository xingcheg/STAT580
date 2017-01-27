#include<stdio.h>
int main()
{
	int c;
	c=getchar();
	while (c != EOF)
	{
		if (c>=97 && c<=120)
		{
			c = c+2;
		}
		
		putchar(c);

		c=getchar();

	}
	return 0;
}
