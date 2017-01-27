#include <stdio.h>
     int main()
{
       int count, c, flag1, flag2;
       count = 0;
	   flag2 = 0;
	   c = getchar();
      
	   while (c != EOF)
	   {
		 if ( (c != ' ') && (c != '\t') && (c != '\n') )
		 {
			 flag1 = 1;
		 }
		 else
		 {
			 flag1 = 0;
		 }
		 
		 if ( (flag1-flag2) == 1 )
		 {
			 count = count + 1;
		 }

		 c = getchar();
		 flag2 = flag1;
       }
      
	   printf("\nNumber of words: %d.\n", count);
       return 0;
}

