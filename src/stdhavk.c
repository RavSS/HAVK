#include <stdhavk.h>

unsigned short strlen(char *string)
{
	unsigned short length;
	
	length = 0;
	
	while (string[length])
		++length;
	
	return length;
}
