#include <havk/string.h>

size_t strlen(const char_ht *string)
{
	size_t length;
	
	length = 0;
	
	while (string[length])
		++length;
	
	return length;
}

size_t sstrncpy(char_ht *destination, const char_ht *source, size_t bytes)
{
	size_t i;

	for (i = 0; i < bytes; ++i)
		destination[i] = source[i];
	
	destination[++i] = '\0';
	
	return i;
}
