#include <havk/string.h>

size_t strlen(const char_ht *string)
{
	size_t length;

	length = 0;

	while (string[length])
		++length;

	return length;
}

size_t h_strncpy(char_ht *destination, const char_ht *source, size_t bytes)
{
	size_t i;
	size_t b;

	b = bytes - 1;

	for (i = 0; i < b; ++i)
	{
		if (source[i])
			destination[i] = source[i];
		else break;
	}

	destination[++i] = '\0';

	return i;
}

char_ht *strcat(char_ht *destination, const char_ht *source)
{
	size_t bytes;

	bytes = strlen(destination);
	h_strncpy(destination + bytes, source, bytes);

	return destination;
}
