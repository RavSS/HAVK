#include <havk/standard.h>

int32_t atoi(const char_ht *string)
{
	int32_t i;
	int32_t r;

	if (*string == '\0' || string == NULL)
		return 0;

	if (string[0] == '-')
		r = -1;
	else r = 0;

	for (i = 0; string[i]; ++i)
		r = r * 10 + string[i] - '0';

	return r;
}

bool h_utoa(uint32_t number, int16_t base, char_ht *string, size_t length)
{
	size_t i;
	size_t r;
	size_t d;
	char_ht c;

	if (length <= 0 || string == NULL)
		return false;

	i = 0;
	r = length;

	do
	{
		d = number % base;

		if (d < 0xA)
			string[i++] = d + '0';
		else
			string[i++] = d + 'A' - 0xA;
	}
	while ((i < r) && (0 < (number /= base)));

	string[i] = '\0';
	r = i - 1;

	for (i = 0; i < r; ++i, --r)
	{
		c = string[r];
		string[r] = string[i];
		string[i] = c;
	}

	return true;
}

bool h_itoa(int32_t number, int16_t base, char_ht *string,
	size_t length, bool sign)
{
	size_t i;
	size_t r;
	size_t d;
	char_ht c;
	bool negative;

	if (length <= 0 || string == NULL)
		return false;

	i = 0;
	r = length - 1;

	if (number < 0)
	{
		negative = true;
		number = -number;
	}

	do
	{
		d = number % base;

		if (d < 0xA)
			string[i++] = d + '0';
		else
			string[i++] = d + 'A' - 0xA;
	}
	while ((i < r) && (0 < (number /= base)));

	if (sign)
	{
		if (negative)
			string[i++] = '-';
		else string[i++] = '+';
	}

	string[i] = '\0';
	r = i - 1;

	for (i = 0; i < r; ++i, --r)
	{
		c = string[r];
		string[r] = string[i];
		string[i] = c;
	}

	return true;
}

void *memcpy(void *destination, const void *source, size_t length)
{
	char *d;
	const char *s;

	d = destination;
	s = source;

	while (length--)
		*d++ = *s++;

	return destination;
}

void sleep(uint32_t seconds, bool fast)
{
	uint_fast64_t left;

	left = tick + (seconds * tick_frequency);

	if (fast)
	{
		/* Spin-loop speed increase via the "PAUSE" instruction.
		/  Also causes 100% CPU usage, which is nearly 20 times as
		/  much CPU usage compared to the "HLT" instruction. */
		while (left > tick)
			__asm__ volatile ("PAUSE");
	}
	else
	{
		/* Only check the condition after a tick. */
		while (left > tick)
			__asm__ volatile ("HLT");
	}
}

char_ht h_getchar(char_ht character)
{
	do
	{
		ps2_key = 0;
		__asm__ volatile ("HLT"); /* Key only updated on interrupt. */
	}
	while (ps2_key != character);

	return ps2_key;
}