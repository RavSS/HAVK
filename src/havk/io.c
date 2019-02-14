#include <havk/io.h>

void _c_char_format(const char_ht arg)
{
	printc(arg);
}

void _s_string_format(const char_ht *arg)
{
	print(arg);
}

static void _d_decimal_format(const int32_t arg)
{
	char dec[INT32_MAX_DIGITS + 1];

	h_itoa(arg, 10, dec, INT32_MAX_DIGITS, 0);

	print(dec);
}

void _D_decimal_format(const uint32_t arg)
{
	char dec[UINT32_MAX_DIGITS + 1];

	h_utoa(arg, 10, dec, UINT32_MAX_DIGITS);

	print(dec);
}

void _h_hexidecimal_format(const int32_t arg)
{
	char hex[INT32_MAX_HEX_DIGITS + 1];

	h_itoa(arg, 16, hex, INT32_MAX_HEX_DIGITS, 0);

	print(hex);
}

void _H_hexidecimal_format(const uint32_t arg)
{
	char hex[UINT32_MAX_HEX_DIGITS + 1];

	h_utoa(arg, 16, hex, UINT32_MAX_HEX_DIGITS);

	print(hex);
}

void _p_pointer_format(const uintptr_t arg)
{
	#if UINTPTR_MAX == UINT32_MAX
		char ptr[UINT32_MAX_HEX_DIGITS + 1];
		h_utoa(arg, 16, ptr, UINT32_MAX_HEX_DIGITS);
	#else
		char ptr[UINT64_MAX_HEX_DIGITS + 1];
		h_utoa(arg, 16, ptr, UINT64_MAX_HEX_DIGITS);
	#endif

	print(ptr);
}

void printf(const char_ht *restrict string_format, ...)
{
	size_t i;

	va_list args;
	va_start(args, string_format);

	for (i = 0; string_format[i]; ++i) if (string_format[i] == '%')
	{
		switch (string_format[i + 1])
		{
			case 'c':
				_c_char_format(va_arg(args, int32_t));
				break;
			case 's':
				_s_string_format(va_arg(args, char_ht*));
				break;
			case 'd':
				_d_decimal_format(va_arg(args, int32_t));
				break;
			case 'D':
				_D_decimal_format(va_arg(args, uint32_t));
				break;
			case 'h':
				_h_hexidecimal_format(va_arg(args, int32_t));
				break;
			case 'H':
				_H_hexidecimal_format(va_arg(args, uint32_t));
				break;
			case 'p':
				_p_pointer_format(va_arg(args, uintptr_t));
				break;
			case '%':
				printc('%');
				break;
			default:
				printf("%%%c_ERROR", string_format[i + 1]);
				break;
		} ++i;
	}
	else printc(string_format[i]);

	return;
}

/* GCC (or rather GAS) doesn't recognize "OUT BYTE", which is proper Intel
/  syntax, but it still recognizes "OUTB", which is AT&T syntax. This is odd
/  since I'm explicitly using Intel syntax. Same goes for `in_byte()`. */
void out_byte(uint8_t value, uint16_t port)
{
	__asm__ volatile("OUTB %1, %0" :: "a" (value), "Nd" (port));
}

uint8_t in_byte(uint16_t port)
{
	uint8_t read;

	__asm__ volatile("INB %0, %1" : "=a" (read) : "Nd" (port));

	return read;
}
