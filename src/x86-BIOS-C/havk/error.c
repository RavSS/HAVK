#include <havk/error.h>

/* TODO: Eventually, I want this file to house error codes. */

void __stack_chk_fail(void)
{
	emergency("Stack smashing detected.");
}

void __chk_fail(void)
{
	emergency("Buffer overflow detected.");
}

void emergency(const char_ht *information)
{
	TERMINAL_FOREGROUND = white;
	TERMINAL_BACKGROUND = light_blue;
	clear();
	cursor_remove();

	shift(VGA_WIDTH_MAX / 2 - 9, 10);
	print("UNRECOVERABLE ERROR\r\t\t");
	
	print(information);

	while (1) { __asm__ volatile ("CLI; HLT;"); }
}