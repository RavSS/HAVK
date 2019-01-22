#include <havk.h>
#include <havk/string.h>
#include <havk/terminal.h>
#include <havk/io.h>

void kernel(void)
{
	TERMINAL_FOREGROUND = light_red;
	TERMINAL_BACKGROUND = blue;

	clear();
	print("\t\t\t\t\n\n\n\n\n\n\n\n\n\n\n\n"); /* Center-text. */

	print("WELCOME TO HAVK\n\r");

	print("\t\t\t    This operating system is\n\r");

	print("\t\t\t still under early development.\n\r");

	while (1);
}
