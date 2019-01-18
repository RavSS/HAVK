#include <vga.h>

void vga_print(char *string)
{
	unsigned short i;
	unsigned short *vga_buffer;
	unsigned short length;

	vga_buffer = (unsigned short *) 0xB8000;
	length = strlen(string);
	
	for (i = 0; i < length; i++)
	{
		vga_buffer[i] = colour(string[i], VGA_COLOUR_LIGHT_CYAN, VGA_COLOUR_GREEN);
	}
}