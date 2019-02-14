#include <havk/terminal.h>

volatile uint16_t *VGA_BUFFER = (volatile uint16_t*) VGA_MEMORY_ADDRESS;

uint16_t VGA_HEIGHT = 0;
uint16_t VGA_WIDTH = 0;

enum TERMINAL_COLOUR TERMINAL_FOREGROUND = white;
enum TERMINAL_COLOUR TERMINAL_BACKGROUND = black;

void vga_print(const char_ht character, uint8_t foreground_colour,
	uint8_t background_colour)
{
	if (VGA_WIDTH >= VGA_WIDTH_MAX)
	{
		++VGA_HEIGHT;
		VGA_WIDTH = 0;
	}

	if (VGA_HEIGHT >= VGA_HEIGHT_MAX)
	{
		VGA_HEIGHT = VGA_HEIGHT_MAX - 1;
		vga_scroll();
	}

	switch (character)
	{
		/* Let's use '\r' or CR as a line terminator. */
		case '\n': /* Reset the line's current width. */
			VGA_WIDTH = 0;
			return;
		case '\r': /* Roll down a line and reset the width too. */
			++VGA_HEIGHT;
			VGA_WIDTH = 0;
			return;
		case '\f': /* Decided to use this as a line clearer. */
			clear_line(VGA_HEIGHT--);
			return;
		case '\b':
			if (VGA_WIDTH <= 1) --VGA_HEIGHT;
			vga_clear(--VGA_WIDTH, VGA_HEIGHT, 0x0,
				TERMINAL_FOREGROUND, TERMINAL_BACKGROUND);
			return;
		/* TODO: Lazy tab support for now. Not good at all for
		/  formatting, as you can't singly '\b' and erase a tab. */
		case '\t':
			for (uint8_t i = 0; i < 8; ++i)
			{
				vga_print('\x20', foreground_colour,
					background_colour);
			}
			return;
		default: break;
	}

	VGA_BUFFER[VGA_POSITION(VGA_WIDTH++, VGA_HEIGHT)]
		= VGA_COLOUR(character, foreground_colour, background_colour);

	return;
}

void vga_clear(uint8_t width, uint8_t height, const char_ht character,
	uint8_t foreground_colour, uint8_t background_colour)
{
	uint_fast16_t i;
	uint16_t c;

	c = VGA_COLOUR(character, foreground_colour, background_colour);

	if (width > VGA_WIDTH_MAX && height > VGA_HEIGHT_MAX)
	{
		for (i = 0; i < 0x7FFF; ++i)
			VGA_BUFFER[i] = c;
	}
	else if (width > VGA_WIDTH_MAX)
	{
		for (i = 0; i < VGA_WIDTH_MAX; ++i)
			VGA_BUFFER[VGA_POSITION(i, height)] = c;
	}
	else if (height > VGA_HEIGHT_MAX)
	{
		for (i = 0; i < VGA_HEIGHT_MAX; ++i)
			VGA_BUFFER[VGA_POSITION(width, i)] = c;
	}
	else VGA_BUFFER[VGA_POSITION(width, height)] = c;

	return;
}

/* TODO: Save buffers, direction options, scroll amount, etc. */
void vga_scroll(void)
{
	uint_fast8_t width, height;

	for (height = 1; height < VGA_HEIGHT_MAX; ++height)
	{
		for (width = 0; width < VGA_WIDTH_MAX; ++width)
		{
			VGA_BUFFER[VGA_POSITION(width, height - 1)]
				= VGA_BUFFER[VGA_POSITION(width, height)];
		}
	}

	clear_line(VGA_HEIGHT_MAX - 1);
	cursor_move(VGA_WIDTH, VGA_HEIGHT - 1);

	return;
}

size_t print(const char_ht *string)
{
	size_t i;

	if (string == NULL) return 0;

	for (i = 0; string[i]; ++i)
		vga_print(string[i], TERMINAL_FOREGROUND, TERMINAL_BACKGROUND);

	cursor_move(VGA_WIDTH, VGA_HEIGHT);

	return i;
}

/* Would this be better in a macro? Error checking is nice though. */
bool shift(uint8_t width, uint8_t height)
{
	if (width > VGA_WIDTH_MAX || height > VGA_HEIGHT_MAX)
		return false;

	VGA_WIDTH = width;
	VGA_HEIGHT = height;

	return true;
}
