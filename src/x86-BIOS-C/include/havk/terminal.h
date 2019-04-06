#ifndef TERMINAL_H
#define TERMINAL_H

#include <havk.h>
#include <havk/io.h>

/* Obviously assumes paging has been set up. */
#define VGA_MEMORY_ADDRESS 0xC00B8000

#define VGA_COLOUR_BLACK 0
#define VGA_COLOUR_BLUE 1
#define VGA_COLOUR_GREEN 2
#define VGA_COLOUR_CYAN 3
#define VGA_COLOUR_RED 4
#define VGA_COLOUR_MAGENTA 5
#define VGA_COLOUR_BROWN 6
#define VGA_COLOUR_LIGHT_GREY 7
#define VGA_COLOUR_DARK_GREY 8
#define VGA_COLOUR_LIGHT_BLUE 9
#define VGA_COLOUR_LIGHT_GREEN 10
#define VGA_COLOUR_LIGHT_CYAN 11
#define VGA_COLOUR_LIGHT_RED 12
#define VGA_COLOUR_LIGHT_MAGENTA 13
#define VGA_COLOUR_LIGHT_BROWN 14
#define VGA_COLOUR_WHITE 15

enum TERMINAL_COLOUR
{
	black = VGA_COLOUR_BLACK,
	blue = VGA_COLOUR_BLUE,
	green = VGA_COLOUR_GREEN,
	cyan = VGA_COLOUR_CYAN,
	red = VGA_COLOUR_RED,
	magenta = VGA_COLOUR_MAGENTA,
	brown = VGA_COLOUR_BROWN,
	light_grey = VGA_COLOUR_LIGHT_GREY,
	dark_grey = VGA_COLOUR_DARK_GREY,
	light_blue = VGA_COLOUR_LIGHT_BLUE,
	light_green = VGA_COLOUR_LIGHT_GREEN,
	light_cyan = VGA_COLOUR_LIGHT_CYAN,
	light_red = VGA_COLOUR_LIGHT_RED,
	light_magenta = VGA_COLOUR_LIGHT_MAGENTA,
	light_brown = VGA_COLOUR_LIGHT_BROWN,
	white = VGA_COLOUR_WHITE
};

extern enum TERMINAL_COLOUR TERMINAL_FOREGROUND;
extern enum TERMINAL_COLOUR TERMINAL_BACKGROUND;

/* x = character, y = foreground, z = background. */
#define VGA_COLOUR(x, y, z) ((x) | ((y) | (z) << 4) << 8)

/* x = X-axis (width), y = Y-axis (height). */
#define VGA_POSITION(x, y) ((y) * (VGA_WIDTH_MAX) + (x))

#define VGA_WIDTH_MAX 80
#define VGA_HEIGHT_MAX 25

extern uint16_t VGA_HEIGHT;
extern uint16_t VGA_WIDTH;

/* Prints a single character with a colour for the foreground and
/  the background. */
void vga_print(const char_ht character, uint8_t foreground_colour,
	uint8_t background_colour);

/* Clears a specified character position at a specified line. If either
/  of the positional values are above their maximum ones, then that
/  line and/or column is completely wiped. */
void vga_clear(uint8_t width, uint8_t height, char_ht character,
	uint8_t foreground_colour, uint8_t background_colour);

/* Similiar to `puts()`. Prints a string and that's it. */
size_t print(const char_ht *string);

/* Just a line-shortening macro for printing a single character. */
#define printc(x) vga_print((x), TERMINAL_FOREGROUND, TERMINAL_BACKGROUND)

/* Shifts to a specific position. If either of the values are above the
/  maximum for their respective axis, then false is returned as an error. */
bool shift(uint8_t width, uint8_t height);

/* Scrolls down one line. Needs more features and options. */
void vga_scroll(void);

/* Clears the entire buffer with the preselected colours. */
#define clear() vga_clear(VGA_WIDTH_MAX + 1, VGA_HEIGHT_MAX + 1, 0x0,\
	TERMINAL_FOREGROUND, TERMINAL_BACKGROUND)

/* Clears an entire line completely. */
#define clear_line(y) vga_clear(VGA_WIDTH_MAX + 1, (y), 0x0,\
	TERMINAL_FOREGROUND, TERMINAL_BACKGROUND)

/* Clears an entire column completely. */
#define clear_column(x) vga_clear((x), VGA_HEIGHT_MAX + 1, 0x0,\
	TERMINAL_FOREGROUND, TERMINAL_BACKGROUND)

/* https://wiki.osdev.org/Text_Mode_Cursor */
/* Removes the cursor from the screen via hardware access. */
#define cursor_remove() do\
{\
	out_byte(0x0A, 0x3D4);\
	out_byte(0x20, 0x3D5);\
} while (0)

/* Moves the cursor to anywhere on the screen via hardware access. */
#define cursor_move(x, y) do\
{\
	out_byte(0x0F, 0x3D4);\
	out_byte((VGA_POSITION((x), (y)) & 0xFF), 0x3D5);\
	out_byte(0x0E, 0x3D4);\
	out_byte(((VGA_POSITION((x), (y)) >> 8) & 0xFF), 0x3D5);\
} while (0)

/* A macro for a little bit more coherency. */
#define cursor_update() cursor_move(VGA_WIDTH, VGA_HEIGHT - 1)

#endif
