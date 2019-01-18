#ifndef VGA_H
#define VGA_H

#include <stdhavk.h>

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

#define colour(x, y, z) (x | (y | z << 4) << 8)

void vga_print(char *string);

#endif