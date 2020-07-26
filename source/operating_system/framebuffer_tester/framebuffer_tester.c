///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System Framebuffer Tester               //
// Filename        -- framebuffer_tester.c                                   //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <havk/havk.h>

volatile uint32_t *framebuffer;
uint32_t framebuffer_size;
uint32_t framebuffer_width;
uint32_t framebuffer_height;
uint32_t scanline_width;
uint32_t pixel_width;
uint8_t pixel_type;

#define PIXEL_POSITION(x, y) ((x) * pixel_width + (y) * scanline_width)
#define CLEAR_SCREEN() memset((void *)framebuffer, 0, framebuffer_size)

// Taken from Wikipedia (modified state handling).
uint64_t xorshift128plus(void)
{
	static uint64_t a = 123, b = 321;
	uint64_t t = a;
	const uint64_t s = b;

	a = s;
	t ^= t << 23;
	t ^= t >> 17;
	t ^= s ^ (s >> 26);
	b = t;

	return t + s;
}

// Poor man's delay.
void delay(uintmax_t value)
{
	for (register volatile uintmax_t i = 0; i < value; ++i)
	{
		__builtin_ia32_pause();
	}
}

// Draws a line on the Y axis. First parameter is a zero-based index.
void vertical_line(uint32_t start, uint32_t pixel, uint32_t length)
{
	for (; length; --length, start += scanline_width)
	{
		framebuffer[start] = pixel;
	}
}

// Draws a line on the X axis. First parameter is a zero-based index.
void horizontal_line(uint32_t start, uint32_t pixel, uint32_t length)
{
	for (; length; --length, start += pixel_width)
	{
		framebuffer[start] = pixel;
	}
}

// Draws a square. First parameter is a zero-based index. Width and height
// refer to the amount of pixels drawn in their respective direction.
void box(uint32_t start, uint32_t pixel, uint32_t width, uint32_t height)
{
	// Top left to top right.
	horizontal_line(start, pixel, width);
	// Bottom left to bottom right.
	horizontal_line(start + framebuffer_width * height, pixel, width);
	// Top left to bottom left.
	vertical_line(start, pixel, height);
	// Top right to bottom right. A width of zero is still made valid.
	vertical_line(start + width - (width != 0), pixel, height);
}

// Draws a grid pattern on the entire framebuffer. The number of requested
// lines on both axes must be higher than one.
// TODO: Expand this with line width settings and a starting position. The
// equations below are very likely not correct.
void grid(uint32_t pixel, uint32_t horizontal_lines,
	uint32_t vertical_lines)
{
	const uint32_t horizontal_gap = scanline_width
		* (framebuffer_height / --horizontal_lines);

	const uint32_t vertical_gap = scanline_width / --vertical_lines;

	while (horizontal_lines)
	{
		horizontal_line(horizontal_gap * --horizontal_lines, pixel,
			framebuffer_width);
	}

	while (vertical_lines)
	{
		vertical_line(vertical_gap * --vertical_lines, pixel,
			framebuffer_height);
	}

	horizontal_line(scanline_width * (framebuffer_height - 1), pixel,
		framebuffer_width);
	vertical_line(scanline_width - 1, pixel, framebuffer_height);
}

// Just a random little pattern for now to show, as I no longer have font
// drawing capabilities implemented.
void test_pattern(uint32_t pixel, uintmax_t transition_delay,
	bool inwards_direction)
{
	const uint32_t base = 5, end = 42;

	if (inwards_direction)
	{
		for (uint8_t i = end - 1; i >= base; --i)
		{
			CLEAR_SCREEN();
			grid(pixel, i, i);
			delay(transition_delay);
		}
	}
	else
	{
		for (uint8_t i = base; i < end; ++i)
		{
			CLEAR_SCREEN();
			grid(pixel, i, i);
			delay(transition_delay);
		}
	}
}

uint64_t main(void)
{
	sysargs_ht arguments;

	arguments.operation = FRAMEBUFFER_ACCESS_OPERATION;
	syscall(&arguments);

	framebuffer = (uint32_t *)arguments.argument_1;
	framebuffer_size = arguments.argument_2;
	framebuffer_width = arguments.argument_3 >> 32;
	framebuffer_height = arguments.argument_3 & UINT16_MAX;
	scanline_width = arguments.argument_4;
	pixel_width = scanline_width / framebuffer_width;
	pixel_type = arguments.argument_5;

	while (true)
	{
		test_pattern(xorshift128plus(), INT32_MAX / 100, false);
		test_pattern(xorshift128plus(), INT32_MAX / 100, true);
	}

	return 0;
}
