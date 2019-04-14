WITH HAVK_Kernel;

USE HAVK_Kernel;

PACKAGE HAVK_Kernel.Graphics
IS
	PROCEDURE Fill_Framebuffer(
		Framebuffer : IN OUT bytes;
		Fill : IN byte);

	PROCEDURE Print_Pixel(
		Framebuffer : IN OUT bytes;
		Width : IN natural;
		Height : IN natural;
		Scanline_Size : IN natural;
		Pixel_Size : IN natural;
		Colour : IN byte);
END HAVK_Kernel.Graphics;
