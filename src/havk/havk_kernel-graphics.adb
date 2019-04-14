WITH HAVK_Kernel.Exceptions;

USE HAVK_Kernel.Exceptions;

PACKAGE BODY HAVK_Kernel.Graphics
IS
	PROCEDURE Fill_Framebuffer(
		Framebuffer : IN OUT bytes;
		Fill : IN byte)
	IS
	BEGIN
		-- Could this be optimized much further?
		FOR I IN 0 .. Framebuffer'Size LOOP
			Framebuffer(I) := Fill;
		END LOOP;
	END Fill_Framebuffer;

	PROCEDURE Print_Pixel(
		Framebuffer : IN OUT bytes;
		Width : IN natural;
		Height : IN natural;
		Scanline_Size : IN natural;
		Pixel_Size : IN natural;
		Colour : IN byte)
	IS
		-- TODO: I'm very unfamiliar with the calculations required,
		-- so this procedure doesn't work as intended. I'm close?
		Pitch : CONSTANT natural := Scanline_Size * Pixel_Size;
		Pixel : CONSTANT natural := Height * Pitch + Width * Pixel_Size;
	BEGIN
		Framebuffer(Pixel) := Colour;
	END Print_Pixel;
END HAVK_Kernel.Graphics;
