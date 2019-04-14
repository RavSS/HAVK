WITH HAVK_Kernel;
WITH HAVK_Kernel.UEFI;
WITH HAVK_Kernel.Graphics;

USE HAVK_Kernel;
USE HAVK_Kernel.UEFI;
USE HAVK_Kernel.Graphics;

PROCEDURE HAVK IS
	Arguments : Access_UEFI_Arguments;
	PRAGMA Import(C, Arguments);

	Screen_Framebuffer :
		bytes(0 .. natural(Arguments.Framebuffer_Size) - 1);
	FOR Screen_Framebuffer'Address USE Arguments.Framebuffer_Address;
	PRAGMA Import(Ada, Screen_Framebuffer);

	Scanline_Size : CONSTANT natural :=
		natural(Arguments.Horizontal_Resolution);

	Pixel_Size : CONSTANT natural :=
		(Scanline_Size / natural(Arguments.Pixels_Per_Scanline));
BEGIN
	-- TODO: This call causes the last chance handler to be raised.
	-- It's due to a constraint error, the framebuffer size does not
	-- seem to be correct, as it's clearly not 128.
	Fill_Framebuffer(Screen_Framebuffer, 16#66#);
END HAVK;
