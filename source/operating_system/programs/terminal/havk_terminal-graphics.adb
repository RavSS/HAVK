-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Terminal                         --
-- Filename        -- havk_terminal-graphics.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Terminal.Graphics
WITH
   Refined_State => (Graphics_Output_Protocol_State => NULL)
IS
   -- TODO: This doesn't handle pixel bitmasks and only does
   -- RGB and BGR for now.
   FUNCTION Create_Pixel
     (Red      : IN number;
      Green    : IN number;
      Blue     : IN number)
      RETURN pixel
   IS
      -- Green will always be in-between red and blue (for now).
      Mid_Byte : CONSTANT number RANGE 0 .. 16#FF00# :=
         Shift_Left(Green, 8) AND 16#FF00#;
      Value    : number := Mid_Byte; -- Green on error.
   BEGIN
      IF
         Pixel_Format = BGR
      THEN
         Value := Shift_Left(Red,  16) OR Mid_Byte OR Blue;
      ELSIF
         Pixel_Format = RGB
      THEN
         Value := Shift_Left(Blue, 16) OR Mid_Byte OR Red;
      END IF;

      RETURN pixel(Value);
   END Create_Pixel;

   PROCEDURE Draw_Fill
     (Pixel_Start  : IN number;
      Pixel_End    : IN number;
      Pixel_Colour : IN pixel)
   IS
   BEGIN
      FOR
         Index IN number RANGE Pixel_Start .. Pixel_End
      LOOP
         Framebuffer(Index) := Pixel_Colour;
      END LOOP;
   END Draw_Fill;

   PROCEDURE Draw_Horizontal_Line
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Line_Size    : IN number)
   IS
      Line_Length : CONSTANT number := Pixel_Start + Line_Size;
   BEGIN
      FOR
         Index IN number RANGE Pixel_Start .. Line_Length
      LOOP
         Framebuffer(Index) := Pixel_Colour;
      END LOOP;
   END Draw_Horizontal_Line;

   PROCEDURE Draw_Vertical_Line
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Line_Size    : IN number)
   IS
      Index : number;
   BEGIN
      FOR
         Line_Index IN number RANGE 1 .. Line_Size
      LOOP
         Index := Pixel_Start + Line_Index * Screen_Width;

         IF
            Index <= Framebuffer_Elements
         THEN
            Framebuffer(Index) := Pixel_Colour;
         ELSE
            RETURN;
         END IF;
      END LOOP;
   END Draw_Vertical_Line;

   PROCEDURE Draw_Box
     (Pixel_Start  : IN number;
      Pixel_Colour : IN pixel;
      Box_Width    : IN number;
      Box_Height   : IN number)
   IS
      Top_Line_End      : CONSTANT number := Pixel_Start + Box_Width;
      Bottom_Line_Start : CONSTANT number :=
         Pixel_Start + Screen_Width * Box_Height;
   BEGIN
      IF -- Top left corner towards down.
         Pixel_Start + Box_Height * Screen_Width <= Framebuffer_Elements
      THEN
         Draw_Vertical_Line(Pixel_Start, Pixel_Colour, Box_Height);
      END IF;

      IF -- Top left corner to across.
         Pixel_Start + Box_Width <= Framebuffer_Elements
      THEN
         Draw_Horizontal_Line(Pixel_Start, Pixel_Colour, Box_Width);
      END IF;

      IF -- Top right corner towards down.
         Top_Line_End + Box_Height * Screen_Width <= Framebuffer_Elements
      THEN
         Draw_Vertical_Line(Top_Line_End, Pixel_Colour, Box_Height);
      END IF;

      IF -- Bottom left corner to across.
         Bottom_Line_Start + Box_Width <= Framebuffer_Elements
      THEN
         Draw_Horizontal_Line(Bottom_Line_Start, Pixel_Colour, Box_Width);
      END IF;
   END Draw_Box;

   FUNCTION Calculate_Pixel
     (Width  : IN number;
      Height : IN number)
      RETURN number
   IS
      Index : CONSTANT number := Width * Pixel_Size + Height * Screen_Width;
   BEGIN
      IF
         Index >= Framebuffer_Elements
      THEN
         RETURN Framebuffer_Elements; -- Last element if out-of-bounds.
      ELSE
         RETURN Index;
      END IF;
   END Calculate_Pixel;
BEGIN
   -- Get the framebuffer information before the package finishes elaborating.
   System_Call(Display_Data);

   Framebuffer_Address  := address(Display_Data.Argument_1);
   Framebuffer_Elements := number(Display_Data.Argument_2 / 4 - 1);
   Screen_Width         := number(Shift_Right(Display_Data.Argument_3, 32));
   Screen_Height        := number(Display_Data.Argument_3 AND 2**32 - 1);
   Pixel_Size           := number(Display_Data.Argument_4) / Screen_Width;
   Pixel_Format         := pixel_formats'enum_val(Display_Data.Argument_5);
   Framebuffer          := To_Pointer(Framebuffer_Address);
END HAVK_Terminal.Graphics;
