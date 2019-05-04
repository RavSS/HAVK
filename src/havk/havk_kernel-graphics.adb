WITH
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Bitwise;
USE
   HAVK_Kernel.Exceptions,
   HAVK_Kernel.Bitwise;

PACKAGE BODY HAVK_Kernel.Graphics
IS
   -- TODO: Make a byte (u8) subtype for a pixel that goes from
   -- 0x0 to 0xFFFFFF. This may be a bad idea if the current system uses
   -- specific bit masks for the pixel that don't end at those values.

   -- TODO: This doesn't handle pixel bit masks and only does
   -- RGB and BGR for now.
   FUNCTION Create_Pixel(
      Red : IN u8;
      Green : IN u8;
      Blue : IN u8)
   RETURN u32 IS
      -- Green will always be in-between red and blue.
      Mid_Byte : CONSTANT u32 := Lsh32(u32(Green), 8);
      Value : u32;
   BEGIN
      IF Pixel_Format = BGR THEN
         Value := Lsh32(u32(Red), 16) OR Mid_Byte OR u32(Blue);
      ELSIF Pixel_Format = RGB THEN
         Value := Lsh32(u32(Blue), 16) OR Mid_Byte OR u32(Red);
      ELSE
         Value := 16#DEADC0DE#; -- Incorrect format error.
      END IF;

      RETURN Value;
   END Create_Pixel;

   FUNCTION Calculate_Pixel(
      Width : IN u32;
      Height : IN u32)
   RETURN u64 IS
      Pixel_X : CONSTANT u64 := u64(Width) * u64(Pixel_Size);
      Pixel_Y : CONSTANT u64 := u64(Height) * u64(Screen_Width);
      Pixel : CONSTANT u64 := Pixel_X + Pixel_Y;
   BEGIN
      RETURN Pixel;
   END Calculate_Pixel;

   PROCEDURE Draw_Fill(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_End : IN u64;
      Pixel_Colour : IN u32)
   IS
   BEGIN
      FOR I IN u64 RANGE Pixel_Start .. Pixel_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   END Draw_Fill;

   PROCEDURE Draw_Pixel(
      Buffer : IN OUT framebuffer;
      Pixel : IN u64;
      Pixel_Colour : IN u32)
   IS
   BEGIN
      Buffer(Pixel) := Pixel_Colour;
   EXCEPTION
      WHEN Constraint_Error => RETURN;
   END Draw_Pixel;

   PROCEDURE Draw_Horizontal_Line(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Line_Size : IN u64)
   IS
      Line_Length : CONSTANT u64 := Pixel_Start + Line_Size;
   BEGIN
      FOR I IN u64 RANGE Pixel_Start .. Line_Length LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   EXCEPTION
      WHEN Constraint_Error => RETURN;
   END Draw_Horizontal_Line;

   PROCEDURE Draw_Vertical_Line(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Line_Size : IN u64)
   IS
   BEGIN
      FOR I IN u64 RANGE 1 .. Line_Size LOOP
         Buffer(Pixel_Start + I * u64(Screen_Width)) :=
            Pixel_Colour;
      END LOOP;
   EXCEPTION
      WHEN Constraint_Error => RETURN;
   END Draw_Vertical_Line;

   PROCEDURE Draw_Box(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      Box_Width : IN u32;
      Box_Height : IN u32)
   IS
      Top_Line_End : CONSTANT u64 := Pixel_Start + u64(Box_Width);

      Bottom_Line_Start : CONSTANT u64 :=
         Pixel_Start + u64(Screen_Width) * u64(Box_Height);
      Bottom_Line_End : CONSTANT u64 :=
         Bottom_Line_Start + u64(Box_Width);
   BEGIN
      -- Top left down.
      FOR I IN u64 RANGE 1 .. u64(Box_Height) LOOP
         Buffer(Pixel_Start + I * u64(Screen_Width)) :=
            Pixel_Colour;
      END LOOP;

      -- Top left across.
      FOR I IN u64 RANGE Pixel_Start .. Top_Line_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;

      -- Top right down.
      FOR I IN u64 RANGE 1 .. u64(Box_Height) LOOP
         Buffer(Top_Line_End + I *
            u64(Screen_Width)) := Pixel_Colour;
      END LOOP;

      -- Bottom left across.
      FOR I IN u64 RANGE Bottom_Line_Start .. Bottom_Line_End LOOP
         Buffer(I) := Pixel_Colour;
      END LOOP;
   EXCEPTION
      WHEN Constraint_Error => RETURN;
   END Draw_Box;
END HAVK_Kernel.Graphics;

