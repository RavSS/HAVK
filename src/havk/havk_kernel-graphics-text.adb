WITH
   HAVK_Kernel.Font,
   HAVK_Kernel.Bitwise;
USE
   HAVK_Kernel.Font,
   HAVK_Kernel.Bitwise;

PACKAGE BODY HAVK_Kernel.Graphics.Text IS
   PROCEDURE Draw_Character(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      ASCII : IN character)
   IS
      I : CONSTANT u8 := character'pos(ASCII);
      Pixel_Location : u64;
      X_Line : u8;
   BEGIN
      FOR Y IN u8 RANGE 0 .. Framefont_Height - 1 LOOP
         X_Line := Framefont(I, Y);

         -- Move down a scanline if Y is more than 0.
         Pixel_Location := Pixel_Start + (u64(Y) * u64(Screen_Width));

         FOR X IN REVERSE u8 RANGE 0 .. Framefont_Width - 1 LOOP
            -- Move to the next pixel by incrementing
            -- the pixel location.
            Pixel_Location := Pixel_Location + u64(Pixel_Size);

            IF Bt(u64(X_Line), X) THEN
               Buffer(Pixel_Location) := Pixel_Colour;
            END IF;
         END LOOP;

      END LOOP;
   END Draw_Character;

   PROCEDURE Shift_Lines(
      Current_Textbox : IN OUT textbox;
      Shifts : IN u32)
   IS
   BEGIN
      FOR I IN u32 RANGE 1 .. Shifts LOOP
         FOR Y IN Current_Textbox.Data'range(1) LOOP
            EXIT WHEN Y = Current_Textbox.Data'last(1);
            FOR X IN Current_Textbox.Data'range(2) LOOP
               Current_Textbox.Data(Y, X) := Current_Textbox.Data(Y + 1, X);
            END LOOP;
         END LOOP;
      END LOOP;

      FOR X IN Current_Textbox.Data'range(2) LOOP
         Current_Textbox.Data(Current_Textbox.Data'last(1), X) :=
            character'val(0);
      END LOOP;
   END Shift_Lines;

   PROCEDURE Update_Cursor(
      Current_Textbox : IN OUT textbox)
   IS
   BEGIN
      IF Current_Textbox.Current_X_Index > Current_Textbox.Data'last(2) THEN
         Current_Textbox.Current_X_Index := Current_Textbox.Data'first(2);
         Current_Textbox.Current_Y_Index :=
            Current_Textbox.Current_Y_Index + 1;
      END IF;

      IF Current_Textbox.Current_Y_Index > Current_Textbox.Data'last(1) THEN
         Shift_Lines(Current_Textbox, 1);
         Current_Textbox.Current_Y_Index := Current_Textbox.Data'last(1);
      END IF;
   END Update_Cursor;

   PROCEDURE Print(
      Current_Textbox : IN OUT textbox;
      Message : IN str)
   IS
   BEGIN
      FOR I IN Message'range LOOP
         Update_Cursor(Current_Textbox);

         Current_Textbox.Data(Current_Textbox.Current_Y_Index,
            Current_Textbox.Current_X_Index) :=
            Message(I);

         Current_Textbox.Current_X_Index :=
            Current_Textbox.Current_X_Index + 1;
      END LOOP;
   END Print;

   PROCEDURE Next_Line(
      Current_Textbox : IN OUT textbox)
   IS
   BEGIN
      Current_Textbox.Current_Y_Index := Current_Textbox.Current_Y_Index + 1;
      Current_Textbox.Current_X_Index := Current_Textbox.Data'first(2);
      Update_Cursor(Current_Textbox);
   END Next_Line;

   PROCEDURE Draw_Textbox(
      Buffer : IN OUT framebuffer;
      Current_Textbox : IN textbox;
      Pixel_Start : IN u64)
   IS
      Current_Pixel : u64 := Pixel_Start;

      Row_Separation : CONSTANT u64 := u64(Current_Textbox.Line_Separation) +
         u64(Framefont_Height);

      Next_Row : CONSTANT u64 := u64(Screen_Width * Pixel_Size) *
         Row_Separation;

      Next_Column : CONSTANT u64 := u64(Framefont_Width) +
         u64(Current_Textbox.Kerning);
   BEGIN
      FOR Y IN Current_Textbox.Data'range(1) LOOP
         FOR X IN Current_Textbox.Data'range(2) LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a new-line or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase.
            IF character'pos(Current_Textbox.Data(Y, X)) > 32 THEN
               Draw_Character(
                  Buffer,
                  Current_Pixel,
                  Current_Textbox.Foreground_Colour,
                  Current_Textbox.Data(Y, X));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Pixel_Start + u64(Y) * Next_Row;
      END LOOP;
   END Draw_Textbox;
END HAVK_Kernel.Graphics.Text;
