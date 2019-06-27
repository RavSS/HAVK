WITH
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics.Text IS
   PROCEDURE Draw_Character(
      Buffer            : IN OUT framebuffer;
      Pixel_Start       : IN num;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character)
   IS
      I : CONSTANT num  := character'pos(ASCII);
      Pixel_Location    : num;
      X_Line            : num;
   BEGIN
      FOR Y IN num RANGE 0 .. Framefont_Height - 1 LOOP
         X_Line := Framefont(I, Y);

         -- Move down a scanline if Y is more than 0.
         Pixel_Location := Pixel_Start + Y * Screen_Width;

         -- Reverse the loop due to how I've stored the font data.
         -- Check "HAVK_Kernel.Font" for bit order information.
         FOR X IN REVERSE num RANGE 0 .. Framefont_Width - 1 LOOP
            -- Move to the next pixel by incrementing
            -- the pixel location.
            Pixel_Location := Pixel_Location + Pixel_Size;

            IF BT(X_Line, X) THEN
               Buffer(Pixel_Location) := Foreground_Colour;
            ELSE
               Buffer(Pixel_Location) := Background_Colour;
            END IF;
         END LOOP;

      END LOOP;
   END Draw_Character;

   PROCEDURE Scroll_Down(
      Current_Textbox : IN OUT textbox)
   IS
   BEGIN
      FOR Y IN Current_Textbox.Data'range(1) LOOP
         EXIT WHEN Y = Current_Textbox.Data'last(1);
         FOR X IN Current_Textbox.Data'range(2) LOOP
            Current_Textbox.Data(Y, X) := Current_Textbox.Data(Y + 1, X);
         END LOOP;
      END LOOP;

      FOR X IN Current_Textbox.Data'range(2) LOOP
         Current_Textbox.Data(Current_Textbox.Data'last(1), X) :=
            character'val(0);
      END LOOP;
   END Scroll_Down;

   PROCEDURE Update_Cursor(
      Current_Textbox : IN OUT textbox)
   IS
   BEGIN
      IF Current_Textbox.Current_X_Index >  Current_Textbox.Data'last(2) THEN
         Current_Textbox.Current_X_Index := Current_Textbox.Data'first(2);
         Current_Textbox.Current_Y_Index :=
            Current_Textbox.Current_Y_Index + 1;
      END IF;

      IF Current_Textbox.Current_Y_Index > Current_Textbox.Data'last(1) THEN
         Current_Textbox.Scroll_Down;
         Current_Textbox.Current_Y_Index := Current_Textbox.Data'last(1);
      END IF;
   END Update_Cursor;

   PROCEDURE Print(
      Current_Textbox : IN OUT textbox;
      Message         : IN string)
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

   PROCEDURE Draw(
      Current_Textbox   : IN textbox;
      Buffer            : IN OUT framebuffer;
      Pixel_Start       : IN num)
   IS
      Current_Pixel   : num := Pixel_Start;
      Row_Separation  : CONSTANT num := Current_Textbox.Line_Separation +
         Framefont_Height;
      Next_Row        : CONSTANT num := Screen_Width * Pixel_Size *
         Row_Separation;
      Next_Column     : CONSTANT num := Framefont_Width +
         Current_Textbox.Kerning;
   BEGIN
      FOR Y IN Current_Textbox.Data'range(1) LOOP
         FOR X IN Current_Textbox.Data'range(2) LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a new-line or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase. Do still draw
            -- spaces (32) though, or else there would be a gap between
            -- words if the user wanted to fill in the background too.
            IF character'pos(Current_Textbox.Data(Y, X)) > 31 THEN
               Draw_Character(
                  Buffer,
                  Current_Pixel,
                  Current_Textbox.Foreground_Colour,
                  Current_Textbox.Background_Colour,
                  Current_Textbox.Data(Y, X));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Pixel_Start + Y * Next_Row;
      END LOOP;
   END Draw;

   PROCEDURE Clear_All(
      Current_Textbox : IN OUT textbox)
   IS
   BEGIN
      FOR Y IN Current_Textbox.Data'range(1) LOOP
         FOR X IN Current_Textbox.Data'range(2) LOOP
            Current_Textbox.Data(Y, X) := character'val(0);
         END LOOP;
      END LOOP;
   END Clear_All;
END HAVK_Kernel.Graphics.Text;
