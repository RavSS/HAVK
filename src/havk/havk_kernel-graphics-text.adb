WITH
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics.Text IS
   PROCEDURE Draw_Character(
      Object            : IN view;
      Pixel_Start       : IN num;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character)
   IS
      I                 : CONSTANT num := character'pos(ASCII);
      Pixel_Location    : num;
      X_Line            : num;
   BEGIN
      FOR Y IN num RANGE 0 .. Framefont_Height - 1 LOOP
         X_Line := Framefont(I, Y);

         -- Move down a scanline if Y is more than 0.
         Pixel_Location := Pixel_Start + Y * Object.Screen_Width;

         -- Reverse the loop due to how I've stored the font data.
         -- Check "HAVK_Kernel.Font" for bit order information.
         FOR X IN REVERSE num RANGE 0 .. Framefont_Width - 1 LOOP
            -- Move to the next pixel by incrementing
            -- the pixel location.
            Pixel_Location := Pixel_Location + Object.Pixel_Size;

            IF BT(X_Line, X) THEN
               Object.Screen(Pixel_Location, Foreground_Colour);
            ELSE
               Object.Screen(Pixel_Location, Background_Colour);
            END IF;
         END LOOP;

      END LOOP;
   END Draw_Character;

   PROCEDURE Scroll_Down(
      Object : IN OUT textbox)
   IS
   BEGIN
      FOR Y IN Object.Data'range(1) LOOP
         EXIT WHEN Y = Object.Data'last(1);
         FOR X IN Object.Data'range(2) LOOP
            Object.Data(Y, X) := Object.Data(Y + 1, X);
         END LOOP;
      END LOOP;

      FOR X IN Object.Data'range(2) LOOP
         Object.Data(Object.Data'last(1), X) := character'val(0);
      END LOOP;
   END Scroll_Down;

   PROCEDURE Update_Cursor(
      Object : IN OUT textbox)
   IS
   BEGIN
      IF Object.Current_X_Index >  Object.Data'last(2) THEN
         Object.Current_X_Index := Object.Data'first(2);
         Object.Current_Y_Index :=
            Object.Current_Y_Index + 1;
      END IF;

      IF Object.Current_Y_Index > Object.Data'last(1) THEN
         Object.Scroll_Down;
         Object.Current_Y_Index := Object.Data'last(1);
      END IF;
   END Update_Cursor;

   PROCEDURE Print(
      Object  : IN OUT textbox;
      Message : IN string)
   IS
   BEGIN
      FOR I IN Message'range LOOP
         Update_Cursor(Object);

         Object.Data(Object.Current_Y_Index, Object.Current_X_Index) :=
            Message(I);

         Object.Current_X_Index := Object.Current_X_Index + 1;
      END LOOP;
   END Print;

   PROCEDURE Next_Line(
      Object : IN OUT textbox)
   IS
   BEGIN
      Object.Current_Y_Index := Object.Current_Y_Index + 1;
      Object.Current_X_Index := Object.Data'first(2);
      Object.Update_Cursor;
   END Next_Line;

   PROCEDURE Draw(
      Object          : IN textbox)
   IS
      Current_Pixel   : num := Object.Position;
      Row_Separation  : CONSTANT num := Object.Line_Separation +
         Framefont_Height;
      Next_Row        : CONSTANT num := Object.Display.Screen_Width *
         Object.Display.Pixel_Size * Row_Separation;
      Next_Column     : CONSTANT num := Framefont_Width + Object.Kerning;
   BEGIN
      FOR Y IN Object.Data'range(1) LOOP
         FOR X IN Object.Data'range(2) LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a new-line or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase. Do still draw
            -- spaces (32) though, or else there would be a gap between
            -- words if the user wanted to fill in the background too.
            IF character'pos(Object.Data(Y, X)) > 31 THEN
               Draw_Character(
                  Object.Display.ALL,
                  Current_Pixel,
                  Object.Foreground_Colour,
                  Object.Background_Colour,
                  Object.Data(Y, X));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Object.Position + Y * Next_Row;
      END LOOP;
   END Draw;

   PROCEDURE Clear_All(
      Object : IN OUT textbox)
   IS
   BEGIN
      FOR Y IN Object.Data'range(1) LOOP
         FOR X IN Object.Data'range(2) LOOP
            Object.Data(Y, X) := character'val(0);
         END LOOP;
      END LOOP;
   END Clear_All;
END HAVK_Kernel.Graphics.Text;
