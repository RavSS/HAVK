WITH
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics.Text
IS
   PROCEDURE Draw_Character(
      Buffer            : IN view;
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
         Pixel_Location := Pixel_Start + Y * Buffer.Screen_Width;

         -- Reverse the loop due to how I've stored the font data.
         -- Check "HAVK_Kernel.Font" for bit order information.
         FOR X IN REVERSE num RANGE 0 .. Framefont_Width - 1 LOOP
            -- Move to the next pixel by incrementing
            -- the pixel location.
            Pixel_Location := Pixel_Location + Buffer.Pixel_Size;

            IF BT(X_Line, X) THEN
               Buffer.Screen(Pixel_Location, Foreground_Colour);
            ELSE
               Buffer.Screen(Pixel_Location, Background_Colour);
            END IF;
         END LOOP;

      END LOOP;
   END Draw_Character;

   PROCEDURE Scroll_Down(
      Object : IN OUT textbox)
   IS
   BEGIN
      -- Shift every line upwards.
      FOR Y IN Object.Data'first(1) .. Object.Data'last(1) - 1 LOOP
         FOR X IN Object.Data'range(2) LOOP
            Object.Data(Y, X) := Object.Data(Y + 1, X);
         END LOOP;
      END LOOP;

      -- Now blank out the last line.
      FOR X IN Object.Data'range(2) LOOP
         Object.Data(Object.Data'last(1), X) := character'val(0);
      END LOOP;
   END Scroll_Down;

   PROCEDURE Update_Cursor(
      Object : IN OUT textbox)
   IS
   BEGIN
      IF Object.Current_X_Index  > Object.Data'last(2) THEN
         Object.Current_X_Index := Object.Data'first(2);
         Object.Current_Y_Index := Object.Current_Y_Index + 1;
      END IF;

      IF Object.Current_Y_Index  > Object.Data'last(1) THEN
         Object.Scroll_Down;
         Object.Current_Y_Index := Object.Data'last(1);
      END IF;
   END Update_Cursor;

   -- TODO: In the debug build, this procedure somehow ends up adding a
   -- random character to the end of the data array inside the textbox.
   -- The address of "Message" seems to be that very character. No clue.
   PROCEDURE Print(
      Object  : IN OUT textbox;
      Message : IN string;
      Centre  : IN boolean := false)
   IS
   BEGIN
      IF Centre AND THEN Message'length /= 0 THEN
         Object.Current_X_Index := Object.Data'last(2) / 2 -
            Message'length / 2;
      END IF;

      FOR I IN Message'range LOOP
         Update_Cursor(Object);

         Object.Data(Object.Current_Y_Index, Object.Current_X_Index) :=
            Message(I);

         Object.Current_X_Index := Object.Current_X_Index + 1;
      END LOOP;
   END Print;

   PROCEDURE Newline(
      Object : IN OUT textbox;
      Amount : IN num := 1)
   IS
   BEGIN
      FOR I in 1 .. Amount LOOP
         Object.Current_Y_Index := Object.Current_Y_Index + 1;
         Object.Current_X_Index := Object.Data'first(2);
         Object.Update_Cursor;
      END LOOP;
   END Newline;

   PROCEDURE Draw_On(
      Object          : IN textbox;
      Display         : IN view)
   IS
      Current_Pixel   : num := Object.Position;
      Row_Separation  : CONSTANT num := Object.Line_Separation +
         Framefont_Height;
      Next_Row        : CONSTANT num := Display.Screen_Width *
         Display.Pixel_Size * Row_Separation;
      Next_Column     : CONSTANT num := Framefont_Width + Object.Kerning;
   BEGIN
      FOR Y IN Object.Data'range(1) LOOP
         FOR X IN Object.Data'range(2) LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a newline or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase. Do still draw
            -- spaces (32) though, or else there would be a gap between
            -- words if the user wanted to fill in the background too. Null
            -- characters are also drawn for the purpose of an explicit blank.
            IF character'pos(Object.Data(Y, X)) > 31 OR ELSE
               character'pos(Object.Data(Y, X)) =  0
            THEN
               Draw_Character(
                  Display,
                  Current_Pixel,
                  Object.Foreground_Colour,
                  Object.Background_Colour,
                  Object.Data(Y, X));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Object.Position + Y * Next_Row;
      END LOOP;
   END Draw_On;

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

   PROCEDURE Clear_Column(
      Object  : IN OUT textbox;
      Column  : IN num)
   IS
   BEGIN
      IF Column NOT IN Object.Data'range(2) THEN
         RETURN;
      END IF;

      FOR Y IN Object.Data'range(1) LOOP
         Object.Data(Y, Column) := character'val(0);
      END LOOP;
   END Clear_Column;

   PROCEDURE Clear_Line(
      Object  : IN OUT textbox;
      Line    : IN num)
   IS
   BEGIN
      IF Line NOT IN Object.Data'range(1) THEN
         RETURN;
      END IF;

      FOR X IN Object.Data'range(2) LOOP
         Object.Data(Line, X) := character'val(0);
      END LOOP;
   END Clear_Line;
END HAVK_Kernel.Graphics.Text;
