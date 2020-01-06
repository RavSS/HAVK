-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-graphics-text.adb                          --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Font,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Graphics.Text
IS
   PROCEDURE Draw_Character
     (Buffer            : IN view;
      Pixel_Start       : IN number;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character)
   IS
      I                 : CONSTANT number := character'pos(ASCII);
      Pixel_Location    : number;
      X_Line            : number;
   BEGIN
      FOR
         Y IN number RANGE 0 .. Framefont_Height - 1
      LOOP
         X_Line := Framefont(I, Y);

         -- Move down a scanline if Y is more than 0.
         Pixel_Location := Pixel_Start + Y * Buffer.Screen_Width;

         -- Reverse the loop due to how I've stored the font data.
         -- Check "HAVK_Kernel.Font" for bit order information.
         FOR
            X IN REVERSE number RANGE 0 .. Framefont_Width - 1
         LOOP
            -- Move to the next pixel by incrementing the pixel location.
            Pixel_Location := Pixel_Location + Buffer.Pixel_Size;

            -- Range check. Regardless of the screen's pixel capacity, I will
            -- just draw as much of the character as I can to make it easier.
            EXIT WHEN Pixel_Location >= Buffer.Framebuffer_Elements;

            IF
               Bit_Test(X_Line, X)
            THEN
               Buffer.Screen(Pixel_Location, Foreground_Colour);
            ELSE
               Buffer.Screen(Pixel_Location, Background_Colour);
            END IF;
         END LOOP;

      END LOOP;
   END Draw_Character;

   PROCEDURE Scroll_Down
     (Object : IN OUT textbox)
   IS
   BEGIN
      -- Shift every line upwards if there are multiple lines.
      IF
         Object.Data'first(1) < Object.Data'last(1)
      THEN
         FOR
            Y IN Object.Data'first(1) .. Object.Data'last(1) - 1
         LOOP
            FOR
               X IN Object.Data'range(2)
            LOOP
               Object.Data(Y, X) := Object.Data(Y + 1, X);
            END LOOP;
         END LOOP;
      END IF;

      -- Now blank out the last line.
      FOR
         X IN Object.Data'range(2)
      LOOP
         Object.Data(Object.Data'last(1), X) := character'val(0);
      END LOOP;
   END Scroll_Down;

   PROCEDURE Update_Cursor
     (Object : IN OUT textbox)
   IS
   BEGIN
      IF
         Object.Current_X_Index  > Object.Data'last(2)
      THEN
         Object.Current_X_Index := Object.Data'first(2);
         Object.Current_Y_Index := Object.Current_Y_Index + 1;
      END IF;

      IF
         Object.Current_Y_Index  > Object.Data'last(1)
      THEN
         Object.Scroll_Down; -- Call before index assignments (`gnatprove`).
         Object.Current_X_Index := Object.Data'first(2);
         Object.Current_Y_Index := Object.Data'last(1);
      END IF;
   END Update_Cursor;

   PROCEDURE Print
     (Object  : IN OUT textbox;
      Message : IN string;
      Centre  : IN boolean := false)
   IS
   BEGIN
      IF
         Centre AND THEN Message'length /= 0
      THEN
         DECLARE -- Index range checks for SPARK.
            Centre_Index         : CONSTANT number := Object.Data'last(2) / 2;
            Centre_Message_Index : CONSTANT number := Message'length / 2;
         BEGIN
            IF
               Centre_Index          = 0 OR ELSE
               Centre_Message_Index  = 0 OR ELSE
               Centre_Index         <= Centre_Message_Index
            THEN
               Object.Current_X_Index := Object.Data'first(2);
            ELSE
               Object.Current_X_Index := Centre_Index - Centre_Message_Index;
            END IF;
         END;
      END IF;

      FOR
         Letter OF Message
      LOOP
         Update_Cursor(Object);
         Object.Data(Object.Current_Y_Index, Object.Current_X_Index) := Letter;
         Object.Current_X_Index := Object.Current_X_Index + 1;
      END LOOP;
   END Print;

   PROCEDURE Newline
     (Object : IN OUT textbox;
      Amount : IN number := 1)
   IS
   BEGIN
      FOR
         I in 1 .. Amount
      LOOP
         Object.Current_Y_Index := Object.Current_Y_Index + 1;
         Object.Current_X_Index := Object.Data'first(2);
         Object.Update_Cursor;
      END LOOP;
   END Newline;

   PROCEDURE Draw_On
     (Object         : IN textbox;
      Display        : IN view)
   IS
      Row_Separation : CONSTANT number := Object.Line_Separation +
         Framefont_Height;
      Next_Row       : CONSTANT number := (Display.Screen_Width *
         Display.Pixel_Size) * Row_Separation;
      Next_Column    : CONSTANT number := Framefont_Width + Object.Kerning;
      Current_Pixel  :          number := Object.Start_Position;
   BEGIN
      FOR
         Y IN Object.Data'range(1)
      LOOP
         FOR
            X IN Object.Data'range(2)
         LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a newline or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase. Do still draw
            -- spaces (32) though, or else there would be a gap between
            -- words if the user wanted to fill in the background too.
            IF
               character'pos(Object.Data(Y, X)) > 31
            THEN
               Draw_Character
                 (Display,
                  Current_Pixel,
                  Object.Foreground_Colour,
                  Object.Background_Colour,
                  Object.Data(Y, X));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Object.Start_Position + Next_Row * (Y + 1);
      END LOOP;
   END Draw_On;

   PROCEDURE Clear_All
     (Object : IN OUT textbox;
      ASCII  : IN character := character'val(0))
   IS
   BEGIN
      FOR
         Y IN Object.Data'range(1)
      LOOP
         FOR
            X IN Object.Data'range(2)
         LOOP
            Object.Data(Y, X) := ASCII;
         END LOOP;
      END LOOP;
   END Clear_All;

   PROCEDURE Clear_Column
     (Object : IN OUT textbox;
      Column : IN number;
      ASCII  : IN character := character'val(0))
   IS
   BEGIN
      IF
         Column IN Object.Data'range(2)
      THEN
         FOR
            Y IN Object.Data'range(1)
         LOOP
            Object.Data(Y, Column) := ASCII;
         END LOOP;
      END IF;
   END Clear_Column;

   PROCEDURE Clear_Line
     (Object : IN OUT textbox;
      Line   : IN number;
      ASCII  : IN character := character'val(0))
   IS
   BEGIN
      IF
         Line IN Object.Data'range(1)
      THEN
         FOR
            X IN Object.Data'range(2)
         LOOP
            Object.Data(Line, X) := ASCII;
         END LOOP;
      END IF;
   END Clear_Line;
END HAVK_Kernel.Graphics.Text;
