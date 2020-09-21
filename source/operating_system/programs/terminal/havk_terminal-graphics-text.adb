-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Terminal                         --
-- Filename        -- havk_terminal-graphics-text.adb                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Terminal.Font;
USE
   HAVK_Terminal.Font;

PACKAGE BODY HAVK_Terminal.Graphics.Text
IS
   PROCEDURE Draw_Character
     (Pixel_Start       : IN number;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character)
   IS
      Pixel_Location : number;
      X_Line         : number;
   BEGIN
      FOR
         Y_Index IN number RANGE 0 .. Framefont_Height - 1
      LOOP
         X_Line := Framefont(character'pos(ASCII), Y_Index);

         -- Move down a scanline if the vertical index is more than zero.
         Pixel_Location := Pixel_Start + Y_Index * Screen_Width;

         -- Reverse the loop due to how I've stored the font data.
         -- Check "HAVK_Terminal.Font" for bit order information.
         FOR
            X_Index IN REVERSE number RANGE 0 .. Framefont_Width - 1
         LOOP
            -- Move to the next pixel by incrementing the pixel location.
            Pixel_Location := Pixel_Location + Pixel_Size;

            -- Range check. Regardless of the screen's pixel capacity, I will
            -- just draw as much of the character as I can to make it easier.
            EXIT WHEN Pixel_Location >= Framebuffer_Elements;

            IF
               Bit_Test(X_Line, X_Index)
            THEN
               Framebuffer(Pixel_Location) := Foreground_Colour;
            ELSE
               Framebuffer(Pixel_Location) := Background_Colour;
            END IF;
         END LOOP;
      END LOOP;
   END Draw_Character;

   PROCEDURE Scroll_Down
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      -- Shift every line upwards if there are multiple lines.
      IF
         Terminal.Data'first(1) < Terminal.Data'last(1)
      THEN
         FOR
            Y_Index IN Terminal.Data'first(1) .. Terminal.Data'last(1) - 1
         LOOP
            FOR
               X_Index IN Terminal.Data'range(2)
            LOOP
               Terminal.Data(Y_Index, X_Index) :=
                  Terminal.Data(Y_Index + 1, X_Index);
            END LOOP;
         END LOOP;
      END IF;

      -- Now blank out the last line with spaces so it clears out old text.
      Clear_Line(Terminal, Terminal.Data'last(1), ASCII => ' ');
   END Scroll_Down;

   PROCEDURE Cursor_Forward
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      IF
         Terminal.Current_X_Index  > Terminal.Data'last(2)
      THEN
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Current_Y_Index := Terminal.Current_Y_Index + 1;
      END IF;

      IF
         Terminal.Current_Y_Index  > Terminal.Data'last(1)
      THEN
         Scroll_Down(Terminal); -- Call before index assignments (`gnatprove`).
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Terminal.Current_Y_Index := Terminal.Data'last(1);
      END IF;
   END Cursor_Forward;

   PROCEDURE Cursor_Backward
     (Terminal : IN OUT textbox)
   IS
   BEGIN
      IF
         Terminal.Current_Y_Index = Terminal.Data'first(1)
      THEN
         IF
            Terminal.Current_X_Index /= Terminal.Data'first(1)
         THEN
            Terminal.Current_X_Index := Terminal.Current_X_Index - 1;
         END IF;
      ELSE
         IF
            Terminal.Current_X_Index /= Terminal.Data'first(1)
         THEN
            Terminal.Current_X_Index := Terminal.Current_X_Index - 1;
         ELSE
            Terminal.Current_Y_Index := Terminal.Current_Y_Index - 1;
            Terminal.Current_X_Index := Terminal.Data'last(2);
         END IF;
      END IF;
   END Cursor_Backward;

   PROCEDURE Backspace
     (Terminal : IN OUT textbox;
      Amount   : IN number := 1)
   IS
   BEGIN
      FOR
         Count IN 1 .. Amount
      LOOP
         Cursor_Backward(Terminal);
         Terminal.Data
           (Terminal.Current_Y_Index, Terminal.Current_X_Index) := ' ';
      END LOOP;
   END Backspace;

   PROCEDURE Print
     (Terminal  : IN OUT textbox;
      Message   : IN string;
      Centre    : IN boolean := false;
      Next_Line : IN boolean := true;
      Uppercase : IN boolean := true)
   IS
      -- Lifted this from "System.Case_Util" because I don't want to bring in
      -- the "GNAT.Case_Util" and the System variant is an internal package
      -- instead.
      FUNCTION To_Upper
        (ASCII : IN character)
         RETURN character
      WITH
         Inline => true;

      FUNCTION To_Upper
        (ASCII : IN character)
         RETURN character
      IS
         ASCII_Value : CONSTANT natural := character'pos(ASCII);
      BEGIN
         IF
            ASCII IN 'a' .. 'z' OR ELSE
            ASCII_Value IN 16#E0# .. 16#F6# | 16#F8# .. 16#FE#
         THEN
            RETURN character'val(ASCII_Value - 16#20#);
         ELSE
            RETURN ASCII;
         END IF;
      END To_Upper;

      Centre_Index         : number;
      Centre_Message_Index : number;
   BEGIN
      IF
         Centre AND THEN Message'length /= 0
      THEN
         Clear_Line(Terminal, Terminal.Current_Y_Index, ASCII => ' ');
         Centre_Index         := Terminal.Data'last(2) / 2;
         Centre_Message_Index := Message'length / 2;

         IF -- Index range checks for `gnatprove`.
            Centre_Index = 0         OR ELSE
            Centre_Message_Index = 0 OR ELSE
            Centre_Index <= Centre_Message_Index
         THEN
            Terminal.Current_X_Index := Terminal.Data'first(2);
         ELSE
            Terminal.Current_X_Index := Centre_Index - Centre_Message_Index;
         END IF;
      END IF;

      FOR
         Letter OF Message
      LOOP
         Cursor_Forward(Terminal);

         -- TODO: The framebuffer font does not currently support lowercase
         -- characters, so I've forced them to be uppercase here by default.
         Terminal.Data(Terminal.Current_Y_Index, Terminal.Current_X_Index) :=
            (IF Uppercase THEN To_Upper(Letter) ELSE Letter);

         Terminal.Current_X_Index := Terminal.Current_X_Index + 1;
      END LOOP;

      IF
         Next_Line
      THEN
         Newline(Terminal);
      END IF;
   END Print;

   PROCEDURE Newline
     (Terminal : IN OUT textbox;
      Amount   : IN number := 1)
   IS
   BEGIN
      FOR
         I in 1 .. Amount
      LOOP
         Terminal.Current_Y_Index := Terminal.Current_Y_Index + 1;
         Terminal.Current_X_Index := Terminal.Data'first(2);
         Cursor_Forward(Terminal);
      END LOOP;
   END Newline;

   PROCEDURE Draw_Terminal
     (Terminal : IN textbox)
   IS
      Row_Separation : CONSTANT number :=
         Terminal.Line_Separation + Framefont_Height;
      Next_Row       : CONSTANT number :=
        (Screen_Width * Pixel_Size) * Row_Separation;
      Next_Column    : CONSTANT number := Framefont_Width + Terminal.Kerning;
      Current_Pixel  : number          := Terminal.Start_Position;
   BEGIN
      FOR
         Y_Index IN Terminal.Data'range(1)
      LOOP
         FOR
            X_Index IN Terminal.Data'range(2)
         LOOP
            -- Don't even bother going over something that is not visible,
            -- like for example a newline or a null character. The drawable
            -- ASCII characters start at 33, from the exclamation mark.
            -- This should grant a performance increase. Do still draw
            -- spaces (32) though, or else there would be a gap between
            -- words if the user wanted to fill in the background too.
            IF
               character'pos(Terminal.Data(Y_Index, X_Index)) > 31
            THEN
               Draw_Character
                 (Current_Pixel,
                  Terminal.Foreground_Colour,
                  Terminal.Background_Colour,
                  Terminal.Data(Y_Index, X_Index));
            END IF;

            Current_Pixel := Current_Pixel + Next_Column;
         END LOOP;

         Current_Pixel := Terminal.Start_Position + Next_Row * (Y_Index + 1);
      END LOOP;
   END Draw_Terminal;

   PROCEDURE Clear_All
     (Terminal : IN OUT textbox;
      ASCII    : IN character := character'val(0))
   IS
   BEGIN
      FOR
         Y_Index IN Terminal.Data'range(1)
      LOOP
         FOR
            X_Index IN Terminal.Data'range(2)
         LOOP
            Terminal.Data(Y_Index, X_Index) := ASCII;
         END LOOP;
      END LOOP;
   END Clear_All;

   PROCEDURE Clear_Column
     (Terminal : IN OUT textbox;
      Column   : IN number;
      ASCII    : IN character := character'val(0))
   IS
   BEGIN
      IF
         Column IN Terminal.Data'range(2)
      THEN
         FOR
            Y_Index IN Terminal.Data'range(1)
         LOOP
            Terminal.Data(Y_Index, Column) := ASCII;
         END LOOP;
      END IF;
   END Clear_Column;

   PROCEDURE Clear_Line
     (Terminal : IN OUT textbox;
      Line     : IN number;
      ASCII    : IN character := character'val(0))
   IS
   BEGIN
      IF
         Line IN Terminal.Data'range(1)
      THEN
         FOR
            X_Index IN Terminal.Data'range(2)
         LOOP
            Terminal.Data(Line, X_Index) := ASCII;
         END LOOP;
      END IF;
   END Clear_Line;
END HAVK_Terminal.Graphics.Text;
