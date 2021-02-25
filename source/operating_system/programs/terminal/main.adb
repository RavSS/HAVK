-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Terminal                         --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Terminal,
   HAVK_Terminal.Graphics,
   HAVK_Terminal.Graphics.Text;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Terminal,
   HAVK_Terminal.Graphics,
   HAVK_Terminal.Graphics.Text;

PROCEDURE Main
WITH
   No_Return => true
IS
   -- The main terminal or virtual terminal used to display text to the user.
   Terminal         : textbox  -- Font width and font height are 8 pixels.
     (Screen_Width  / 14 + 2,  -- (Font width  - 1) * 2 + 2. Wider.
      Screen_Height / 14 - 2); -- (Font height - 1) * 2 - 2. Shorter.

   -- The index on the framebuffer as to where the terminal begins.
   Terminal_Start   : CONSTANT number := Calculate_Pixel
     (Screen_Width / 8, Screen_Height / 8);

   -- The program will exit erroneously after feeding this many characters to
   -- the textbox state.
   Death_Countdown  : number := 1000;

   -- Receive terminal data on port (subheader value) 404 for now, just for a
   -- showcase.
   Text_Data        : arguments  := (receive_message_operation, OTHERS => 0);
   Text_Data_String : XMM_string := (OTHERS => NUL);
   Text_Data_Port   : CONSTANT   := 404;
   Error_Check      : error;
BEGIN
   Draw_Fill(0, Framebuffer_Elements, 0);

   Terminal.Start_Position := Terminal_Start;
   Terminal.Background_Colour := Create_Pixel(0, 0, 0);
   Terminal.Foreground_Colour := Create_Pixel(255, 25, 25);
   Print(Terminal, "Welcome.", Centre => true);
   Draw_Terminal(Terminal);

   LOOP
      Text_Data.Argument_1 := 0;

      -- Make it into a volatile function later, use a holding variable for
      -- the error value now.
      Error_Check := System_Call(Text_Data, Text_Data_String);

      IF
         Error_Check = no_error    AND THEN
         Text_Data.Argument_2 /= 0 AND THEN
         Text_Data.Argument_3 = Text_Data_Port
      THEN
         -- TODO: Converting from the "XMM_string" type to a standard string
         -- seems to be exhibit odd behaviour. Print character-by-character
         -- anyway.
         FOR
            ASCII OF Text_Data_String(1 .. positive(Text_Data.Argument_2))
         LOOP
            CASE
               ASCII
            IS
               WHEN LF =>
                  Newline(Terminal);
               WHEN BS =>
                  Backspace(Terminal);
               WHEN OTHERS =>
                  Print(Terminal, ASCII & "", Next_Line => false);
                  Death_Countdown := (IF Death_Countdown /= 0 THEN
                     Death_Countdown - 1 ELSE Death_Countdown);
            END CASE;
         END LOOP;

         Draw_Terminal(Terminal);
      END IF;

      IF
         Death_Countdown = 0
      THEN
         Newline(Terminal, Amount => 2);
         Print(Terminal, "Example bug triggered. Now dying.", Centre => true);
         Draw_Terminal(Terminal);
         RAISE Program_Error
         WITH
            "Example bug triggered. Now dying.";
      END IF;
   END LOOP;
END Main;
