-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Terminal                         --
-- Filename        -- havk_terminal-graphics-text.ads                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package handles graphical text. The main state that gets passed around
-- is a "textbox", which can be used for any case where textual information
-- needs to be displayed.
PACKAGE HAVK_Terminal.Graphics.Text
IS
   -- Stores textbox data in two dimensions. First index is for the line,
   -- second index is for the column.
   TYPE textbox_data IS ARRAY(number RANGE <>, number RANGE <>) OF character;

   -- This is the type used for showing textboxes and message boxes.
   -- For now, it is used for terminal/command-line terminal display logic.
   TYPE textbox
     (Width  : number; -- Amount of columns (characters on a row).
      Height : number) -- Amount of rows (characters in a column).
   IS RECORD
      -- The X index of the text cursor, which is the column (textbox width).
      Current_X_Index   : number := 0;
      -- The Y index of the text cursor, which is the row (textbox height).
      Current_Y_Index   : number := 0;
      -- Set to a black background by default.
      Background_Colour : pixel  := 0;
      -- Set to white text by default.
      Foreground_Colour : pixel  := 16#FFFFFF#;
      -- The distance between drawn characters is 2 pixels by default.
      Kerning           : number := 2;
      -- TODO: Font size is currently unused, but I think I can "upscale"
      -- the font if I want?
      Font_Size         : number := 1;
      -- The line separation between rows is 3 pixels by default.
      -- This does not include the font height, that is handled automatically.
      Line_Separation   : number := 3;
      -- Where the textbox should be drawn on a framebuffer.
      Start_Position    : number := 0;
      -- Holds the data for the textbox. First dimension is the line/row (Y).
      Data              : textbox_data(0 .. Height, 0 .. Width) :=
         (OTHERS => (OTHERS => ' '));
   END RECORD;

   -- Adds a string into a textbox.
   -- TODO: This currently forces text to be uppercase by default as my
   -- own font does not support lowercase characters for the time being.
   PROCEDURE Print
     (Terminal  : IN OUT textbox;
      Message   : IN string;
      Centre    : IN boolean := false;
      Next_Line : IN boolean := true;
      Uppercase : IN boolean := true);

   -- Moves the cursor down a row while handling correct positioning.
   PROCEDURE Newline
     (Terminal : IN OUT textbox;
      Amount   : IN number := 1)
   WITH
      Inline => true;

   -- Erases the character behind the cursor in the textbox.
   PROCEDURE Backspace
     (Terminal : IN OUT textbox;
      Amount   : IN number := 1)
   WITH
      Inline => true;

   -- Draws the textbox onto the framebuffer.
   PROCEDURE Draw_Terminal
     (Terminal : IN textbox);

   -- Removes all characters from a textbox and replaces them with null.
   PROCEDURE Clear_All
     (Terminal : IN OUT textbox;
      ASCII    : IN character := character'val(0))
   WITH
      Inline => true;

   -- Removes all characters from a textbox's column.
   PROCEDURE Clear_Column
     (Terminal : IN OUT textbox;
      Column   : IN number;
      ASCII    : IN character := character'val(0))
   WITH
      Inline => true;

   -- Removes all characters from a textbox's row.
   PROCEDURE Clear_Line
     (Terminal : IN OUT textbox;
      Line     : IN number;
      ASCII    : IN character := character'val(0))
   WITH
      Inline => true;

   -- This draws a character onto the framebuffer type which is defined
   -- in this very procedure's parent package.
   PROCEDURE Draw_Character
     (Pixel_Start       : IN number;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character);

PRIVATE
   -- Shifts every row upwards and clears out the row at the bottom.
   PROCEDURE Scroll_Down
     (Terminal : IN OUT textbox);

   -- Refreshes the current data array index variables, effectively moving the
   -- cursor forwards.
   PROCEDURE Cursor_Forward
     (Terminal : IN OUT textbox)
   WITH
      Post => Terminal.Current_Y_Index <= Terminal.Data'last(1) AND THEN
              Terminal.Current_X_Index <= Terminal.Data'last(2);

   -- Refreshes the current data array index variables, effectively moving the
   -- cursor backwards.
   PROCEDURE Cursor_Backward
     (Terminal : IN OUT textbox)
   WITH
      Post => Terminal.Current_Y_Index <= Terminal.Data'last(1) AND THEN
              Terminal.Current_X_Index <= Terminal.Data'last(2);
END HAVK_Terminal.Graphics.Text;
