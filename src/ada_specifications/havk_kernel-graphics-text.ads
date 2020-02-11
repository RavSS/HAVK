-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-graphics-text.ads                          --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package handles graphical text. The main object is a textbox, which
-- can be used for any case where textual information needs to be displayed.
PACKAGE HAVK_Kernel.Graphics.Text
IS
   PRAGMA Preelaborate;

   -- Stores textbox data in two dimensions. First index is for the line,
   -- second index is for the column.
   TYPE textbox_data IS ARRAY(number RANGE <>, number RANGE <>) OF character;

   -- This is the type used for showing textboxes and message boxes.
   -- For now, it is used for terminal/commandline console display logic.
   TYPE textbox
     (Width  : number; -- Amount of columns (characters on a row).
      Height : number) -- Amount of rows (characters in a column).
   IS TAGGED RECORD
      -- Warnings raised by GNAT, but not anything to do with Ada standards.
      PRAGMA Warnings(off, "record layout may cause performance issues");
      PRAGMA Warnings(off, "*length depends on a discriminant");
      PRAGMA Warnings(off, "comes too early and was moved down");
      -- Holds the data for the textbox. First dimension is the line/row (Y).
      Data              : textbox_data(0 .. Height, 0 .. Width) :=
         (OTHERS => (OTHERS => character'val(32)));
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
   END RECORD;

   -- Adds a string into a textbox.
   -- TODO: This currently forces text to be uppercase by default as my
   -- own font does not support lowercase characters for the time being.
   PROCEDURE Print
     (Object    : IN OUT textbox;
      Message   : IN string;
      Centre    : IN boolean := false;
      Next_Line : IN boolean := true;
      Uppercase : IN boolean := true);

   -- Moves the cursor down a row while handling correct positioning.
   PROCEDURE Newline
     (Object  : IN OUT textbox;
      Amount  : IN number := 1)
   WITH
      Inline => true;

   -- Draws the textbox onto a framebuffer.
   PROCEDURE Draw_On
     (Object  : IN textbox;
      Display : IN view);

   -- Removes all characters from a textbox and replaces them with null.
   PROCEDURE Clear_All
     (Object  : IN OUT textbox;
      ASCII   : IN character := character'val(0))
   WITH
      Inline => true;

   -- Removes all characters from a textbox's column.
   PROCEDURE Clear_Column
     (Object  : IN OUT textbox;
      Column  : IN number;
      ASCII   : IN character := character'val(0))
   WITH
      Inline => true;

   -- Removes all characters from a textbox's row.
   PROCEDURE Clear_Line
     (Object  : IN OUT textbox;
      Line    : IN number;
      ASCII   : IN character := character'val(0))
   WITH
      Inline => true;

   -- This draws a character onto the framebuffer type which is defined
   -- in this very procedure's parent package.
   PROCEDURE Draw_Character
     (Buffer            : IN view;
      Pixel_Start       : IN number;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character);

PRIVATE
   -- Shifts every row upwards and clears out the row at the bottom.
   PROCEDURE Scroll_Down
     (Object  : IN OUT textbox);

   -- Refreshes the current data array index variables to where they should be
   -- according to array bounds.
   PROCEDURE Update_Cursor
     (Object  : IN OUT textbox)
   WITH
      Post'class => Object.Current_Y_Index <= Object.Data'last(1) AND THEN
                    Object.Current_X_Index <= Object.Data'last(2);
END HAVK_Kernel.Graphics.Text;
