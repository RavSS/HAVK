PACKAGE HAVK_Kernel.Graphics.Text IS
   TYPE textbox_data IS ARRAY(num RANGE <>, num RANGE <>) OF character;

   TYPE textbox(
      Width             : num;
      Height            : num)
   IS TAGGED RECORD
      -- Warnings raised by GNAT, but not anything to do with Ada standards.
      PRAGMA Warnings(off, "record layout may cause performance issues");
      PRAGMA Warnings(off, "*length depends on a discriminant");
      PRAGMA Warnings(off, "comes too early and was moved down");
      -- Holds the data for the textbox. It may contain junk upon declaration.
      Data              : textbox_data(1 .. Height, 1 .. Width);
      -- The X index of the current cursor, which is the column.
      Current_X_Index   : num := 1;
      -- The Y index of the current cursor, which is the row.
      Current_Y_Index   : num := 1;
      -- Set to a black background by default.
      Background_Colour : pixel := 0;
      -- Set to white text by default.
      Foreground_Colour : pixel := 16#FFFFFF#;
      -- The distance between drawn characters is 2 pixels by default.
      Kerning           : num := 2;
      -- TODO: Font size is currently unused, but I think I can "upscale"
      -- the font if I want?
      Font_Size         : num := 1;
      -- The line separation between rows is 3 pixels by default.
      -- This does not include the font height, that is handled automatically.
      Line_Separation   : num := 3;
   END RECORD;

   PROCEDURE Draw_Character(
      Buffer            : IN OUT framebuffer;
      Pixel_Start       : IN num;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character);

PRIVATE
   PROCEDURE Scroll_Down(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Update_Cursor(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Print(
      Current_Textbox   : IN OUT textbox;
      Message           : IN string);

   PROCEDURE Next_Line(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Draw(
      Current_Textbox   : IN textbox;
      Buffer            : IN OUT framebuffer;
      Pixel_Start       : IN num);

   PROCEDURE Clear_All(
      Current_Textbox   : IN OUT textbox)
   WITH
      Inline => true;
END HAVK_Kernel.Graphics.Text;
