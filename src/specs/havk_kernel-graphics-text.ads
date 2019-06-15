WITH
   HAVK_Kernel,
   HAVK_Kernel.Graphics;
USE
   HAVK_Kernel.Graphics;

PACKAGE HAVK_Kernel.Graphics.Text IS
   TYPE textbox_data IS ARRAY(num RANGE <>, num RANGE <>) OF character;
   FOR textbox_data'component_size USE 8;

   TYPE textbox(
      Textbox_Character_Width  : num;
      Textbox_Character_Height : num)
   IS RECORD
      -- Holds the data for the textbox. It may contain junk upon declaration.
      Data              : textbox_data(1 .. Textbox_Character_Height,
                                       1 .. Textbox_Character_Width);
      -- The X index of the current cursor, which is the column.
      Current_X_Index   :   num := 1;
      -- The Y index of the current cursor, which is the row.
      Current_Y_Index   :   num := 1;
      -- Set to a black background by default.
      Background_Colour : pixel := 0;
      -- Set to white text by default.
      Foreground_Colour : pixel := 16#FFFFFF#;
      -- The distance between drawn characters is 2 pixels by default.
      Kerning           :   num := 2;
      -- TODO: Font size is currently unused, but I think I can "upscale"
      -- the font if I want?
      Font_Size         :   num := 1;
      -- The line separation between rows is 3 pixels by default.
      -- This does not include the font height, that is handled automatically.
      Line_Separation   :   num := 3;
   END RECORD;

   -- TODO: Use the Object.Method notation for textboxes. I can't do that
   -- right now, because I don't have a secondary stack. Just keep the object
   -- as the first parameter to ensure compatibility later on.

   PROCEDURE Shift_Line(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Update_Cursor(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Print(
      Current_Textbox   : IN OUT textbox;
      Message           : IN str);

   PROCEDURE Next_Line(
      Current_Textbox   : IN OUT textbox);

   PROCEDURE Draw_Character(
      Buffer            : IN OUT framebuffer;
      Pixel_Start       : IN num;
      Foreground_Colour : IN pixel;
      Background_Colour : IN pixel;
      ASCII             : IN character);

   PROCEDURE Draw_Textbox(
      Buffer            : IN OUT framebuffer;
      Current_Textbox   : IN textbox;
      Pixel_Start       : IN num);

   PROCEDURE Clear_Textbox(
      Current_Textbox   : IN OUT textbox)
   WITH
      Inline => true;
END HAVK_Kernel.Graphics.Text;
