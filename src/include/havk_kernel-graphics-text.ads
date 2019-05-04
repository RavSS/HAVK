WITH
   HAVK_Kernel,
   HAVK_Kernel.Graphics;
USE
   HAVK_Kernel.Graphics;

PACKAGE HAVK_Kernel.Graphics.Text IS
   TYPE textbox_data IS ARRAY(u32 RANGE <>, u32 RANGE <>) OF character;

   TYPE textbox(
      Textbox_Character_Width : u32;
      Textbox_Character_Height : u32)
   IS RECORD
      -- Initialize the characters to null characters by default.
      Data : textbox_data(1 .. Textbox_Character_Height,
         1 .. Textbox_Character_Width) :=
         (OTHERS => (OTHERS => character'val(0)));
      -- The X index of the current cursor, which is the column.
      Current_X_Index : u32 := 1;
      -- The Y index of the current cursor, which is the row.
      Current_Y_Index : u32 := 1;
      -- Set to a black background by default.
      Background_Colour : u32 := 0;
      -- Set to white text by default.
      Foreground_Colour : u32 := 16#FFFFFF#;
      -- The distance between drawn characters is 2 pixels by default.
      Kerning : u32 := 2;
      -- TODO: Font size is currently unused, but I think I can "upscale"
      -- the font if I want?
      Font_Size : u8 := 1;
      -- The line separation between rows is 3 pixels by default.
      -- This does not include the font height, that is handled automatically.
      Line_Separation : u32 := 3;
   END RECORD;

   PROCEDURE Shift_Lines(
      Current_Textbox : IN OUT textbox;
      Shifts : IN u32);

   PROCEDURE Update_Cursor(
      Current_Textbox : IN OUT textbox);

   PROCEDURE Print(
      Current_Textbox : IN OUT textbox;
      Message : IN str);

   PROCEDURE Next_Line(
      Current_Textbox : IN OUT textbox);

   PROCEDURE Draw_Character(
      Buffer : IN OUT framebuffer;
      Pixel_Start : IN u64;
      Pixel_Colour : IN u32;
      ASCII : IN character);

   PROCEDURE Draw_Textbox(
      Buffer : IN OUT framebuffer;
      Current_Textbox : IN textbox;
      Pixel_Start : IN u64);
END HAVK_Kernel.Graphics.Text;
