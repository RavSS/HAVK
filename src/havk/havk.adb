WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text,
   HAVK_Kernel.Initialise;
USE
   HAVK_Kernel,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

-- This is the main procedure, it is where HAVK starts itself after entry.
PROCEDURE HAVK
WITH
   No_Return     => true,
   SPARK_Mode    => off -- My `gnatprove` version can't process "enum_rep".
IS
   -- Retrieve the arguments passed by HAVK's UEFI bootloader.
   Bootloader      : CONSTANT UEFI.arguments  := Initialise.Get_Arguments;
   -- The UEFI memory map, which is an array of memory (region) descriptors.
   Map             : CONSTANT UEFI.memory_map := Initialise.Get_Memory_Map;
   -- Get an object which describes the system display.
   Display         : CONSTANT view            := Get_Display(Bootloader);

   -- The terminal where I will display things to the user concerning the
   -- current system like warnings, errors, info, etc.
   -- Font width and height are both 8 pixels, so divide by 10 the
   -- horizontal resolution (2 for default kerning) and divide by 11
   -- the vertical resolution (3 for default line separation). A small
   -- border is placed around the terminal.
   Terminal_Border : CONSTANT num := Display.Screen_Width / 4;
   Terminal_Start  : CONSTANT num := Display.Calculate_Pixel(
      Terminal_Border / 2, Terminal_Border / 2);
   Terminal        : textbox(
      (Display.Screen_Width  - Terminal_Border) / 10,
      (Display.Screen_Height - Terminal_Border) / 11);

   Welcome         : CONSTANT string := "WELCOME TO HAVK";
   Date_Of_Build   : CONSTANT string := Initialise.HAVK_Build_Datetime;
BEGIN
   -- Initialise the debugging message mechanism if debugging is enabled.
   -- As of now, this only sends information over a serial connection.
   PRAGMA Debug(Initialise.Debugger);

   Log("Entered main kernel procedure.");

   Initialise.Grid_Test(Display, Display.Create_Pixel(70, 10, 10));
   Initialise.Descriptor_Tables;
   Initialise.Default_Page_Layout(Bootloader, Map);

   -- Set up the terminal.
   Terminal.Clear_All;
   Terminal.Start_Position    := Terminal_Start;
   Terminal.Background_Colour := Display.Create_Pixel(  0,  0, 0);
   Terminal.Foreground_Colour := Display.Create_Pixel(200, 55, 0);

   -- Print the welcome message and date of the current build's compilation.
   Terminal.Print(Welcome, Centre => true);
   Terminal.Newline;
   Terminal.Print("COMPILED AT " & Date_Of_Build, Centre => true);
   Terminal.Newline(2);

   -- Show the magic number value.
   Initialise.See_Magic(Terminal);
   Terminal.Newline;

   -- Print the font test.
   Initialise.Font_Test(Terminal);
   Terminal.Newline;

   -- TODO: Needs work.
   Terminal.Print("MEMORY MAP ENUMERATION:");
   Terminal.Newline;
   Terminal.Print("   MEMORY DESCRIPTORS:" & num'image(Map'length));
   Terminal.Newline(2);

   Terminal.Draw_On(Display);
   Log("First terminal draw done.");

   -- Prepare primitive forms of input via PS/2.
   Initialise.PS2_Input;
   Initialise.Input_Key_Test(Terminal, Display);

   Initialise.Seconds_Count(Terminal,  Display);
END HAVK;
