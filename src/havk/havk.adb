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
   No_Return     => true
IS
   -- Access the arguments passed by HAVK's UEFI bootloader.
   Bootloader      : CONSTANT UEFI.access_arguments
   WITH
      Import     => true,
      Convention => NASM,
      Link_Name  => "bootloader.arguments";

   Display         : view := Create_View(Bootloader.ALL);

   -- The UEFI memory map. Assumes the descriptor size has been "fixed".
   Map             : CONSTANT UEFI.memory_map(0 .. Bootloader.Memory_Map_Size /
      Bootloader.Memory_Map_Descriptor_Size)
   WITH
      Import     => true,
      Convention => C,
      Address    => Bootloader.Memory_Map_Address;

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
   PRAGMA Debug(Debug_Initialise);
   PRAGMA Debug(Debug_Message("Entered main kernel procedure."));

   Initialise.Grid_Test(Display, Display.Create_Pixel(70, 10, 10));
   Initialise.Descriptor_Tables;
   Initialise.Default_Page_Layout(Bootloader.ALL, Map);

   Initialise.PS2_Input;

   -- Set up the terminal.
   Terminal.Clear_All;
   Terminal.Position := Terminal_Start; -- Set the top-left corner of the box.
   Terminal.Background_Colour := Display.Create_Pixel(  0,  0, 0);
   Terminal.Foreground_Colour := Display.Create_Pixel(200, 55, 0);

   -- Print the welcome message and date of the current build's compilation.
   Terminal.Print(Welcome,       Centre => true);
   Terminal.Newline;
   Terminal.Print(Date_Of_Build, Centre => true);
   Terminal.Newline(2);

   -- Print the font test.
   Initialise.Font_Test(Terminal);
   Terminal.Newline;

   -- TODO: Needs work.
   Terminal.Print("MEMORY MAP ENUMERATION:" & character'val(0));
   Terminal.Newline;
   Terminal.Print("   MEMORY DESCRIPTORS:" & num'image(Map'length));
   Terminal.Newline;
   Terminal.Print("   INSTALLED RAM SIZE:" & num'image((
      Map(Map'last).Start_Address_Physical'address +
      Map(Map'last).Number_Of_Pages * 4096) / 1024 / 1024) &
      " MEBIBYTES");
   Terminal.Newline(2);

   Terminal.Draw_On(Display);
   PRAGMA Debug(Debug_Message("First terminal draw done."));

   Initialise.Input_Key_Test(Terminal, Display);

   PRAGMA Debug(Debug_Message("Reached the end of the HAVK procedure."));
   Initialise.Seconds_Count(Terminal,  Display);
END HAVK;
