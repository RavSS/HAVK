-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk.adb                                               --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text,
   HAVK_Kernel.Initialise;
USE
   HAVK_Kernel,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;

-- This is the main procedure, it is where HAVK starts itself after entry.
PROCEDURE HAVK
IS
   -- Get an object which describes the system display.
   Display        : CONSTANT view := Get_Display(UEFI.Get_Arguments);

   -- The main terminal or virtual console used to display text to the user.
   Terminal       : textbox( -- Both the font's width and height are 8 pixels.
      Display.Screen_Width  / 14 + 2,  -- (Font width  - 1) * 2 + 2. Wider.
      Display.Screen_Height / 14 - 2); -- (Font height - 1) * 2 - 2. Shorter.

   -- The index on the framebuffer as to where the terminal begins.
   Terminal_Start : CONSTANT num := Display.Calculate_Pixel(
      Display.Screen_Width  / 8,
      Display.Screen_Height / 8);

   Welcome        : CONSTANT string := "WELCOME TO HAVK";
   Date_Of_Build  : CONSTANT string := Initialise.HAVK_Build_Datetime;
BEGIN
   -- Initialise the debugging message mechanism if debugging is enabled.
   -- As of now, this only sends information over a serial connection.
   PRAGMA Debug(Initialise.Debugger);

   Log("Entered main kernel procedure.");

   Initialise.Grid_Test(Display, Display.Create_Pixel(70, 10, 10));
   Initialise.Descriptor_Tables;
   Initialise.Default_Page_Layout;

   -- Heap allocation is possible after this returns.
   Memory.Prepare_Heap(Initialise.Kernel_Paging_Layout);

   -- Set up the terminal.
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

   -- Check the memory map.
   Initialise.Memory_Map_Info(Terminal);
   Terminal.Newline;

   Terminal.Draw_On(Display);
   Log("First terminal draw done.");

   -- Prepare primitive forms of input via PS/2.
   Initialise.PS2_Input;
   Initialise.Input_Key_Test(Terminal, Display);

   -- Count seconds until the user exits it.
   Initialise.Seconds_Count(Terminal,  Display);

   -- Don't call the last chance handler on return, instead stop the processor.
   Log("End of HAVK procedure reached. CPU reset imminent.", warning);
   Terminal.Newline(2);
   Terminal.Print("HAVK IS A WORK-IN-PROGRESS FROM HERE, EXITING...");
   Terminal.Newline(2);
   Terminal.Print("THE CPU HAS BEEN RESET", Centre => true);
   Terminal.Draw_On(Display);
END HAVK; -- Upon return, the entry routine resets the CPU and halts forever.
