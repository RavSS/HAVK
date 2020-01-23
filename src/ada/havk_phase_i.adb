-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_phase_i.adb                                       --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

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

-- This is the first phase and it is the main procedure. Information critical
-- to the system should be parsed in mono-tasking mode first.
PROCEDURE HAVK_Phase_I
WITH
   No_Return => true
IS
   -- Get an object which describes the system display.
   Display        : CONSTANT view := Get_Display(UEFI.Get_Arguments);

   -- The main terminal or virtual console used to display text to the user.
   Terminal       : textbox( -- Both the font's width and height are 8 pixels.
      Display.Screen_Width  / 14 + 2,  -- (Font width  - 1) * 2 + 2. Wider.
      Display.Screen_Height / 14 - 2); -- (Font height - 1) * 2 - 2. Shorter.

   -- The index on the framebuffer as to where the terminal begins.
   Terminal_Start : CONSTANT number := Display.Calculate_Pixel(
      Display.Screen_Width  / 8,
      Display.Screen_Height / 8);

   Welcome        : CONSTANT string := "WELCOME TO HAVK";
   Date_Of_Build  : CONSTANT string := Initialise.HAVK_Build_Datetime;
BEGIN
   -- Initialise the debugging message mechanism if debugging is enabled.
   -- As of now, this only sends information over a serial connection.
   PRAGMA Debug(Initialise.Debugger);

   Log("Entered Phase I successfully.", nominal);

   -- Set up the terminal.
   Terminal.Start_Position    := Terminal_Start;
   Terminal.Background_Colour := Display.Create_Pixel(  0,  0, 0);
   Terminal.Foreground_Colour := Display.Create_Pixel(200, 55, 0);

   -- Print the welcome message and date of the current build's compilation.
   Terminal.Print(Welcome, Centre => true);
   Terminal.Newline;
   Terminal.Print("COMPILED AT " & Date_Of_Build, Centre => true);
   Terminal.Newline;
   Terminal.Print("COMPILER WAS " & Standard'compiler_version, Centre => true);
   Terminal.Newline(2);

   -- Show the magic number value.
   Initialise.See_Magic(Terminal);
   Terminal.Newline;

   -- Show a basic graphical shape on screen.
   Initialise.Grid_Test(Display, Display.Create_Pixel(70, 10, 10));
   Log("Grid test drawn to the main framebuffer.");

   -- Print the font test.
   Initialise.Font_Test(Terminal);
   Terminal.Newline;

   -- Check the memory map.
   Initialise.Memory_Map_Info(Terminal);
   Terminal.Newline;

   Terminal.Draw_On(Display);
   Log("First terminal draw done.");

   -- Verify the ACPI implementation and set up the APICs if we can.
   Initialise.Interrupt_Controllers;

   -- Create new descriptor tables and make interrupts possible.
   Initialise.Descriptor_Tables;

   -- Use a new page structure and map the kernel, UEFI/ACPI data, etc.
   Initialise.Default_Page_Layout;

   -- Set-up the system call instruction's functionality and options.
   Initialise.System_Call_Instruction;

   -- Prepare primitive forms of input via PS/2.
   Initialise.PS2_Input;

   -- Do a variety of tests if the user wishes to do so.
   Initialise.Tests(Terminal, Display);

   Terminal.Print("START-UP WAS SUCCESSFUL.");
   Terminal.Newline(2);
   Terminal.Print("ENTERING PHASE II.", Centre => true);
   Terminal.Draw_On(Display);

   -- Begin multi-tasking and leave this phase.
   Initialise.Enter_Phase_II;
END HAVK_Phase_I;
