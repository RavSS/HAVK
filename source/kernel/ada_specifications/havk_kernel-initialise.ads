-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-initialise.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Descriptors,
   HAVK_Kernel.System_Call,
   HAVK_Kernel.System_Call.Handler,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Memory.Frames,
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text,
   HAVK_Kernel.Drive,
   HAVK_Kernel.Drive.FAT;
USE
   HAVK_Kernel.Graphics,
   HAVK_Kernel.Graphics.Text;
USE TYPE
   HAVK_Kernel.Drive.FAT.version;

-- This package contains all of the initialisation routines for the other
-- packages. The reason is to keep the main procedures more readable.
PACKAGE HAVK_Kernel.Initialise
IS
   -- Prepares the descriptor tables, which is necessary for interrupts
   -- and switching DPLs etc.
   PROCEDURE Descriptor_Tables
   RENAMES Descriptors.Load;

   -- Set up the Advanced Programmable Interrupt Controllers (APICs) depending
   -- on which ones are available. This also verifies the ACPI tables.
   PROCEDURE Interrupt_Controllers;

   -- Initialises any timers that are available.
   PROCEDURE Timers;

   -- Identity maps the kernel's address space to the default page layout.
   PROCEDURE Default_Page_Layout;

   -- Draws a grid to the screen as an initial test.
   PROCEDURE Grid_Test
     (Display  : IN view;
      Colour   : IN pixel);

   -- Prints the magic number to the textbox.
   PROCEDURE See_Magic
     (Terminal : IN OUT textbox);

   -- Prints the nearly all of the current font's characters.
   PROCEDURE Font_Test
     (Terminal : IN OUT textbox);

   -- Outputs the character from the input handler to the textbox.
   PROCEDURE Input_Key_Test
     (Terminal : IN OUT textbox;
      Display  : IN view);

   -- Prints the amount of seconds passed (since call) to a textbox endlessly.
   PROCEDURE Seconds_Count
     (Terminal : IN OUT textbox;
      Display  : IN view);

   -- Show arbitrary information about the memory map and its descriptors.
   PROCEDURE Memory_Map_Info
     (Terminal : IN OUT textbox);

   -- Initialises the PS/2 controller for keyboard input purposes (as of now).
   PROCEDURE PS2_Input;

   -- Initialises the system call instruction's handler by preparing the MSRs.
   PROCEDURE System_Call_Instruction
   RENAMES System_Call.Handler.Set_MSRs;

   -- Retrieves the date and time in ISO 8601 format of when the current
   -- running version of the kernel was compiled and built.
   FUNCTION HAVK_Build_Datetime
      RETURN string
   WITH
      Post => HAVK_Build_Datetime'result'first = 01 AND THEN
              HAVK_Build_Datetime'result'last  = 19; -- "YYYY-MM-DDTHH:MM:SS"

   -- Initialises any debug utilities. The "Printing" parameter controls
   -- whether or not logs are sent to the terminal/textbox.
   PROCEDURE Debugger
     (Terminal : IN OUT textbox;
      Printing : IN boolean)
   WITH
      Inline => true;

   -- Does the PS/2 input and IRQ 0 count tests for now, but it can be expanded
   -- to anything else as well.
   PROCEDURE Tests
     (Terminal : IN OUT textbox;
      Display  : IN view);

   -- This executes the `CPUID` instruction and outputs a few bits of
   -- interesting information.
   PROCEDURE CPU_Feature_Check
     (Terminal : IN OUT textbox);

   -- Finds the boot partition and returns it in the first parameter.
   PROCEDURE Boot_Partition_Check
     (EFI_File_System : OUT Drive.FAT.file_system)
   WITH
      Post => Drive.FAT.Get_FAT_Version(EFI_File_System) = Drive.FAT.FAT16;

   -- Prepare the heap, as certain functionality like manipulating virtual
   -- addresses requires dynamic memory.
   PROCEDURE Dynamic_Memory
   RENAMES Memory.Frames.Prepare_Kernel_Heap;

   -- Sets up the mechanisms for multi-tasking and leaves initialisation.
   -- Everything that doesn't have support for concurrency needs to be called
   -- before calling this is even considered.
   PROCEDURE Begin_Tasking
   WITH
      No_Return => true;

PRIVATE
   Initialise_Tag : CONSTANT string := "INIT";

END HAVK_Kernel.Initialise;
