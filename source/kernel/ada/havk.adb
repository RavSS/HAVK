-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel,
   HAVK_Kernel.Initialise;
USE
   HAVK_Kernel;

-- This is the first phase and it is the main procedure. Information critical
-- to the system should be parsed in mono-tasking mode first.
PROCEDURE HAVK
WITH
   No_Return => true
IS
   Entry_Tag : CONSTANT string := "ENTRY";
BEGIN
   -- Set up any serial port functionality etc.
   Initialise.Debugger;

   Log("Entered HAVK successfully.", Tag => Entry_Tag);
   Log("Date of build: " & Initialise.HAVK_Build_Datetime & '.',
      Tag => Entry_Tag);

   -- See what the bootloader passed as the magic number and anything else.
   Initialise.Check_Entry;

   -- Allow kernel heap allocations and deallocations after this returns.
   -- Also parses the UEFI memory map.
   Initialise.Dynamic_Memory;

   -- See what the processor can do and then output some text about it.
   Initialise.CPU_Feature_Check;

   -- Create new descriptor tables and make interrupts possible.
   Initialise.Descriptor_Tables;

   -- Use a new page structure and map the kernel, UEFI/ACPI data, etc.
   Initialise.Default_Page_Layout;

   -- Check the memory map and log any information.
   Initialise.Memory_Map_Info;

   -- Verify the ACPI implementation and set up the APICs if we can.
   Initialise.Interrupt_Controllers;

   -- Now we can receive interrupts, prepare the timers properly.
   Initialise.Timers;

   -- Set up the system call instruction's functionality and options.
   Initialise.System_Call_Instruction;

   -- Prepare primitive forms of input via PS/2.
   Initialise.PS2_Input;

   -- Begin multi-tasking and leave this phase.
   Initialise.Begin_Tasking;
END HAVK;
