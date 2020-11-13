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
   HAVK_Kernel.Memory.Frames;

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

   -- Verify what the bootloader passed to us.
   PROCEDURE Check_Entry;

   -- Logs arbitrary information about the memory map and its descriptors.
   PROCEDURE Memory_Map_Info;

   -- Initialises the system call instruction's handler by preparing the MSRs.
   PROCEDURE System_Call_Instruction
   RENAMES System_Call.Handler.Set_MSRs;

   -- Retrieves the date and time in ISO 8601 format of when the current
   -- running version of the kernel was compiled and built. The format returned
   -- in the string is "YYYY-MM-DDTHH:MM:SS".
   SUBTYPE datetime_string IS string(1 .. 19);
   FUNCTION HAVK_Build_Datetime
      RETURN datetime_string;

   -- Initialises any debugging utilities.
   PROCEDURE Debugger
   WITH
      Inline => true;

   -- Checks the boot configuration options.
   -- TODO: This probably deserves its own package, as a bit of parsing logic
   -- is contained in here.
   PROCEDURE Boot_Configuration_Check;

   -- This executes the `CPUID` instruction and outputs a few bits of
   -- interesting information.
   PROCEDURE CPU_Feature_Check;

   -- Prepare the heap, as certain functionality like manipulating virtual
   -- addresses requires dynamic memory.
   PROCEDURE Dynamic_Memory
   RENAMES Memory.Frames.Prepare_Kernel_Heap;

   -- Sets up the mechanisms for multi-tasking and leaves kernel-level
   -- initialisation. Everything that doesn't have support for concurrency
   -- needs to be called before calling this is even considered.
   PROCEDURE Begin_Tasking
   WITH
      No_Return => true;

PRIVATE
   Initialise_Tag         : CONSTANT string := "INIT";

   -- The root server program that will start everything else up.
   Initialiser_Name       : CONSTANT string := "Initialiser";
   Initialiser_Address    : Memory.canonical_address := 0;
   Initialiser_Size       : number := 0;

   -- An ATA PIO driver with the bare essentials to start loading files off the
   -- EFI system partition (ESP).
   ATA_PIO_Name           : CONSTANT string := "ATA PIO Driver";
   ATA_PIO_Driver_Address : Memory.canonical_address := 0;
   ATA_PIO_Driver_Size    : number := 0;

END HAVK_Kernel.Initialise;
