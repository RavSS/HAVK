-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.ads                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   SPARK.Heap,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Descriptors,
   HAVK_Kernel.APIC,
   HAVK_Kernel.APIC.Timer,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Memory.Manager;
USE TYPE
   HAVK_Kernel.Paging.access_page_layout;

-- TODO: I've had a look at the native RTS for GNAT CE 2019 on x86-64 Linux and
-- I think I could shift some code to a new version of that RTS for proper Ada
-- tasking, which would give me access to the nice syntax like it did with the
-- "NEW" heap allocation keyword. I think it could help prevent race conditions
-- and also help `gnatprove` showcase that there are none. The actual (main)
-- runtime package for tasking seems to be "System.Tasking", which is the
-- s-taskin.ad{s,b} file. There's a ton of features that need to be implemented
-- for the compiler to accept it (from my observations). If I do manage to
-- implement it, then it would make managing multiprocessing easier too after I
-- trampoline the APs. For now, I'll stick to this current package since it's
-- easy and we only have the BSP available to us until the I/O APICs are used.

-- This package details multi-tasking logic. I know that Ada has tasking
-- functionality, but for now, it will probably be much simpler to implement it
-- without any language-centric features which need to be baked into the RTS.
-- All tasks created run in ring 3; nothing except the kernel is permitted to
-- run normally in ring 0. System calls will need to be used instead.
-- TODO: Error checking needs to be improved and made coherent.
PACKAGE HAVK_Kernel.Tasking
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      Tasking_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes)
   )
IS
   -- Creates a new task. Interrupts are disabled during its call until return.
   -- TODO: Due to how I've implemented strict NX compliance, flat binaries
   -- can't store data properly. I'm not going to add in the equivalent of
   -- Linux's `mprotect()`, so serious work needs to be done here. An ELF
   -- loader is very likely necessary in the kernel itself.
   PROCEDURE Create
     (Task_Name  : IN string;
      Code_Size  : IN number;
      Heap_Size  : IN number;
      Stack_Size : IN number := 16 * KiB)
   WITH
      Global => (In_Out => (Tasking_State, Descriptors.TSS,
                            Paging.Kernel_Paging_Layout,
                            Memory.Manager.Frame_Allocator_State),
                 Input  => (SPARK.Heap.Dynamic_Memory,
                            Memory.Memory_State, Memory.Kernel_Virtual_Base,
                            Memory.Kernel_Isolated_Text_Base,
                            Memory.Kernel_Isolated_Text_Size,
                            Memory.Kernel_Isolated_Data_Base,
                            Memory.Kernel_Isolated_Data_Size,
                            Memory.Kernel_Isolated_BSS_Base,
                            Memory.Kernel_Isolated_BSS_Size,
                            UEFI.Bootloader_Arguments)),
      Pre    => Code_Size  MOD Paging.Page = 0          AND THEN
                Heap_Size  MOD Paging.Page = 0          AND THEN
                Stack_Size MOD Paging.Page = 0          AND THEN
                Code_Size   IN Paging.Page .. 2 * GiB   AND THEN -- TODO: Need
                Heap_Size   IN Paging.Page .. 128 * GiB AND THEN -- clearer
                Stack_Size  IN Paging.Page .. 2 * MiB   AND THEN -- limits.
                Task_Name'first = 1                     AND THEN
                Task_Name'last IN Task_Name'first .. 64;

   -- Changes the CR3 register to the default kernel page layout's L4.
   -- Used on entry of ISRs and other bits of kernel code that needs exposure
   -- to all sections of the HAVK kernel.
   PROCEDURE Switch_To_Kernel_CR3
   WITH
      Global         => (Input => Memory.Memory_State),
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__switch_to_kernel_cr3",
      Linker_Section => ".isolated_text";

   -- The same as `Swap_To_Kernel_CR3()`, except that it changes to the active
   -- task's page layout. Does not do anything if tasking is not enabled, so it
   -- can be called long before tasking is set up.
   PROCEDURE Switch_To_Task_CR3
   WITH
      Global         => (Input => Memory.Memory_State),
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__switch_to_task_cr3",
      Linker_Section => ".isolated_text";

   -- Returns the physical address of the code entry. This allows you to map it
   -- and then load instructions at it as well.
   FUNCTION Get_Task_Physical_Entry
     (Task_Name : IN string)
      RETURN address
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State),
      Pre               => Task_Name'first = 1 AND THEN
                           Task_Name'last IN Task_Name'first .. 64;

   -- Just returns the active task's name.
   FUNCTION Get_Active_Task_Name
      RETURN string
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State),
      Post              => Get_Active_Task_Name'result'first = 1 AND THEN
                           Get_Active_Task_Name'result'last IN
                              Get_Active_Task_Name'result'first .. 64;

   -- Determines whether or not to switch tasks. Can also act as a task yield.
   -- For now, this is just a bad round-robin implementation.
   PROCEDURE Schedule
   WITH
      Global => (In_Out => (Tasking_State, Descriptors.TSS),
                 Input  => APIC.Timer.Ticks);

   -- Changes HAVK from a mono-tasking kernel to a multi-tasking kernel.
   -- Does not return. The first task is the first one entered.
   PROCEDURE Start
   WITH
      Global        => (In_Out => Tasking_State),
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__start_tasking";

PRIVATE
   -- TODO: This is not complete with the SIMD registers etc.
   TYPE callee_saved_registers IS RECORD
      RBX : number;
      RBP : number;
      R12 : number;
      R13 : number;
      R14 : number;
      R15 : number;
      SS  : number RANGE 0 .. 2**16 - 1; -- All other segments are the same.
   END RECORD
   WITH
      Convention => Assembler;
   FOR callee_saved_registers USE RECORD
      RBX AT 00 RANGE 0 .. 63;
      RBP AT 08 RANGE 0 .. 63;
      R12 AT 16 RANGE 0 .. 63;
      R13 AT 24 RANGE 0 .. 63;
      R14 AT 32 RANGE 0 .. 63;
      R15 AT 40 RANGE 0 .. 63;
      SS  AT 48 RANGE 0 .. 63; -- Zero-extended to 64 bits when `MOV`'d.
   END RECORD;

   -- Details a task's various states and settings.
   TYPE task_control_block IS RECORD
      Name                : string(1 .. 64) := (OTHERS => character'val(0));
      State               : ALIASED callee_saved_registers := (OTHERS => 0);
      Stack               : address := 0;
      Stack_Size          : number := 0;
      Kernel_Stack        : address := 0;
      Kernel_Stack_Size   : number := 0;
      Physical_Space      : Memory.Manager.space;
      Physical_Entry      : address := 0;
      Physical_Stack_Base : address := 0;
      Virtual_Space       : Paging.access_page_layout := NULL;
      Alive               : boolean := false;
      -- TODO: Adjust this dynamically.
      Max_Ticks           : number RANGE 1 .. number'last := 1;
   END RECORD
   WITH
      Dynamic_Predicate => Stack_Size MOD Paging.Page = 0        AND THEN
                           Kernel_Stack_Size MOD Paging.Page = 0 AND THEN
                          (IF Alive THEN
                              Virtual_Space /= NULL AND THEN
                              Name /= (1 .. 64 => character'val(0)));

   -- Indicates whether or not tasking is enabled. This is mainly for usage
   -- within the scheduler and the appropriate page layout switchers.
   Enabled : ALIASED boolean := false
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "var__tasking_enabled",
      Linker_Section => ".isolated_bss";

   -- I've gone with a flat array instead of something like a linked list
   -- because it's easier to prove with the current SPARK tools.
   -- It's definitely possible, but simplicity is the focus.
   TYPE access_task_control_block IS ACCESS task_control_block;
   TYPE task_control_blocks IS ARRAY(number RANGE <>) OF
      access_task_control_block;

   -- A max of 256 processes for now. This can be changed whenever later on,
   -- it's just a reasonable limit for now.
   SUBTYPE task_limit IS number RANGE 1 .. 256;

   -- The main array of tasks. I avoided a linked list because it's harder
   -- to prove and relies on pointers more. It's definitely possible to do
   -- with SPARK's current tools, but I don't believe it is worth it.
   Tasks       : task_control_blocks(task_limit)
   WITH
      Part_Of => Tasking_State;

   -- Indicates the task which is being given CPU time.
   Active_Task : task_limit := task_limit'first
   WITH
      Part_Of => Tasking_State;

   -- For now, I'll focus on just supporting flat binary files that start at
   -- 0x1000 to avoid clashing with the null page. We'll use a small code model
   -- instead of a medium or large one. GCC should select it automatically.
   -- Later on, ELF files should recognise this as the standard base as well.
   Virtual_Entry             : CONSTANT address := 16#1000#;

   -- This is the virtual address for RSP that is pushed onto the actual stack
   -- upon a task's creation. I've gone with a 32-bit (signed) integer's
   -- maximum value, so it can fit inside ESP and 64-bit addressing issues can
   -- be ignored, as the memory model is not "large". This does mean that we're
   -- limited to programs under 2 GiB, but that's really enough.
   Virtual_Stack_Base        : CONSTANT address := address(integer'last);

   -- This is just 8 KiB for the TSS's ring 0 RSP. I don't see a need in
   -- making it a dynamic size yet. Perhaps I could use stacks from the IST.
   Default_Kernel_Stack_Size : CONSTANT := 8 * KiB;

   -- Begins the first step of the tasking switch. This is where tasks go to
   -- stand-by while other tasks take off from here after being active. This
   -- effectively calls `INT 100`, which modifies the CR3 register, so this is
   -- placed in the isolated text section to maintain mappings between the
   -- kernel and all tasks.
   PROCEDURE Round_Robin
     (Yield : IN boolean := false)
   WITH
      Global         => (In_Out   => (Tasks, Active_Task, Descriptors.TSS),
                         Input    => APIC.Timer.Ticks,
                         Proof_In => Enabled),
      Linker_Section => ".isolated_text",
      Pre            => Enabled AND THEN Tasks(Active_Task) /= NULL,
      Post           => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- Raises `INT 49`, which is the interrupt vector for the context switch
   -- ISR. Do not redefine the external name to call the
   -- `assembly__switch_task()` routine directly. This lets the context switch
   -- itself be independent and additionally change the CR3 contents based on
   -- which ring it was called from.
   PROCEDURE Switch
   WITH
      Global         => (In_Out => (Tasks, Active_Task, Descriptors.TSS)),
      Import         => true,
      Convention     => Assembler,
      External_Name  => "assembly__raise_interrupt_049",
      Linker_Section => ".isolated_text",
      Pre            => Tasks(Active_Task) /= NULL,
      Post           => Tasks(Active_Task) /= NULL;

   -- This stores the active task's stack and moves onto the next task.
   -- It changes exactly what the `Get_*` functions below return.
   PROCEDURE Store_Task
     (Task_Stack : IN address)
   WITH
      Global        => (In_Out   => (Tasks, Active_Task, Descriptors.TSS),
                        Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__store_task",
      Pre           => Enabled AND THEN Tasks(Active_Task) /= NULL AND THEN
                      (FOR SOME Tasked OF Tasks => Tasked /= NULL),
      Post          => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- Simply returns `Tasks(Active_Task).Stack`. This is here so the
   -- calculation does not have to be done in assembly and can be done much
   -- more safely at the cost of needing a temporary task management stack.
   FUNCTION Get_Task_Stack
      RETURN address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task_stack",
      Pre           => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- Returns a thin pointer to the active task's state, which is a record
   -- for callee-saved registers.
   FUNCTION Get_Task_State
      RETURN address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task_state",
      Pre           => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- Returns the base address of the task's level 4 page map (PML4) to put
   -- into the CR3 register. This is much easier to calculate in Ada.
   FUNCTION Get_Task_CR3
      RETURN address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task_cr3",
      Pre           => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- This sets up the stack for a task by allocating a stack and pushing
   -- important information onto it. It takes in a physical address of the
   -- stack's end (the highest address value possible in an x86 stack limit).
   -- It's your job to make sure the stack is at least larger than 48 bytes, so
   -- keep obvious sane limits.
   PROCEDURE Prepare_Task_Stack
     (Task_Index : IN task_limit;
      Stack_Base : IN address)
   WITH
      Global => (In_Out => Tasks),
      Pre    => Tasks(Task_Index) /= NULL,
      Post   => Tasks(Task_Index) /= NULL;

END HAVK_Kernel.Tasking;
