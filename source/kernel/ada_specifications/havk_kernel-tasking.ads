-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.ads                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   SPARK.Heap,
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Descriptors,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Memory.Frames;
USE TYPE
   HAVK_Kernel.UEFI.access_memory_descriptor,
   HAVK_Kernel.Intrinsics.general_register;

-- This package details multi-tasking logic. I know that Ada has tasking
-- functionality, but for now, it will probably be much simpler to implement it
-- without any language-centric features which need to be baked into the RTS.
-- All tasks created run in ring 3; nothing except the kernel is permitted to
-- run normally in ring 0. System calls will need to be used instead. Note that
-- I use the terms "task" and "process" synonymously. Previously, HAVK used a
-- 1:1 threading model, but now, an N:1 threading model (user space scheduling)
-- must be used, as thread capabilities have been cut from the kernel on
-- purpose for the purposes of reducing complexity.
-- TODO: Error checking needs to be improved and made coherent.
-- TODO: Look into Ada's tasking functionality and see if this functionality
-- can be integrated into there. I cannot seem to figure out how it's
-- implemented in the RTS nor where to start looking, as there's multiple
-- files.
-- TODO: I've rushed the contracts and added numerous panic exceptions to
-- account for odd cases where the task scheduler loses track of the active
-- task or active thread. Need to define them much more accurately.
PACKAGE HAVK_Kernel.Tasking
WITH
   Abstract_State =>
   (
      Tasking_State
      WITH
         External => (Async_Readers, Async_Writers,
                      Effective_Reads, Effective_Writes)
   )
IS
   -- The limits of a task's name.
   SUBTYPE task_name_string IS string(1 .. 64);

   -- A record that describes the current status of a task.
   TYPE task_status IS RECORD
      -- The index of the task. If this is zero, then the task does not exist.
      Index   : number := 0;
      -- Indicates whether the task is alive or not. The task can exist yet
      -- also be dead.
      Alive   : boolean := false;
      -- The name the task was created with.
      Name    : task_name_string := (OTHERS => NUL);
   END RECORD
   WITH
      Dynamic_Predicate => (IF Index = 0 THEN
                               NOT Alive AND THEN
                              (FOR ALL ASCII OF Name => ASCII = NUL));
   FOR task_status USE RECORD
      Index   AT 00 RANGE 0 .. 63;
      Alive   AT 08 RANGE 0 .. 07;
      Name    AT 09 RANGE 0 .. (8 * task_name_string'length) - 1;
   END RECORD;

   -- Creates a new task. Interrupts are disabled during its call until return.
   -- TODO: Needs better error checking.
   PROCEDURE Create
     (Task_Name      : IN string;
      Initial_Frames : IN number;
      Error_Status   : OUT error)
   WITH
      Global => (In_Out => (Tasking_State, Descriptors.TSS,
                            Paging.Kernel_Page_Layout_State,
                            SPARK.Heap.Dynamic_Memory,
                            Memory.Frames.Frame_Allocator_State),
                 Input  => (Memory.Kernel_Virtual_Base,
                            Memory.Kernel_Isolated_Text_Base,
                            Memory.Kernel_Isolated_Text_Size,
                            Memory.Kernel_Isolated_Data_Base,
                            Memory.Kernel_Isolated_Data_Size,
                            Memory.Kernel_Isolated_BSS_Base,
                            Memory.Kernel_Isolated_BSS_Size,
                            UEFI.Bootloader_Arguments,
                            UEFI.Memory_Map)),
      Pre    => Initial_Frames IN 1 .. (2 * GiB) / Paging.Page AND THEN
                Task_Name'length <= task_name_string'length;

   -- This removes the task at the specified index.
   PROCEDURE Remove
     (Task_Index   : IN number;
      Error_Status : OUT error)
   WITH
      Global => (In_Out => (Tasking_State, SPARK.Heap.Dynamic_Memory,
                            Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State)),
      Post   => Error_Status IN no_error | index_error | attempt_error;

   -- TODO: An early attempt at task cleanup. It marks the active task as dead.
   PROCEDURE Kill_Active_Task
     (Kill_Code : IN number);

   -- Clears out the active task's time slice.
   PROCEDURE Yield;

   -- Does the same as the various address mapping procedures in the paging
   -- package, but for a task instead. A small wrapper.
   PROCEDURE Map_Address_Range
     (Task_Index       : IN number;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size        : IN Paging.page_frame_variant := Paging.Page;
      Present          : IN boolean :=  true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean :=  true)
   WITH
      Inline => true, -- See `Paging.Map_Address()` for info on W^X.
      Pre    => (IF Write_Access THEN No_Execution);

   -- Obtains the index of a task by name. Returns an attempt error if the task
   -- name did not match a task, with the task index then being out of range.
   PROCEDURE Get_Task_Index
     (Task_Name    : IN string;
      Task_Index   : OUT number;
      Error_Status : OUT error)
   WITH
      Global => (Input => Tasking_State),
      Pre    => Task_Name'length <= task_name_string'length,
      Post   => (IF Error_Status = attempt_error THEN
                    Task_Index = number'first);

   -- Returns the index/identity of the active task.
   FUNCTION Get_Active_Task_Index
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State);

   -- Returns the status information for a task.
   FUNCTION Get_Task_Status
     (Task_Index : IN number)
      RETURN task_status
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State);

   -- Determines whether or not to switch tasks. Can also act as a task yield.
   -- For now, this is just a bad round-robin implementation.
   PROCEDURE Schedule
   WITH
      Global => (In_Out => (Tasking_State, Descriptors.TSS));

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
   Tasking_Tag : CONSTANT string := "TASKING";

   -- A max of 256 tasks right now. This can be changed whenever later on, it's
   -- just a reasonable limit for now. Note that task zero is the kernel itself
   -- and is thus reserved.
   SUBTYPE task_limit IS number RANGE 1 .. 256;

   -- A record that stores the callee-saved registers. It should remain
   -- consistent with the struct specified in "tasking.s". Other registers
   -- (including the SIMD registers) are stored in a interrupt stub that an
   -- inactive task will `REX.W IRET` from.
   TYPE callee_saved_registers IS RECORD
      RBX : Intrinsics.general_register := 0;
      RBP : Intrinsics.general_register := 0;
      -- This is the kernel stack in action. Its base address would be in the
      -- TSS descriptor.
      RSP : Intrinsics.general_register := 0;
      R12 : Intrinsics.general_register := 0;
      R13 : Intrinsics.general_register := 0;
      R14 : Intrinsics.general_register := 0;
      R15 : Intrinsics.general_register := 0;
      -- All other segment indices are identical to the SS (except for the CS).
      -- This is not actually loaded into the SS register, but rather the other
      -- segment index registers. I am fairly certain the other registers aside
      -- from CS, GS, FS, and of course SS, are not really used. Zero-extended
      -- up to 64 bits when `MOV`'d, as these are actually special registers.
      SS  : Intrinsics.general_register RANGE 0 .. 2**16 - 1 :=
         Intrinsics.general_register(Descriptors.DS_Ring_3);
   END RECORD
   WITH
      Convention  => Assembler,
      Object_Size => (56 * 8) + 63 + 1;
   FOR callee_saved_registers USE RECORD
      RBX AT 00 RANGE 0 .. 63;
      RBP AT 08 RANGE 0 .. 63;
      RSP AT 16 RANGE 0 .. 63;
      R12 AT 24 RANGE 0 .. 63;
      R13 AT 32 RANGE 0 .. 63;
      R14 AT 40 RANGE 0 .. 63;
      R15 AT 48 RANGE 0 .. 63;
      SS  AT 56 RANGE 0 .. 63;
   END RECORD;

   -- Details a task's various states and settings. The index of the task
   -- itself is considered to be the task identity.
   TYPE task_control_block IS LIMITED RECORD
      -- The name of the task. Only the task has a name, not the threads.
      Name                  : task_name_string := (OTHERS => NUL);
      -- Indicates the frame count of the task's initial code/data which is
      -- given to it in `Create()`.
      Initial_Frames        : Memory.Frames.frame_limit := 0;
      -- This is the base address of the physical frames given to the task upon
      -- create. The size onwards is given by the "Code_Size" field above.
      Physical_Base         : Memory.page_address := 0;
      -- The paging layout for the task and how it sees virtual memory. I've
      -- only given a task a single page layout for now, each thread shares
      -- both virtual and physical memory with each other.
      Virtual_Space         : Paging.page_layout;
      -- The registers the context switcher will load and `REX.W IRET` to with.
      State                 : ALIASED callee_saved_registers;
      -- This goes inside the TSS so the CPU knows which stack to switch to on
      -- an interrupt. The address must be the bottom of the stack, meaning
      -- that it's the highest address and will grow downwards. It is also a
      -- virtual address, as the task must have it mapped while making sure
      -- it's unaware of the physical memory layout. Must be accessible by
      -- ring 0 in all page layouts.
      Kernel_Stack          : Memory.page_address := 0;
      -- The byte size of the kernel stack. This should be a multiple of 4 KiB.
      Kernel_Stack_Size     : number := 0;
      -- This is the physical base of the kernel stack. It's also the address
      -- at which the stack grows downwards like the stack's virtual address.
      -- TODO: For now, I identity-map the kernel stack. I'll need to pick a
      -- sensible virtual range in which to map these.
      Kernel_Stack_Physical : Memory.page_address := 0;
      -- Indicates whether or not the task is alive.
      Alive                 : boolean := false;
      -- TODO: This is just a random value passed by the task as of now.
      -- A value of anything other than zero indicates an error of some sort.
      Exit_Code             : number := 0;
      -- Right now, I am just using round robin without a meaningful priority
      -- algorithm, so this value is how many LAPIC timer interrupts the thread
      -- is supposed to receive.
      -- TODO: Adjust this dynamically.
      Max_Ticks             : number RANGE 1 .. number'last := 10;
      -- This is the next page address of the heap (meaning that the last
      -- mapped address is 4 bits behind it). It is to be handled by a child
      -- package.
      Heap_End              : Memory.page_address := 0;
   END RECORD
   WITH
      Dynamic_Predicate => (IF Alive THEN
                              Initial_Frames IN 1 .. (2 * GiB) / Paging.Page
                                 AND THEN
                              Name /= (Name'range => NUL) AND THEN
                              Exit_Code = 0);

   -- I've gone with a flat array instead of something like a linked list
   -- because it's easier to prove with the current SPARK tools.
   -- It's definitely possible, but simplicity is the focus.
   TYPE access_task_control_block IS ACCESS task_control_block;
   TYPE task_control_blocks IS ARRAY(task_limit) OF
      access_task_control_block;

   -- The main array of tasks. I avoided a linked list because it's harder
   -- to prove and relies on pointers more. It's definitely possible to do
   -- with SPARK's current tools, but I don't believe it is worth it.
   Tasks       : task_control_blocks
   WITH
      Part_Of => Tasking_State;

   -- Indicates the task which is being given CPU time.
   Active_Task : task_limit := task_limit'first
   WITH
      Part_Of => Tasking_State;

   -- For now, the base virtual address for new tasks and their first thread
   -- starts at 0x1000 to avoid clashing with the null page. We'll use a small
   -- code model instead of a medium or large one. GCC should select it
   -- automatically. All ELF files should recognise this as the base virtual
   -- address.
   Virtual_Entry             : CONSTANT Memory.canonical_address := 16#1000#;

   -- This is just 32 KiB for the TSS's ring 0 RSP. I don't see a need in
   -- making it a dynamic size yet. Perhaps I could use stacks from the IST.
   -- Be sure to keep checking the "*.su" files generated by GCC in the build
   -- directory, as I have encountered stack overflows before that halt the
   -- entire kernel/system.
   Default_Kernel_Stack_Size : CONSTANT := 32 * KiB;

   -- Indicates whether or not tasking is enabled. This is mainly for usage
   -- within the scheduler and the appropriate page layout switchers.
   Enabled                   : ALIASED boolean := false
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "global__tasking_enabled",
      Linker_Section => ".isolated_bss";

   -- Tracks the current time slice for the active task.
   Countdown                 : ALIASED number := 0
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "global__tasking_countdown",
      Linker_Section => ".isolated_bss";

   -- Prepares an entry for the indicated task. By default the task is not
   -- alive by default for the purposes of modifying its memory before it
   -- activates. Passing a specified stack is optional for user convenience.
   PRAGMA Warnings(GNATprove, off,
      "unused initial value of ""Instruction_Address""",
      Reason => "It's put directly into memory and used as the starting RIP.");
   PRAGMA Warnings(GNATprove, off,
      "unused initial value of ""Stack_Address""",
      Reason => "It's put directly into memory and used as the starting RSP.");
   PROCEDURE Prepare_Entry
     (Task_Index          : IN task_limit;
      Instruction_Address : IN Memory.canonical_address;
      Error_Status        : OUT error;
      Stack_Address       : IN address := address'last)
   WITH
      Global => (In_Out => (Tasks, Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State),
                 Input  => (UEFI.Memory_Map, SPARK.Heap.Dynamic_Memory)),
      Pre    => Tasks(Task_Index) /= NULL,
      Post   => Tasks(Task_Index) /= NULL AND THEN
                Error_Status IN no_error | attempt_error | memory_error;

   -- Begins the first step of the tasking switch. This is where tasks go to
   -- stand-by while other tasks take off from here after being active. This
   -- effectively calls `INT 49`, which modifies the CR3 register, so this is
   -- placed in the isolated text section to maintain mappings between the
   -- kernel and all tasks.
   PROCEDURE Round_Robin
   WITH
      Global         => (In_Out   => (Tasks, Active_Task, Countdown,
                                      Descriptors.TSS),
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

   -- This cleans up dead tasks and dead task threads.
   PROCEDURE Task_Cleaner
   WITH
      Global => (In_Out => (Tasks, SPARK.Heap.Dynamic_Memory,
                            Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State),
                 Input  => Active_Task);

   -- This stores the active task's stack and moves onto the next task.
   -- It changes exactly what the `Get_*()` functions below return.
   PROCEDURE Round_Robin_Cycle
   WITH
      Global        => (In_Out   => (Active_Task, Descriptors.TSS),
                        Input    => Tasks,
                        Output   => Countdown,
                        Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__round_robin_cycle",
      Pre           => Enabled AND THEN
                       Tasks(Active_Task) /= NULL,
      Post          => Enabled                    AND THEN
                       Tasks(Active_Task) /= NULL AND THEN
                       Tasks(Active_Task).Alive;

   -- Returns a thin pointer to the active task's state, which is a record for
   -- callee-saved registers. Note that the task does not have to be alive.
   FUNCTION Get_Active_Task_State
      RETURN Memory.canonical_address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_active_task_state",
      Pre           => Enabled AND THEN
                       Tasks(Active_Task) /= NULL;

   -- Returns the base address of the task's level 4 page map (PML4) to put
   -- into the CR3 register. This is much easier to calculate in Ada.
   FUNCTION Get_Active_Task_CR3
      RETURN Memory.page_address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_active_task_cr3",
      Pre           => Enabled AND THEN
                       Tasks(Active_Task) /= NULL;

END HAVK_Kernel.Tasking;
