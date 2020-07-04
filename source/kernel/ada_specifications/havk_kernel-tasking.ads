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
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Paging,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Memory.Frames;

-- This package details multi-tasking logic. I know that Ada has tasking
-- functionality, but for now, it will probably be much simpler to implement it
-- without any language-centric features which need to be baked into the RTS.
-- All tasks created run in ring 3; nothing except the kernel is permitted to
-- run normally in ring 0. System calls will need to be used instead. Note that
-- I use the terms "task" and "process" synonymously for now, with "threads"
-- being the children of a "task" or "process".
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
                            UEFI.Bootloader_Arguments)),
      Pre    => Initial_Frames IN 1 .. (2 * GiB) / Paging.Page AND THEN
                Task_Name'first = 1                            AND THEN
                Task_Name'last IN Task_Name'first .. 64;

   -- Creates a thread for the indicated task. By default the thread is not
   -- alive by default for the purposes of a new task's first thread being
   -- created. Passing a specified stack is optional for convenience.
   PRAGMA Warnings(GNATprove, off, "unused initial value of ""Thread_*""",
      Reason => "These values are put directly into memory.");
   PROCEDURE Create_Thread
     (Task_Index   : IN number;
      Thread_Entry : IN Memory.canonical_address;
      Error_Status : OUT error;
      Thread_Stack : IN Memory.canonical_address := address'last;
      Living       : IN boolean                  := false)
   WITH
      Global => (In_Out => (Tasking_State, Memory.Frames.Frame_Allocator_State,
                            Paging.Kernel_Page_Layout_State),
                 Input  => (UEFI.Bootloader_Arguments,
                            SPARK.Heap.Dynamic_Memory));

   -- This removes the task at the specified index.
   PROCEDURE Remove
     (Task_Index   : IN number;
      Error_Status : OUT error)
   WITH
      Global => (In_Out => (Tasking_State, SPARK.Heap.Dynamic_Memory,
                            Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State)),
      Post   => Error_Status IN no_error | index_error | attempt_error;

   -- Removes a thread specific to a task. The thread in question does not have
   -- to be dead beforehand.
   PROCEDURE Remove_Thread
     (Task_Index   : IN number;
      Thread_Index : IN number;
      Error_Status : OUT error)
   WITH
      Global => (In_Out => (Tasking_State, Paging.Kernel_Page_Layout_State),
                 Input  => SPARK.Heap.Dynamic_Memory),
      Post   => Error_Status IN no_error | index_error | attempt_error;

   -- TODO: An early attempt at task cleanup. It also switches the task so that
   -- the active task (most notably its stack) is no longer in current use.
   PROCEDURE Kill_Active_Task;

   -- Handles page faults caused by tasks.
   PROCEDURE Page_Fault_Handler
     (Error_Code : IN number) -- The error code should be 32 bits.
   WITH
      Inline => true;

   -- Obtains the index of a task by name. Returns an attempt error if the task
   -- name did not match a task, with the task index then being out of range.
   PROCEDURE Get_Task_Index
     (Task_Name    : IN string;
      Task_Index   : OUT number;
      Error_Status : OUT error)
   WITH
      Global => (Input => Tasking_State),
      Pre    => Task_Name'first = 1 AND THEN
                Task_Name'last IN Task_Name'first .. 64,
      Post   => (IF Error_Status = attempt_error THEN
                    Task_Index = number'first);

   -- Just returns the active task's name.
   FUNCTION Get_Active_Task_Name
      RETURN string
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State),
      Post              => Get_Active_Task_Name'result'first = 1 AND THEN
                           Get_Active_Task_Name'result'last IN
                              Get_Active_Task_Name'result'first .. 64;

   FUNCTION Get_Active_Task_Index
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => Tasking_State);

   FUNCTION Get_Active_Thread_Index
      RETURN number
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

   -- A max of 256 for both tasks and task threads for now. This can be changed
   -- whenever later on, it's just a reasonable limit for now. Note that task
   -- zero is the kernel itself and is thus reserved.
   SUBTYPE   task_limit IS number RANGE 1 .. 256;
   SUBTYPE thread_limit IS number RANGE 1 .. 256;

   -- TODO: This is not complete with the SIMD registers etc., although I save
   -- them onto the task's ring 0 stack in the ISR stubs.
   TYPE callee_saved_registers IS RECORD
      RBX : number := 0;
      RBP : Memory.canonical_address := 0;
      -- This is the kernel stack in action. It's base address would be in the
      -- TSS descriptor.
      RSP : Memory.canonical_address := 0;
      R12 : number := 0;
      R13 : number := 0;
      R14 : number := 0;
      R15 : number := 0;
      -- All other segment indices are identical to the SS (except for the CS).
      -- This is not actually loaded into the SS register, but rather the other
      -- segment index registers. I am fairly certain the other registers aside
      -- from CS, GS, FS, and of course SS, are not really used.
      SS  : number RANGE 0 .. 2**16 - 1 := Descriptors.DS_Ring_3;
   END RECORD
   WITH
      Convention  => Assembler,
      Object_Size => (56 * 8) + 63 + 1;
   FOR callee_saved_registers USE RECORD
      RBX    AT 00 RANGE 0 .. 63;
      RBP    AT 08 RANGE 0 .. 63;
      RSP    AT 16 RANGE 0 .. 63;
      R12    AT 24 RANGE 0 .. 63;
      R13    AT 32 RANGE 0 .. 63;
      R14    AT 40 RANGE 0 .. 63;
      R15    AT 48 RANGE 0 .. 63;
      SS     AT 56 RANGE 0 .. 63; -- Zero-extended to 64 bits when `MOV`'d.
   END RECORD;

   -- This details a thread belonging to a task. I've gone with a 1:1 model,
   -- where HAVK is aware of all the threads a task is using. No user-level
   -- scheduling is required.
   -- TODO: Implement group scheduling.
   TYPE thread_control_block IS RECORD
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
      Kernel_Stack_Physical : Memory.canonical_address := 0;
      -- Indicates whether or not the thread is alive.
      Alive                 : boolean := false;
      -- When true, this thread should be treated like it was allocated, even
      -- if it's currently dead i.e. not alive.
      Allocated             : boolean := false;
      -- Right now, I am just using round robin without a meaningful priority
      -- algorithm, so this value is how many LAPIC timer interrupts the thread
      -- is supposed to receive.
      -- TODO: Adjust this dynamically.
      Max_Ticks             : number RANGE 1 .. number'last := 5;
   END RECORD
   WITH
      Dynamic_Predicate => Kernel_Stack_Size MOD Paging.Page = 0 AND THEN
                          (IF Alive THEN Allocated)              AND THEN
                          (IF NOT Allocated THEN NOT Alive);

   -- A flat array of records (not accesses) is needed, or else it violates
   -- SPARK mode.
   TYPE thread_control_blocks IS ARRAY(thread_limit) OF thread_control_block;

   -- Details a task's various states and settings. The index of the task
   -- itself is considered to be the task identity.
   TYPE task_control_block IS LIMITED RECORD
      -- The name of the task. Only the task has a name, not the threads.
      Name           : string(1 .. 64) := (OTHERS => character'val(0));
      -- Indicates the frame count of the task's initial code/data which is
      -- given to it in `Create()`.
      Initial_Frames : number := 0;
      -- This is the base address of the physical frames given to the task upon
      -- create. The size onwards is given by the "Code_Size" field above.
      Physical_Base  : Memory.page_address := 0;
      -- The paging layout for the task and how it sees virtual memory. I've
      -- only given a task a single page layout for now, each thread shares
      -- both virtual and physical memory with each other.
      Virtual_Space  : Paging.page_layout;
      -- An array of threads that belong to the current task. Handled by the
      -- kernel, but created by the task.
      Threads        : thread_control_blocks;
      -- This is the thread in use for the current task.
      Active_Thread  : thread_limit := thread_control_blocks'first;
      -- Indicates whether the task is alive or not. This has precedence over
      -- the thread's living status.
      Alive          : boolean := false;
      -- This is the number of frames the task has requested after creation.
      Heap_Frames    : number := 0;
   END RECORD
   WITH
      Dynamic_Predicate => (IF Alive THEN
                              Initial_Frames IN 1 .. (2 * GiB) / Paging.Page
                                 AND THEN
                              Name /= (1 .. 64 => character'val(0)));

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

   -- This is just 16 KiB for the TSS's ring 0 RSP. I don't see a need in
   -- making it a dynamic size yet. Perhaps I could use stacks from the IST.
   Default_Kernel_Stack_Size : CONSTANT := 16 * KiB;

   -- Indicates whether or not tasking is enabled. This is mainly for usage
   -- within the scheduler and the appropriate page layout switchers.
   Enabled                   : ALIASED boolean := false
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "global__tasking_enabled",
      Linker_Section => ".isolated_bss";

   -- Tracks the current time slice for the active task's active thread.
   Countdown                 : ALIASED number := 0
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "global__tasking_countdown",
      Linker_Section => ".isolated_bss";

   -- Begins the first step of the tasking switch. This is where tasks go to
   -- stand-by while other tasks take off from here after being active. This
   -- effectively calls `INT 49`, which modifies the CR3 register, so this is
   -- placed in the isolated text section to maintain mappings between the
   -- kernel and all tasks.
   PROCEDURE Round_Robin
     (Yield : IN boolean := false)
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
      Global        => (In_Out   => (Tasks, Active_Task, Descriptors.TSS,
                                     SPARK.Heap.Dynamic_Memory,
                                     Memory.Frames.Frame_Allocator_State,
                                     Paging.Kernel_Page_Layout_State),
                        Output   => Countdown,
                        Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__round_robin_cycle",
      Pre           => Enabled AND THEN
                       Tasks(Active_Task) /= NULL,
      Post          => Enabled                    AND THEN
                       Tasks(Active_Task) /= NULL AND THEN
                       Tasks(Active_Task).Alive   AND THEN
                       Tasks(Active_Task)
                         .Threads(Tasks(Active_Task).Active_Thread).Alive;

   -- Returns a thin pointer to the active task's active thread state, which is
   -- a record for callee-saved registers. Note that the task or the thread
   -- does not have to be alive or (in the case of the latter) allocated.
   FUNCTION Get_Active_Thread_State
      RETURN Memory.canonical_address
   WITH
      Global        => (Input => (Tasks, Active_Task), Proof_In => Enabled),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_active_thread_state",
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
      Pre           => Enabled                    AND THEN
                       Tasks(Active_Task) /= NULL AND THEN
                       Tasks(Active_Task).Alive   AND THEN
                       Tasks(Active_Task)
                         .Threads(Tasks(Active_Task).Active_Thread).Alive;

END HAVK_Kernel.Tasking;
