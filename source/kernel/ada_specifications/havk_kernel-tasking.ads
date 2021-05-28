-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking.ads                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
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
   Abstract_State => Tasking_State
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
      Padding : number RANGE 0 .. 2**7 - 1 := 0;
      -- The name the task was created with.
      Name    : task_name_string := (OTHERS => NUL);
   END RECORD
   WITH
      Dynamic_Predicate => (IF Index = 0 THEN
                               NOT Alive AND THEN
                               Name = task_name_string'(OTHERS => NUL)),
      Object_Size       => (9 * 8) + (task_name_string'length * 8);
   FOR task_status USE RECORD
      Index   AT 00 RANGE 0 .. 63;
      Alive   AT 08 RANGE 0 .. 00;
      Padding AT 08 RANGE 1 .. 07;
      Name    AT 09 RANGE 0 .. (task_name_string'length * 8) - 1;
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
      Pre    => Initial_Frames IN 1 .. (2 * GiB) / Paging.page_size'enum_rep
                   AND THEN
                Task_Name'length <= task_name_string'length;

   -- This removes the task at the specified index.
   PROCEDURE Remove
     (Task_Index   : IN number;
      Error_Status : OUT error)
   WITH
      Global => (In_Out => (Tasking_State, Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State)),
      Post   => Error_Status IN no_error | index_error | attempt_error;

   -- TODO: An early attempt at task cleanup. It marks the active task as dead.
   PROCEDURE Kill_Active_Task
     (Kill_Code : IN number);

   -- Clears out the active task's time slice and yields to another task. If a
   -- non-zero value is passed as the next task index, then that will be the
   -- next active task (if it exists).
   PROCEDURE Yield
     (Error_Status    : OUT error;
      Next_Task_Index : IN number := 0);

   -- Does the same as the various address mapping procedures in the paging
   -- package, but for a task instead. A small wrapper.
   PROCEDURE Map_Address_Range
     (Task_Index       : IN number;
      Virtual_Address  : IN address;
      Physical_Address : IN address;
      Size             : IN number;
      Page_Size_Type   : IN Paging.page_frame_variant := Paging.page_size;
      Present          : IN boolean := true;
      Write_Access     : IN boolean := false;
      User_Access      : IN boolean := false;
      No_Execution     : IN boolean := true)
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
      Global => (Input => Tasking_State);

   -- Returns the status information for a task.
   FUNCTION Get_Task_Status
     (Task_Index : IN number)
      RETURN task_status
   WITH
      Global => (Input => Tasking_State);

   -- Determines whether or not to switch tasks. For now, this is just a bad
   -- round-robin implementation.
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

   -- A record that stores all registers of a task. It should remain consistent
   -- with the struct specified in "tasking.s". Registers are put into the
   -- record according to descending alphabetical order. Note that before you
   -- change this, some assembly code relies on the CR3 value being at the
   -- start of the record (at the base address).
   TYPE register_state IS RECORD
      CR3    : ALIASED Memory.page_address := 0;
      CS     : ALIASED Intrinsics.general_register RANGE 0 .. 2**16 - 1 := 0;
      MMX    : ALIASED Intrinsics.MMX_registers;
      R8     : ALIASED Intrinsics.general_register := 0;
      R9     : ALIASED Intrinsics.general_register := 0;
      R10    : ALIASED Intrinsics.general_register := 0;
      R11    : ALIASED Intrinsics.general_register := 0;
      R12    : ALIASED Intrinsics.general_register := 0;
      R13    : ALIASED Intrinsics.general_register := 0;
      R14    : ALIASED Intrinsics.general_register := 0;
      R15    : ALIASED Intrinsics.general_register := 0;
      RAX    : ALIASED Intrinsics.general_register := 0;
      RBP    : ALIASED Intrinsics.general_register := 0;
      RBX    : ALIASED Intrinsics.general_register := 0;
      RCX    : ALIASED Intrinsics.general_register := 0;
      RDI    : ALIASED Intrinsics.general_register := 0;
      RDX    : ALIASED Intrinsics.general_register := 0;
      RFLAGS : ALIASED Intrinsics.general_register := 0;
      RIP    : ALIASED Memory.canonical_address := 0;
      RSI    : ALIASED Intrinsics.general_register := 0;
      RSP    : ALIASED Intrinsics.general_register := 0;
      -- All other segment indices are identical to the SS (except for the CS).
      -- The other registers aside from CS, GS, FS, and of course SS, are not
      -- really used. Zero-extended up to 64 bits when `MOV`'d, as these are
      -- actually special registers.
      SS     : ALIASED Intrinsics.general_register RANGE 0 .. 2**16 - 1 := 0;
      XMM    : ALIASED Intrinsics.XMM_registers;
   END RECORD
   WITH
      Convention  => Assembler,
      Object_Size => (232 * 8) + 2047 + 1;
   FOR register_state USE RECORD
      CR3    AT 000 RANGE 0 .. 0063;
      CS     AT 008 RANGE 0 .. 0063;
      MMX    AT 016 RANGE 0 .. 0511;
      R8     AT 080 RANGE 0 .. 0063;
      R9     AT 088 RANGE 0 .. 0063;
      R10    AT 096 RANGE 0 .. 0063;
      R11    AT 104 RANGE 0 .. 0063;
      R12    AT 112 RANGE 0 .. 0063;
      R13    AT 120 RANGE 0 .. 0063;
      R14    AT 128 RANGE 0 .. 0063;
      R15    AT 136 RANGE 0 .. 0063;
      RAX    AT 144 RANGE 0 .. 0063;
      RBP    AT 152 RANGE 0 .. 0063;
      RBX    AT 160 RANGE 0 .. 0063;
      RCX    AT 168 RANGE 0 .. 0063;
      RDI    AT 176 RANGE 0 .. 0063;
      RDX    AT 184 RANGE 0 .. 0063;
      RFLAGS AT 192 RANGE 0 .. 0063;
      RIP    AT 200 RANGE 0 .. 0063;
      RSI    AT 208 RANGE 0 .. 0063;
      RSP    AT 216 RANGE 0 .. 0063;
      SS     AT 224 RANGE 0 .. 0063;
      XMM    AT 232 RANGE 0 .. 2047; -- TODO: Put this on a 16-byte boundary.
   END RECORD;

   -- Details a task's various states and settings. The index of the task
   -- itself is considered to be the task identity.
   TYPE task_control_block IS LIMITED RECORD
      -- Whether the task control block is occupied. Should be set back to
      -- false after being true only when the rest of the block no longer holds
      -- any notable information and has been reset to its default values. If
      -- this is false, then it is safe to use the current task control block.
      Present                    : boolean := false;
      -- Indicates whether or not the task is alive.
      Alive                      : boolean := false;
      -- The name of the task.
      Name                       : task_name_string := (OTHERS => NUL);
      -- Indicates the frame count of the task's initial code/data which is
      -- given to it in `Create()`.
      Initial_Frames             : Memory.Frames.frame_limit := 0;
      -- This is the base address of the physical frames given to the task upon
      -- create. The size onwards is given by the "Initial_Frames" field above.
      -- The heap size is controlled by a child package and is not counted
      -- towards this value.
      Physical_Base              : Memory.page_address := 0;
      -- The paging layout for the task and how it sees virtual memory. I've
      -- only given a task a single page layout for now.
      Virtual_Space              : Paging.page_layout;
      -- Indicates the task state that is in use. When true, the ring 0 state
      -- is used; else, the ring 3 state is the state to save information to.
      System_Call_Interrupted    : boolean := false;
      -- The registers saved and reloaded during context switches. There are
      -- two states: one for system calls and one for user space. Upon a system
      -- call, the user's state is saved and the appropriate system calls work
      -- off the user state's registers etc. This lets system call states be
      -- context switchable.
      Ring_0_State               : ALIASED register_state;
      Ring_3_State               : ALIASED register_state;
      -- This is a stack to use during system calls. The address must be the
      -- bottom of the stack, meaning that it's the highest address and will
      -- grow downwards.
      System_Call_Stack_Virtual  : Memory.page_address := 0;
      -- TODO: For now, I identity-map the system call stack. I'll need to pick
      -- a sensible virtual range in which to map these. A bug can likely occur
      -- where the task's ELF code/data overlaps the system call stack's
      -- mapping. That would break some things.
      System_Call_Stack_Physical : Memory.page_address := 0;
      -- The byte size of the system call stack. This should be a multiple of 4
      -- KiB (for alignment).
      System_Call_Stack_Size     : number := 0;
      -- These fields are identical to the system call stack specific ones, but
      -- they are intended for usage within interrupt handlers.
      -- TODO: As of now, they are not necessary, as no specific information is
      -- being saved onto the interrupt stacks (so a stack per CPU could be
      -- used instead). I've kept it for future expansion. Similarly, the
      -- interrupt stack is also identity-mapped.
      Interrupt_Stack_Virtual    : Memory.page_address := 0;
      Interrupt_Stack_Physical   : Memory.page_address := 0;
      Interrupt_Stack_Size       : number := 0;
      -- TODO: This is just a random value passed by the task as of now.
      -- A value of anything other than zero indicates an error of some sort.
      Exit_Code                  : number := 0;
      -- Right now, I am just using round robin without a meaningful priority
      -- algorithm, so this value is how many LAPIC timer interrupts the task
      -- is supposed to receive.
      -- TODO: Adjust this dynamically.
      Max_Ticks                  : number RANGE 1 .. number'last := 10;
      -- This is the next page address of the heap (meaning that the last
      -- mapped address is 4 bits behind it). It is to be handled by a child
      -- package.
      Heap_End                   : Memory.page_address := 0;
   END RECORD
   WITH
      Dynamic_Predicate => (IF NOT Present THEN NOT Alive) AND THEN
                           (IF Alive THEN
                              Initial_Frames IN 1 .. (2 * GiB) /
                                 Paging.page_size'enum_rep AND THEN
                              Name /= (Name'range => NUL) AND THEN
                              Exit_Code = 0);

   -- The main array of tasks. I avoided a linked list because it's harder to
   -- prove and relies on pointers more. It's definitely possible to do with
   -- SPARK's current tools, but I don't believe it is worth it. Additionally,
   -- this is all pre-allocated.
   Tasks       : ARRAY(task_limit) OF task_control_block
   WITH
      Part_Of => Tasking_State;

   -- Indicates the task which is being given CPU time.
   Active_Task : task_limit := task_limit'first
   WITH
      Part_Of => Tasking_State;

   -- Early attempt at having some semblance of a priority queue, except it is
   -- only for a single upcoming task. Currently just used for in-kernel task
   -- switches to another task without some `setjmp()` type of functionality.
   -- When zero, there is no preferred next task and regular scheduling can
   -- occur.
   Next_Task   : number RANGE 0 .. task_limit'last := 0
   WITH
      Part_Of => Tasking_State;

   -- For now, the base virtual address for new tasks and their first thread
   -- starts at 0x1000 to avoid clashing with the null page. We'll use a small
   -- code model instead of a medium or large one. GCC should select it
   -- automatically. All ELF files should recognise this as the base virtual
   -- address.
   Virtual_Entry : CONSTANT Memory.canonical_address := 16#1000#;

   -- These are static sizes for now, as I don't see a need in making them
   -- dynamic sizes yet. Be sure to keep checking the "*.su" files generated by
   -- GCC in the build directory, as I have encountered stack overflows before
   -- that halt the entire kernel/system.
   Default_System_Call_Stack_Size : CONSTANT := 24 * KiB;
   Default_Interrupt_Stack_Size   : CONSTANT := 16 * KiB;

   -- Indicates whether or not tasking is enabled. This is mainly for usage
   -- within the scheduler and the appropriate page layout switchers.
   Enabled                   : ALIASED boolean := false
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "ada__tasking_enabled",
      Linker_Section => ".isolated_bss";

   -- Tracks the current time slice for the active task.
   Countdown                 : ALIASED number := 0
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "ada__tasking_countdown",
      Linker_Section => ".isolated_bss";

   -- Pointers to the active task's context/register states. This is a
   -- workaround for the assembly routine where context switching is performed
   -- so it does not have to make a function call and can instead just address
   -- the register state records directly; otherwise, the call would clobber
   -- the interrupted register state without doing a double backup of it.
   Active_Task_Ring_0_Context : ALIASED Memory.canonical_address := 0
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "ada__ring_0_tasking_context",
      Linker_Section => ".isolated_bss";

   Active_Task_Ring_3_Context : ALIASED Memory.canonical_address := 0
   WITH
      Part_Of        => Tasking_State,
      Export         => true,
      Convention     => Assembler,
      External_Name  => "ada__ring_3_tasking_context",
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
                 Input  => (UEFI.Memory_Map, UEFI.Bootloader_Arguments,
                            Memory.Kernel_Virtual_Base)),
      Pre    => Tasks(Task_Index).Present,
      Post   => Tasks(Task_Index).Present AND THEN
                Error_Status IN no_error | attempt_error | memory_error;

   -- This cleans up dead tasks and dead task threads.
   PROCEDURE Task_Cleaner
   WITH
      Global => (In_Out => (Tasks, Paging.Kernel_Page_Layout_State,
                            Memory.Frames.Frame_Allocator_State),
                 Input  => Active_Task);

   -- Changes the active task and the pointer to the active task context based
   -- on the round-robin scheduling algorithm.
   PROCEDURE Round_Robin
   WITH
      Global => (In_Out   => (Active_Task, Descriptors.TSS),
                 Input    => Tasks,
                 Output   => (Active_Task_Ring_0_Context,
                              Active_Task_Ring_3_Context, Countdown),
                 Proof_In => Enabled),
      Pre    => Enabled AND THEN
                Tasks(Active_Task).Present,
      Post   => Enabled                    AND THEN
                Tasks(Active_Task).Present AND THEN
                Tasks(Active_Task).Alive;

   -- Switches state information for the tasking mechanism itself to a specific
   -- task. Note that it does not save the current ring 0 state when called.
   -- You have to do that before calling this.
   PROCEDURE Switch_To_Task
     (Task_Index : IN task_limit)
   WITH
      Pre => Tasks(Task_Index).Present;

   -- Returns a thin pointer to the active task's state, which is a record for
   -- all registers. Note that the task does not have to be alive.
   FUNCTION Get_Task_State_Address
     (Task_Index : IN task_limit;
      Ring_3     : IN boolean := true)
      RETURN Memory.canonical_address
   WITH
      Global => (Input => Tasks),
      Pre    => Tasks(Task_Index).Present;

   -- Sets which register state is the active one for the active task.
   PROCEDURE Set_Active_Task_State_Mode
     (System_Call_Mode : IN boolean)
   WITH
      Global        => (In_Out => Tasks, Input => Active_Task),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__set_active_task_state_mode",
      Pre           => Tasks(Active_Task).Present;

   -- Simply retrieves one of the active task's state addresses depending on
   -- which one is used.
   FUNCTION Get_Active_Task_State
      RETURN Memory.canonical_address
   WITH
      Global        => (Input => (Tasks, Active_Task,
                                  Active_Task_Ring_0_Context,
                                  Active_Task_Ring_3_Context)),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_active_task_state",
      Pre           => Tasks(Active_Task).Present;

END HAVK_Kernel.Tasking;
