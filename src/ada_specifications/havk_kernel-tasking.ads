-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-tasking.ads                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Interrupts.APIC,
   HAVK_Kernel.Interrupts.APIC.Timer,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Paging;
USE
   HAVK_Kernel.Interrupts;

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
-- easy and we only have the BSP available to us until the IOAPICs are used.

-- This package details multi-tasking logic. I know that Ada has tasking
-- functionality, but for now, it will probably be much simpler to implement it
-- without any language-centric features which need to be baked into the RTS.
PACKAGE HAVK_Kernel.Tasking
WITH
   Abstract_State => Tasking_State
IS
   -- Creates a new task. Interrupts are disabled during its call until return.
   PROCEDURE Create
     (Initial_Name  : IN string;
      Initial_Entry : IN address;
      Stack_Size    : IN number  := 8 * KiB;
      User_Task     : IN boolean := false)
   WITH
      Global => (In_Out => (Tasking_State, Interrupts.TSS),
                 Input  => (Memory.Kernel_Heap_Base, Memory.Kernel_Heap_End)),
      Pre    => Initial_Name'first >= 01     AND THEN
                Initial_Name'last  <= 64     AND THEN
                Stack_Size IN 16 .. 64 * KiB AND THEN -- A stack size limit.
                Stack_Size MOD 16 = 0;

   -- Determines whether or not to switch tasks. Can also act as a task yield.
   -- For now, this is just a bad round-robin implementation.
   PROCEDURE Schedule
   WITH
      Global => (In_Out => Tasking_State, Input => APIC.Timer.Ticks);

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
      SS  : number RANGE 0 .. 16#FFFF#; -- All the other segments are the same.
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
      Name              : string(1 .. 64);
      State             : callee_saved_registers;
      Stack             : address;
      Stack_Size        : number;
      Kernel_Stack      : address;
      Kernel_Stack_Size : number;
      Virtual_Space     : Paging.access_page_layout;
      Dead              : boolean;
      User_Mode         : boolean;
      Max_Ticks         : number RANGE 1 .. number'last;
   END RECORD;

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

   -- Indicates whether or not tasking is enabled. This is mainly for usage
   -- within the scheduler.
   Enabled : boolean := false
   WITH
      Part_Of       => Tasking_State,
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__tasking_enabled";

   -- Begins the first step of the tasking switch.
   PROCEDURE Round_Robin
     (Yield : IN boolean := false)
   WITH
      Global => (In_Out => (Tasks, Active_Task, Enabled),
                 Input  =>  APIC.Timer.Ticks),
      Pre    => Enabled AND THEN Tasks(Active_Task) /= NULL;

   -- Raises `INT 100`, which is the interrupt vector for the context switch
   -- ISR. Do not redefine the external name to call the routine directly.
   PROCEDURE Switch
     (Active_State : IN callee_saved_registers)
   WITH
      Global        => (In_Out => Tasking_State),
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__raise_interrupt_100";

   -- This stores the active task's stack and moves onto the next task.
   -- It changes exactly what the `Get_*` functions below return.
   PROCEDURE Store_Task
     (Task_Stack : IN address)
   WITH
      Global        => (In_Out => (Tasks, Active_Task, Interrupts.TSS)),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__store_task",
      Pre           => Tasks(Active_Task) /= NULL,
      Post          => Tasks(Active_Task) /= NULL;

   -- Simply returns `Tasks(Active_Task).Stack`. This is here so the
   -- calculation does not have to be done in assembly and can be done much
   -- more safely at the cost of needing a temporary task management stack.
   FUNCTION Get_Task_Stack
      RETURN address
   WITH
      Global        => (Input => (Tasks, Active_Task)),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task_stack",
      Pre           => Tasks(Active_Task) /= NULL;

   -- Returns a thin pointer to the active task's state, which is a record
   -- for callee-saved registers.
   FUNCTION Get_Task_State
      RETURN address
   WITH
      Global        => (Input => (Tasks, Active_Task,
                        Memory.Kernel_Virtual_Base)),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task_state",
      Pre           => Tasks(Active_Task) /= NULL,
      Post          => Get_Task_State'result >=
                       Address_Value(Memory.Kernel_Virtual_Base);

END HAVK_Kernel.Tasking;
