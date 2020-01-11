-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-tasking.ads                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Memory,
   HAVK_Kernel.Paging;

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
      Initial_Entry : IN address)
   WITH
      Global => (In_Out => Tasking_State,
                 Input  => (Memory.Kernel_Heap_Base, Memory.Kernel_Heap_End)),
      Pre    => Initial_Name'first >= 1 AND THEN Initial_Name'last <= 64;

   -- Determines whether or not to switch tasks. It can be used as an
   -- alternative to the `Switch()` procedure if a strict yield is not
   -- desired. For now, this is just a bad round-robin implementation.
   PROCEDURE Scheduler
   WITH
      Global => (In_Out => Tasking_State, Input => Interrupts.Ticker),
      Inline => true;

   -- Changes HAVK from a mono-tasking kernel to a multi-tasking kernel.
   -- Does not return. The first task is the first one entered.
   PROCEDURE Start
   WITH
      Global        => (In_Out => Tasking_State),
      No_Return     => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__start_tasking";

   -- Raises `INT 100`, which is the interrupt vector for the context switch
   -- ISR. For now, this acts as a yield procedure if called directly.
   -- If this returns (it will) and it reaches the end of a task's initial
   -- procedure or function, then what happens next is undefined.
   -- Use it in a loop or an intial subprogram that does not return and is
   -- proven to not return.
   PROCEDURE Switch
   WITH
      Global        => (In_Out => Tasking_State),
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__raise_interrupt_100";

PRIVATE
   -- Details a task's various states and settings.
   -- TODO: Only handles ring-0 tasks for now.
   TYPE task_control_block IS RECORD
      Name          : string(1 .. 64);
      Stack         : address;
      Virtual_Space : Paging.page_layout;
      Dead          : boolean;
      Max_Ticks     : number RANGE 1 .. number'last;
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

   -- This stores the active task's stack and then changes the active task
   -- itself. Intended to be called from the context switch routine.
   PROCEDURE Store_Task
     (Task_Stack : IN address)
   WITH
      Global        => (In_Out => Tasking_State),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__store_task";

   -- Simply returns `Tasks(Active_Task).Stack`. This is here so the
   -- calculation does not have to be done in assembly and can be done much
   -- more safely at the cost of needing a temporary task management stack.
   FUNCTION Get_Task
      RETURN address
   WITH
      Global        => (Input => Tasking_State),
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__get_task",
      Pre           => Tasks(Active_Task) /= NULL;

END HAVK_Kernel.Tasking;
