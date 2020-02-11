-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-tasking.adb                                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Tasking
WITH
   Refined_State => (Tasking_State => (Tasks, Active_Task, Enabled))
IS
   -- TODO: Does not replace dead/finished tasks or catch them.
   -- TODO: Does not go very fast or very safely.
   -- TODO: It can create a ring 3 task, but it needs proper memory management.
   PROCEDURE Create
     (Initial_Name  : IN string;
      Initial_Entry : IN address;
      Stack_Size    : IN number  := 8 * KiB;
      User_Task     : IN boolean := false)
   WITH
      Refined_Global => (In_Out => (Tasks, Descriptors.TSS))
   IS
      -- This effectively makes the stack go through an interrupt handler
      -- simulation so it can be jumped to and switched properly.
      FUNCTION Prepare_Task_Stack
        (CS  : IN number;
         DS  : IN number;
         RIP : IN address;
         RSP : IN address)
         RETURN address
      WITH
         Global        => NULL,
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__prepare_task_stack";

      Padded_Name : string(1 .. 64) := (OTHERS => character'val(0));

      -- See the base interrupts specification on how these are determined.
      CS_Ring_0 : CONSTANT number := 16#08#;
      DS_Ring_0 : CONSTANT number := 16#10#;
      CS_Ring_3 : CONSTANT number := 16#28# OR 3;
      DS_Ring_3 : CONSTANT number := 16#20# OR 3;

      -- This is just 8-KiB for the TSS's ring 0 RSP. I don't see a need in
      -- making it a dynamic size.
      Default_Kernel_Stack_Size : CONSTANT number := 8 * KiB;
   BEGIN
      Intrinsics.Disable_Interrupts;

      Padded_Name(Initial_Name'range) := Initial_Name;

      FOR
         Tasked OF Tasks
      LOOP
         IF
            Tasked = NULL
         THEN
            Tasked := NEW task_control_block'
            (
               Name              => Padded_Name,
               State             => (OTHERS => 0),
               Stack             => Memory.Allocate_System_Stack(Stack_Size),
               Stack_Size        => Stack_Size,
               Kernel_Stack      => 0,
               Kernel_Stack_Size => 0,
               Virtual_Space     => NULL,
               Dead              => false,
               User_Mode         => User_Task,
               Max_Ticks         => 50 -- TODO: Need a better scheduler...
            );

            IF -- Adjust the new stack(s) depending on the DPL.
               User_Task
            THEN
               Tasked.Stack := Prepare_Task_Stack
                 (CS_Ring_3, DS_Ring_3, Initial_Entry, Tasked.Stack);
               Tasked.State.SS := DS_Ring_3;

               -- Only ring 3 tasks need a stack for when the kernel
               -- interrupts it to do whatever work.
               Tasked.Kernel_Stack := Memory.Allocate_System_Stack
                  (Default_Kernel_Stack_Size);
               Tasked.Kernel_Stack_Size := Default_Kernel_Stack_Size;
            ELSE
               Tasked.Stack := Prepare_Task_Stack
                 (CS_Ring_0, DS_Ring_0, Initial_Entry, Tasked.Stack);
               Tasked.State.SS := DS_Ring_0;
            END IF;

            -- TODO: Move this elsewhere when performance can matter.
            IF -- If this is the first task, then set the TSS's ring 0 RSP.
               Descriptors.TSS.RSP_Ring_0  = 0
            THEN
               Descriptors.TSS.RSP_Ring_0 := Tasked.Kernel_Stack;
            END IF;

            EXIT WHEN true;
         END IF;
      END LOOP;

      Intrinsics.Enable_Interrupts;
   END Create;

   PROCEDURE Schedule
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task, Enabled),
                         Input  =>  APIC.Timer.Ticks)
   IS
   BEGIN
      IF
         Enabled
      THEN
         IF
            Tasks(Active_Task) = NULL
         THEN
            RAISE Panic
            WITH
               "The task scheduler has lost track of the active task.";
            PRAGMA Annotate(GNATprove, False_Positive,
               "exception might be raised",
               "Cannot happen without external corruption.");
         ELSE
            Round_Robin;
         END IF;
      END IF;
   END Schedule;

   PROCEDURE Round_Robin
     (Yield : IN boolean := false)
   IS
   BEGIN
      IF -- A true state-of-the-art scheduler.
         Yield OR ELSE APIC.Timer.Ticks MOD Tasks(Active_Task).Max_Ticks = 0
      THEN
         Switch(Tasks(Active_Task).State);
      END IF;
   END Round_Robin;

   PROCEDURE Store_Task
     (Task_Stack : IN address)
   IS
   BEGIN
      IF
         Active_Task + 1 IN Tasks'range AND THEN
         Tasks(Active_Task + 1) /= NULL
      THEN
         Active_Task := Active_Task + 1;
      ELSE
         Active_Task := Tasks'first;
      END IF;

      IF
         Tasks(Active_Task) = NULL
      THEN
         RAISE Panic
         WITH
            "The task scheduler has lost track of the active task.";
         PRAGMA Annotate(GNATprove, False_Positive,
            "exception might be raised",
            "Cannot happen without external corruption.");
      END IF;

      Tasks(Active_Task).Stack := Task_Stack;

      IF
         Tasks(Active_Task).User_Mode
      THEN
         Descriptors.TSS.RSP_Ring_0 := Tasks(Active_Task).Kernel_Stack;
      ELSE
         Descriptors.TSS.RSP_Ring_0 := 0; -- Not really necessary.
      END IF;
   END Store_Task;

   FUNCTION Get_Task_Stack
      RETURN address
   IS
     (Tasks(Active_Task).Stack);

   FUNCTION Get_Task_State
      RETURN address
   IS
     (Tasks(Active_Task).State'address)
   WITH
      SPARK_Mode => off; --- Address attribute used.

END HAVK_Kernel.Tasking;
