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
   -- TODO: Does not handle user-mode tasks.
   -- TODO: Does not go very fast or very safely.
   PROCEDURE Create
     (Initial_Name  : IN string;
      Initial_Entry : IN address)
   WITH
      Refined_Global => (In_Out => Tasks,
                         Input  => (Memory.Kernel_Heap_Base,
                                    Memory.Kernel_Heap_End))
   IS
      Padded_Name   :  string(1 .. 64) := (OTHERS => character'val(0));

      -- This effectively puts default register values onto the stack and
      -- makes the stack go through an interrupt handler simulation so it
      -- can be switched properly.
      FUNCTION Prepare_Task
        (CS  : IN number;
         DS  : IN number;
         RIP : IN address;
         RSP : IN address)
         RETURN address
      WITH
         Global        => NULL,
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__prepare_task";
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
               Name          => Padded_Name,
               Stack         => Memory.Allocate_System_Stack(8 * KiB),
               Virtual_Space => (OTHERS => (OTHERS => (Available_1 => 0,
                                 Available_2 => 0, Zeroed => 0, Pointer => 0,
                                 OTHERS => false))),
               Dead          => false,
               Max_Ticks     => 50 -- TODO: Need a better scheduler...
            );

            -- Quickly patch the new stack and make it a ring-0 task.
            Tasked.Stack :=
               Prepare_Task(16#08#, 16#10#, Initial_Entry, Tasked.Stack);

            EXIT WHEN true;
         END IF;
      END LOOP;

      Intrinsics.Enable_Interrupts;
   END Create;

   PROCEDURE Scheduler
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task, Enabled),
                         Input  =>  Interrupts.Ticker)
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
         ELSIF -- A true state-of-the-art scheduler.
            Interrupts.Ticker MOD Tasks(Active_Task).Max_Ticks = 0
         THEN
            Switch;
         END IF;
      END IF;
   END Scheduler;

   PROCEDURE Store_Task
     (Task_Stack : IN address)
   WITH
      Refined_Global => (In_Out => (Tasks, Active_Task)),
      Refined_Post   => Active_Task = Active_Task'old + 1 OR ELSE
                        Active_Task = Tasks'first
   IS
   BEGIN
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
         Active_Task + 1 IN Tasks'range AND THEN
         Tasks(Active_Task + 1) /= NULL
      THEN
         Active_Task := Active_Task + 1;
      ELSE
         Active_Task := Tasks'first;
      END IF;
   END Store_Task;

   FUNCTION Get_Task
      RETURN address
   IS
     (Tasks(Active_Task).Stack)
   WITH
      Refined_Global => (Input => (Tasks, Active_Task));

END HAVK_Kernel.Tasking;
