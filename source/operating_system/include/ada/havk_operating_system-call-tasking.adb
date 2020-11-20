-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call-tasking.adb                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System.Call.Tasking
IS
   PROCEDURE Task_Finder
     (Task_Name : IN string;
      Status    : OUT task_status)
   IS
      Identity_Data   : arguments := (identify_task_operation, OTHERS => <>);
      Identity_String : XMM_string := (OTHERS => NUL);
   BEGIN
      FOR
         Identity_Index IN number RANGE 1 .. 255
      LOOP
         Identity_Data.Argument_1 := general_register(Identity_Index);

         IF -- Check its name and if it's a living task.
            System_Call(Identity_Data, Identity_String) = no_error AND THEN
            To_Status(Identity_String).Data.Name(Task_Name'range) = Task_Name
         THEN
            Status := To_Status(Identity_String).Data;
            RETURN;
         END IF;
      END LOOP;

      Status := (Index => 0, OTHERS => <>);
   END Task_Finder;

   PROCEDURE Exit_Task
     (Return_Code : IN number)
   IS
      Exit_Data : arguments := (exit_task_operation,
         Argument_1 => general_register(Return_Code), OTHERS => <>);
   BEGIN
      System_Call(Exit_Data);

      -- TODO: Remove this when the context is switchable during a system call.
      LOOP -- Wait for our time slice to expire.
         NULL;
      END LOOP;
   END Exit_Task;

END HAVK_Operating_System.Call.Tasking;