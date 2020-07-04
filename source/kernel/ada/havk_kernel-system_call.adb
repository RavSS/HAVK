-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory,
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.System_Call
IS
   PROCEDURE Null_Operation_Call
     (RSI : IN number;
      RDX : IN number;
      R8  : IN number;
      R9  : IN number;
      RIP : IN address;
      RAX : OUT error)
   IS
      Active_Thread_Index : CONSTANT number := Tasking.Get_Active_Thread_Index;
   BEGIN
      Log("Task """ & Tasking.Get_Active_Task_Name &
         """ (thread " & Image(Active_Thread_Index) &
         ") called the null operation. " &
         "Argument 1: 0x" & Image(RSI, Base => 16) & " - " &
         "Argument 2: 0x" & Image(RDX, Base => 16) & " - " &
         "Argument 3: 0x" & Image(R8,  Base => 16) & " - " &
         "Argument 4: 0x" & Image(R9,  Base => 16) & " - " &
         "Call address: 0x" & Image(RIP) & '.',
         Tag => System_Call_Tag);
      RAX := no_error;
   END Null_Operation_Call;

   PROCEDURE Create_Thread_Operation_Call
     (RSI : IN number;
      RDX : IN number;
      RAX : OUT error)
   IS
      Active_Task : CONSTANT number := Tasking.Get_Active_Task_Index;
      Error_Check : error;
   BEGIN
      Log("Creating new thread for task " & Image(Active_Task) & '.',
         Tag => System_Call_Tag);

      IF
         RSI <= number(positive'last) AND THEN
         RDX <= number(positive'last)
      THEN
         Tasking.Create_Thread(Active_Task, Memory.canonical_address(RSI),
            Error_Check, Thread_Stack => Memory.canonical_address(RDX),
            Living => true);
         RAX := Error_Check;
      ELSE
         RAX := memory_error;
      END IF;
   END Create_Thread_Operation_Call;

END HAVK_Kernel.System_Call;
