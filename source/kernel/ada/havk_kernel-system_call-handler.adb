-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call-handler.adb                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Tasking;

PACKAGE BODY HAVK_Kernel.System_Call.Handler
IS
   PROCEDURE System_Call_Handler
     (Values : NOT NULL ACCESS arguments)
   IS
      Caller_Index  : CONSTANT number := Tasking.Get_Active_Task_Index;
      Caller_Status : CONSTANT Tasking.task_status :=
         Tasking.Get_Task_Status(Caller_Index);
   BEGIN
      PRAGMA Warnings(GNATprove, off,
         "attribute Valid is assumed to return True",
         Reason => "Need the validity check. Make sure invalids are handled.");
      IF
         NOT Caller_Status.Alive
      THEN
         Log("Task """ & Caller_Status.Name &
            """ attempted to do a system call after death.",
            Tag => System_Call_Tag, Warn => true);
         RETURN;
      ELSIF
         NOT Values.Operation_Call'valid
      THEN
         Log("Task """ & Caller_Status.Name &
            """ called a non-existent system operation.",
            Tag => System_Call_Tag, Warn => true);
         RETURN;
      END IF;

      CASE
         Values.Operation_Call
      IS
         WHEN null_operation               => Null_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.Argument_3,
            Values.Argument_4,
            Values.Argument_5,
            Values.Call_Address,
            Values.Error_Status);

         WHEN exit_task_operation          => Exit_Task_Operation_Call
           (Values.Argument_1);

         WHEN receive_message_operation    => Receive_Message_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.Argument_3,
            Values.XMM_State,
            Values.Error_Status);

         WHEN send_message_operation       => Send_Message_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.Argument_3,
            Values.XMM_State,
            Values.Error_Status);

         WHEN identify_task_operation      => Identify_Task_Operation_Call
           (Values.Argument_1,
            Values.XMM_State,
            Values.Error_Status);

         WHEN load_elf_operation           => Load_ELF_Operation_Call
           (Values.XMM_State,
            Values.Error_Status);

         WHEN heap_increase_operation      => Heap_Increase_Operation_Call
           (Values.Argument_1,
            Values.Error_Status);

         WHEN yield_operation              => Yield_Operation_Call
           (Values.Error_Status);

         WHEN log_operation                => Log_Operation_Call
           (Values.XMM_State,
            Values.Error_Status);

         WHEN irq_statistics_operation     => IRQ_Statistics_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.Error_Status);

         WHEN buffer_operation             => Buffer_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.XMM_State,
            Values.Error_Status);

         WHEN framebuffer_access_operation => Framebuffer_Access_Operation_Call
           (Values.Argument_1,
            Values.Argument_2,
            Values.Argument_3,
            Values.Argument_4,
            Values.Argument_5,
            Values.Error_Status);
      END CASE;
   END System_Call_Handler;

   PROCEDURE Set_MSRs
   IS
      -- Set SCE to true and leave everything else, as it's usually good to go.
      EFER        : CONSTANT number := Intrinsics.Read_MSR(IA32_EFER);

      -- See the comments/ramblings attached to the declaration of "IA32_STAR"
      -- for an understanding of what goes here.
      Segments    : CONSTANT number := Shift_Left(16#18#, 48) OR
         Shift_Left(16#08#, 32);

      -- The mask for RFLAGS. Right now, interrupts are disabled during the
      -- handling of the system call to avoid some concurrency issues.
      -- It can be resolved much later when performance can be improved.
      -- Reminder that a set bit here means that bit is zeroed upon entry.
      -- READ: https://en.wikipedia.org/wiki/FLAGS_register
      RFLAGS_Mask : CONSTANT number := 2#1_0_00_1_1_1_1_1_0_1_1_1_0_0_1#;
   BEGIN
      Intrinsics.Write_MSR(IA32_EFER, EFER OR 1); -- OR'd here due to SPARK.
      Intrinsics.Write_MSR(IA32_STAR, Segments);
      Intrinsics.Write_MSR(IA32_LSTAR, number(System_Call_Entry));
      Intrinsics.Write_MSR(IA32_FMASK, RFLAGS_Mask);
      Log("System calls ready.", Tag => System_Call_Tag);
   END Set_MSRs;

END HAVK_Kernel.System_Call.Handler;
