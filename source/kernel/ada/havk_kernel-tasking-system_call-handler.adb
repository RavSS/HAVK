-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-system_call-handler.adb            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Tasking.System_Call.Handler
IS
   PROCEDURE System_Call_Handler
   IS
      Caller_Index    : CONSTANT task_limit := Active_Task;
      -- TODO: This register state copy is needed due to SPARK aliasing rules.
      -- Perhaps it could be removed in the future if I rework the base tasking
      -- package itself.
      Arguments_State : register_state := Tasks(Caller_Index).Ring_3_State;
   BEGIN
      IF
         NOT Tasks(Active_Task).Alive
      THEN
         Log("Task """ & Tasks(Active_Task).Name &
            """ attempted to do a system call after death.",
            Tag => System_Call_Tag, Warn => true);
         RETURN;
      END IF;

      CASE
         operation(Tasks(Active_Task).Ring_3_State.RAX)
      IS
         WHEN Null_Operation               => Null_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.RDX,
            Arguments_State.R8,
            Arguments_State.R9,
            Arguments_State.RIP,
            Arguments_State.RAX);

         WHEN Exit_Task_Operation          => Exit_Task_Operation_Call
           (Arguments_State.RDI);

         WHEN Receive_Message_Operation    => Receive_Message_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.RDX,
            Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN Send_Message_Operation       => Send_Message_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.RDX,
            Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN Identify_Task_Operation      => Identify_Task_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN Load_ELF_Operation           => Load_ELF_Operation_Call
           (Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN Heap_Increase_Operation      => Heap_Increase_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RAX);

         WHEN Yield_Operation              => Yield_Operation_Call
           (Arguments_State.RAX);

         WHEN Log_Operation                => Log_Operation_Call
           (Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN IRQ_Statistics_Operation     => IRQ_Statistics_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.RAX);

         WHEN Buffer_Operation             => Buffer_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.XMM,
            Arguments_State.RAX);

         WHEN Framebuffer_Access_Operation => Framebuffer_Access_Operation_Call
           (Arguments_State.RDI,
            Arguments_State.RSI,
            Arguments_State.RDX,
            Arguments_State.R8,
            Arguments_State.R9,
            Arguments_State.RAX);

         WHEN OTHERS =>
            Log("Task """ & Tasks(Active_Task).Name &
               """ called a non-existent or unimplemented system operation.",
               Tag => System_Call_Tag, Warn => true);
            Arguments_State.RAX := index_error'enum_rep;
      END CASE;

      Tasks(Caller_Index).Ring_3_State := Arguments_State;

      IF -- TODO: This should not be possible, but `gnatprove` needs it.
         NOT Tasks(Active_Task).Present OR ELSE
         NOT Tasks(Active_Task).Alive
      THEN -- Need to rework the contracts so this assuredly cannot occur.
         RAISE Panic
         WITH
            Source_Location & " - Switched to an unusable task.";
         PRAGMA Annotate(GNATprove, Intentional,
            "exception might be raised",
            "Can't return to a task if it's not there or if it's dead.");
      END IF;

      Tasks(Caller_Index).System_Call_Interrupted := false;
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

END HAVK_Kernel.Tasking.System_Call.Handler;
