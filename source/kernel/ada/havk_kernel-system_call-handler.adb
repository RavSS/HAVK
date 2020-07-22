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
   BEGIN
      PRAGMA Warnings(GNATprove, off,
         "attribute Valid is assumed to return True",
         Reason => "Need the validity check. Make sure invalids are handled.");
      IF
         NOT Values.RDI'valid
      THEN
         Log("Task """ & Tasking.Get_Active_Task_Name &
            """ called a non-existent system operation.",
            Tag => System_Call_Tag, Warn => true);
         RETURN;
      END IF;

      CASE
         Values.RDI
      IS
         WHEN null_operation =>
            Null_Operation_Call(Values.RSI, Values.RDX, Values.R8, Values.R9,
               Values.R10, register(Values.RCX_RIP), Values.RAX);

         WHEN exit_thread_operation =>
            Exit_Thread_Operation_Call(Values.RSI);

         WHEN create_thread_operation =>
            Create_Thread_Operation_Call(Values.RSI, Values.RDX, Values.RAX);

         WHEN framebuffer_access_operation =>
            Framebuffer_Access_Operation_Call(Values.RSI, Values.RDX,
               Values.R8, Values.R9, Values.R10, Values.RAX);

         WHEN OTHERS => -- TODO: Expand these as we implement the user space.
            Log("Call """ & Values.RDI'image &
               """ is not yet implemented.", Tag => System_Call_Tag,
               Warn => true);
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
