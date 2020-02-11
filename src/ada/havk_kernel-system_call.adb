-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-system_call.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.System_Call
IS
   PROCEDURE Set_MSRs
   IS
      -- Set SCE to true and leave everything else, as it's usually good to go.
      EFER : CONSTANT number := Intrinsics.Read_MSR(IA32_EFER);

      -- See the comments/ramblings attached to the declaration of "IA32_STAR"
      -- for an understanding of what goes here.
      Segments    : CONSTANT number := Shift_Left(16#18#, 48) OR
         Shift_Left(16#08#, 32);

      -- The mask for RFLAGS. Right now, interrupts are disabled during the
      -- handling of the system call, as only a single stack exists for it.
      -- It can be resolved much later when performance can be improved.
      -- Reminder that a set bit here means that bit is zeroed upon entry.
      -- READ: https://en.wikipedia.org/wiki/FLAGS_register
      RFLAGS_Mask : CONSTANT number := 2#1_0_00_1_1_1_1_1_0_1_1_1_0_0_1#;
   BEGIN
      Intrinsics.Write_MSR(IA32_EFER, EFER OR 1); -- OR'd here due to SPARK.
      Intrinsics.Write_MSR(IA32_STAR, Segments);
      Intrinsics.Write_MSR(IA32_LSTAR, number(System_Call_Entry));
      Intrinsics.Write_MSR(IA32_FMASK, RFLAGS_Mask);
      Log("System calls ready.");
   END Set_MSRs;

   PRAGMA Warnings(off, "formal parameter * is not referenced",
      Reason => "Temporary warning silence until this is further expanded");
   PROCEDURE System_Call_Handler
     (Operation    : IN system_operation;
      Argument_1   : IN number;
      Argument_2   : IN number;
      Call_Address : IN address;
      Argument_3   : IN number;
      Argument_4   : IN number)
   IS
   BEGIN
      PRAGMA Warnings(GNATprove, off,
         "attribute Valid is assumed to return True",
         Reason => "Need the validity check. Make sure invalids are handled.");
      IF
         NOT Operation'valid
      THEN
         Log("A non-existent system operation was called.", warning);
         RETURN;
      END IF;

      CASE
         Operation
      IS
         WHEN test_operation =>
            Log("A user task called the test operation.");
            Log("Argument 1:" & number'image(Argument_1));
            Log("Argument 2:" & number'image(Argument_2));
            Log("Argument 3:" & number'image(Argument_3));
            Log("Argument 4:" & number'image(Argument_4));
      END CASE;
   END System_Call_Handler;
END HAVK_Kernel.System_Call;
