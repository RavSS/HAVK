-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-pit.adb                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PIT
IS
   PROCEDURE Send
     (Data : IN generic_data)
   IS
      Value : CONSTANT byte
      WITH
         Import  => true,
         Size    => 8,
         Address => Data'address;
   BEGIN
      -- The assumption holds true because the largest value is under 2^16 - 1.
      -- TODO: Remove this with you-know-what when we shift to Ada 202X.
      PRAGMA Assume(Enum_Rep(Channel_Port) < 2**16);
      Intrinsics.Output_Byte(Enum_Rep(Channel_Port), Value);
   END Send;

   PROCEDURE Setup
   IS
      PROCEDURE Send_Divisor_Byte IS NEW Send
        (generic_data => byte, Channel_Port => channel_0_port);

      PROCEDURE Send_Command IS NEW Send
        (generic_data => command, Channel_Port => command_register_port);

      Timer_Setting : CONSTANT command :=
      (
         BCD_Mode            => false,
         Selected_Timer_Mode => rate_generator_mode,
         Selected_Byte_Mode  => dual_byte_access,
         Selected_Channel    => channel_0
      );
   BEGIN
      Log("Setting up the PIT.");
      Intrinsics.Disable_Interrupts;

      Send_Command(Timer_Setting);
      Send_Divisor_Byte(Divisor AND byte'last);
      Send_Divisor_Byte(Shift_Right(Divisor, 8) AND byte'last);

      Intrinsics.Enable_Interrupts;
      Log("The PIT has been configured.", nominal);
   END Setup;

   PROCEDURE Sleep
     (Millisecond_Wait : IN number)
   IS
   BEGIN
      -- TODO: Make this safe for multiple tasks.

      Intrinsics.Disable_Interrupts;

      -- Convert milliseconds into ticks depending on the tick rate.
      Countdown := Millisecond_Wait / (1000 / Tick_Rate);
      Intrinsics.Memory_Fence; -- Finish writing the countdown value first.

      Intrinsics.Enable_Interrupts;

      WHILE
         Countdown /= 0
      LOOP
         Intrinsics.Spinlock_Pause;
      END LOOP;
   END Sleep;

   PROCEDURE Interrupt_Handler
   IS
   BEGIN
      Ticks := Ticks + 1;

      IF
         Countdown /= 0
      THEN
         Countdown := Countdown - 1;
      END IF;
   END Interrupt_Handler;

END HAVK_Kernel.PIT;
