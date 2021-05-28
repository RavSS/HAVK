-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-pit.adb                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PIT
IS
   PROCEDURE Send
     (Data : IN generic_data)
   IS
      FUNCTION To_Byte IS NEW Ada.Unchecked_Conversion
        (source => generic_data, target => byte);
      -- PRAGMA Annotate(GNATprove, False_Positive,
      --    "type with constraints on bit representation *",
      --    "The format is manually checked (see the representation for it).");
   BEGIN
      Intrinsics.Output_Byte(Channel_Port'enum_rep, number(To_Byte(Data)));
   END Send;

   PROCEDURE Setup
   IS
      PROCEDURE Send_Divisor_Byte IS NEW Send
        (generic_data => byte, Channel_Port => channel_0_port);

      PROCEDURE Send_Command IS NEW Send
        (generic_data => command, Channel_Port => command_register_port);

      Timer_Setting : CONSTANT command :=
        (BCD_Mode            => false,
         Selected_Timer_Mode => rate_generator_mode,
         Selected_Byte_Mode  => dual_byte_access,
         Selected_Channel    => channel_0);
   BEGIN
      Log("Setting up the PIT.", Tag => PIT_Tag);
      Intrinsics.Disable_Interrupts;

      Send_Command(Timer_Setting);
      Send_Divisor_Byte(byte(Divisor AND 16#FF#));
      Send_Divisor_Byte(byte(Shift_Right(Divisor, 8) AND 16#FF#));

      Intrinsics.Enable_Interrupts;
      Log("The PIT has been configured.", Tag => PIT_Tag);
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

   PROCEDURE Interrupt_Manager
   IS
   BEGIN
      IF
         Ticks /= number'last
      THEN
         Ticks := Ticks + 1;
      ELSE
         Ticks := 0;
      END IF;

      IF
         Countdown /= 0
      THEN
         Countdown := Countdown - 1;
      END IF;
   END Interrupt_Manager;

END HAVK_Kernel.PIT;
