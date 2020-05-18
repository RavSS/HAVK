-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
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
      Byte_Value : CONSTANT number RANGE 0 .. 2**08 - 1
      WITH
         Import  => true,
         Size    => 8,
         Address => Data'address;

      Port_Word  : number RANGE 0 .. 2**16 - 1
      WITH
         Import  => true,
         Size    => 16,
         Address => Channel_Port'address;
   BEGIN
      Intrinsics.Output_Byte(Port_Word, Byte_Value);
   END Send;

   PROCEDURE Setup
   IS
      PROCEDURE Send_Divisor_Byte IS NEW Send
        (generic_data => number, Channel_Port => channel_0_port);

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
      Send_Divisor_Byte(Divisor AND 2**8 - 1);
      Send_Divisor_Byte(Shift_Right(Divisor, 8) AND 2**8 - 1);

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
      Ticks := Ticks + 1;

      IF
         Countdown /= 0
      THEN
         Countdown := Countdown - 1;
      END IF;
   END Interrupt_Manager;

END HAVK_Kernel.PIT;
