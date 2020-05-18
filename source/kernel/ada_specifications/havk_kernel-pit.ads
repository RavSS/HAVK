-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-pit.ads                                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- A package that controls the Programmable Interval Timer (PIT) for when you
-- need a timer that isn't accurate or precise.
-- READ: https://wiki.osdev.org/PIT
-- TODO: This lacks detailed documentation and features. For now, I'm only
-- using the PIT to calibrate the LAPIC timer and for sleeping.
PACKAGE HAVK_Kernel.PIT
WITH
   Preelaborate => true
IS
   -- Does the most basic set up for the PIT.
   PROCEDURE Setup;

   -- Does nothing for a specific amount of milliseconds. This is not safe
   -- between tasks and it's also not intended for accuracy or precision.
   PROCEDURE Sleep
     (Millisecond_Wait : IN number)
   WITH
      Inline => true;

   -- Goes inside the IRQ 0 interrupt handler entry. Must not be called
   -- outside it.
   PROCEDURE Interrupt_Manager
   WITH
      Inline => true;

   -- The amount of times IRQ 0 has been raised by the PIT.
   Ticks     : number := 0;

   -- The default tick rate of the PIT in hertz. For now, this is a constant.
   Tick_Rate : CONSTANT number := 500;

PRIVATE
   PIT_Tag   : CONSTANT string := "PIT";

   -- The PIT's oscillator emits a third of the NTSC colour-burst frequency,
   -- so the below calculation just indicates that to be explicit about it as
   -- both frequencies are repeating. This value is in hertz, not megahertz.
   Frequency : CONSTANT number := number(315.0 / 88.0 / 3.0 * 1000000.0);

   -- We'll run the PIT at a phase of the tick rate by default. Also is a
   -- constant until we have reason to change it during runtime. The divisor
   -- can only ever be a 16-bit unsigned value. Zero is actually valid.
   Divisor   : CONSTANT number RANGE 0 .. 2**16 - 1 := Frequency / Tick_Rate;

   -- A semi-temporary variable for counting down. Note that the PIT has
   -- actual countdown hardware functionality, but constantly reconfiguring it
   -- is most certainly slower due to I/O port usage.
   Countdown : number := 0;

   -- The PIT is interacted with via the x86 I/O ports.
   TYPE port IS
     (channel_0_port,        -- Raises IRQ 0 depending on an specific interval.
      channel_1_port,        -- Unimplemented on even remotely modern hardware.
      channel_2_port,        -- The PC speaker channel.
      command_register_port) -- Configures the PIT itself. Not readable.
   WITH
      Size => 16;
   FOR port USE
     (channel_0_port        => 16#40#,
      channel_1_port        => 16#41#,
      channel_2_port        => 16#42#,
      command_register_port => 16#43#);

   -- An enumeration type for indicating which channel to send the command to.
   TYPE channel IS
     (channel_0,
      channel_1,
      channel_2,
      channel_read_back);
   FOR channel USE
     (channel_0         => 2#00#,
      channel_1         => 2#01#,
      channel_2         => 2#10#,
      channel_read_back => 2#11#);

   -- The various modes for the PIT's channels. These have numerous amounts of
   -- special conditions and some cannot be used with all channels.
   TYPE timer_mode IS
     (periodic_mode,
      one_shot_mode,
      rate_generator_mode,
      square_wave_generator_mode,
      software_triggered_strobe_mode,
      hardware_triggered_strobe_mode);
   FOR timer_mode USE
     (periodic_mode                  => 2#000#,
      one_shot_mode                  => 2#001#,
      rate_generator_mode            => 2#010#,
      square_wave_generator_mode     => 2#011#,
      software_triggered_strobe_mode => 2#100#,
      hardware_triggered_strobe_mode => 2#101#);

   -- Since the PIT's ports can only take in a byte at a time, this indicates
   -- various ways to send the data. Usually, you would want to send both
   -- the low byte and the high byte as a pair.
   TYPE byte_access_mode IS
     (latch_count_access,
      low_byte_access,
      high_byte_access,
      dual_byte_access);
   FOR byte_access_mode USE
     (latch_count_access => 2#00#,
      low_byte_access    => 2#01#,
      high_byte_access   => 2#10#,
      dual_byte_access   => 2#11#);

   -- This special command is sent when you want to selectively "read back" the
   -- various channels by inputting a byte from their respective port.
   TYPE read_back_command IS RECORD
      Zeroed              : boolean;
      Channel_0_Read_Back : boolean;
      Channel_1_Read_Back : boolean;
      Channel_2_Read_Back : boolean;
      No_Latch_Status     : boolean;
      No_Latch_Count      : boolean;
      -- The below bits must be set whenever this command is sent.
      Reserved_1          : boolean RANGE true .. true;
      Reserved_2          : boolean RANGE true .. true;
   END RECORD;
   FOR read_back_command USE RECORD
      Zeroed                   AT 0 RANGE 0 .. 0;
      Channel_0_Read_Back      AT 0 RANGE 1 .. 1;
      Channel_1_Read_Back      AT 0 RANGE 2 .. 2;
      Channel_2_Read_Back      AT 0 RANGE 3 .. 3;
      No_Latch_Status          AT 0 RANGE 4 .. 4;
      No_Latch_Count           AT 0 RANGE 5 .. 5;
      Reserved_1               AT 0 RANGE 6 .. 6;
      Reserved_2               AT 0 RANGE 7 .. 7;
   END RECORD;

   -- WARNING: It seems like you can't define an unchecked union subtype and
   -- then use it in a generic instantiation without it causing a compilation
   -- crash when you call the new subprogram with a variable passed to it using
   -- the new subtype. GCC version 9.2.0 with the GNAT CE 2019 front end:
   -- "output_constructor_regular_field, at varasm.c:5207"
   -- For that reason, I've had to remove the unchecked union and repeat some
   -- code below for two records that are basically identical.

   -- The format when sending a byte to the command register port. It is used
   -- for configuring a channel.
   TYPE command IS RECORD
      BCD_Mode            : boolean;
      Selected_Timer_Mode : timer_mode;
      Selected_Byte_Mode  : byte_access_mode;
      Selected_Channel    : channel;
   END RECORD;
   FOR command USE RECORD
      BCD_Mode            AT 0 RANGE 0 .. 0;
      Selected_Timer_Mode AT 0 RANGE 1 .. 3;
      Selected_Byte_Mode  AT 0 RANGE 4 .. 5;
      Selected_Channel    AT 0 RANGE 6 .. 7;
   END RECORD;

   -- When read-back for a channel is enabled, then reading its port will
   -- return this record in a byte.
   TYPE read_back_status IS RECORD
      BCD_Mode            : boolean;
      Selected_Timer_Mode : timer_mode;
      Selected_Byte_Mode  : byte_access_mode;
      Count_Is_Zero       : boolean;
      Set_Output_Pin      : boolean;
   END RECORD;
   FOR read_back_status USE RECORD
      BCD_Mode             AT 0 RANGE 0 .. 0;
      Selected_Timer_Mode  AT 0 RANGE 1 .. 3;
      Selected_Byte_Mode   AT 0 RANGE 4 .. 5;
      Count_Is_Zero        AT 0 RANGE 6 .. 6;
      Set_Output_Pin       AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Conveniently converts whatever type into a byte and outputs it to a port.
   PRAGMA Warnings(GNATprove, off, "unused variable ""Data""*",
      Reason => "The format is only converted/imported into a byte.");
   GENERIC
      TYPE generic_data IS PRIVATE;
      Channel_Port : IN port;
   PROCEDURE Send
     (Data         : IN generic_data)
   WITH
      Inline => true;

END HAVK_Kernel.PIT;
