-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-apic-timer.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

-- This package is for controlling the local APIC timer.
PACKAGE HAVK_Kernel.APIC.Timer
IS
   -- Simply sets up the LAPIC timer with a given divisor.
   PROCEDURE Setup;

   -- The amount of interrupts generated by the timer. This is an alternate
   -- self-managed way of checking the timer's count.
   Ticks : number := 0;

PRIVATE
   APIC_Timer_Tag : CONSTANT string := "APICTIME";

   -- The LAPIC timer (when it's in the CPU as opposed to being discrete)
   -- has a frequency based on the attached CPU core's frequency. That
   -- frequency is then divided by one of the below values. Note that the
   -- value must go into the lower 4 bits of the LAPIC's divisor configuration
   -- register. Bit 2 (from zero) is always zeroed out.
   TYPE timer_divisor IS
     (divisor_2,
      divisor_4,
      divisor_8,
      divisor_16,
      divisor_32,
      divisor_64,
      divisor_128,
      divisor_1)
   WITH
      Object_Size => number'size;
   FOR timer_divisor USE
     (divisor_2   => 2#0000#,
      divisor_4   => 2#0001#,
      divisor_8   => 2#0010#,
      divisor_16  => 2#0011#,
      divisor_32  => 2#1000#,
      divisor_64  => 2#1001#,
      divisor_128 => 2#1010#,
      divisor_1   => 2#1011#);

   -- Indicates the various modes the LAPIC timer could be in.
   TYPE timer_mode IS
    ( -- One-shot mode is when the timer counts down from a value and when it
      -- hits zero, it delivers the interrupt. The value is not automatically
      -- reset; thus, it's a "one shot" timer value.
      one_shot_mode,
      -- Periodic mode is when the timer simply does the same as the one-shot
      -- mode, but the timer also automatically restarts periodically.
      periodic_mode,
      -- The TSC deadline mode is the most precise and relies on a deadline
      -- value, for which when it's passed by the CPU's timestamp value.
      -- It requires explicit CPU support, which can be checked with `CPUID`.
      TSC_deadline_mode)
   WITH
      Size => 2;
   FOR timer_mode USE
     (one_shot_mode     => 0,
      periodic_mode     => 1,
      TSC_deadline_mode => 2);

   -- The LVT register format for the timer.
   TYPE timer_register_format IS RECORD
      -- The IRQ for the timer interrupts, similar to how the PIT used IRQ 0.
      Interrupt_Vector   : number RANGE 00 .. 16#0000000000FF#;
      Reserved_1         : number RANGE 00 .. 16#00000000000F#;
      -- When true, the interrupt is pending delivery. When false, it is idle.
      -- This is a read only bit, it does nothing when written.
      Delivery_Status    : boolean;
      Reserved_2         : number RANGE 00 .. 16#000000000007#;
      -- When true, the timer interrupt is disabled.
      Interrupt_Masked   : boolean;
      -- Indicates the LAPIC timer's mode. See the type's comment for detail.
      Current_Timer_Mode : timer_mode;
      Reserved_3         : number RANGE 00 .. 16#1FFFFFFFFFFF#;
   END RECORD
   WITH
      Object_Size => number'size;
   FOR timer_register_format USE RECORD
      Interrupt_Vector       AT 0 RANGE 00 .. 07;
      Reserved_1             AT 0 RANGE 08 .. 11;
      Delivery_Status        AT 0 RANGE 12 .. 12;
      Reserved_2             AT 0 RANGE 13 .. 15;
      Interrupt_Masked       AT 0 RANGE 16 .. 16;
      Current_Timer_Mode     AT 0 RANGE 17 .. 18;
      Reserved_3             AT 0 RANGE 19 .. 63;
   END RECORD;

END HAVK_Kernel.APIC.Timer;
