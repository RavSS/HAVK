-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-apic-timer.adb                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Interrupts.APIC.Timer
IS
   PROCEDURE Setup
   IS
      -- TODO: Replace this with its attribute when switching to Ada 2020.
      FUNCTION Enum_Rep IS NEW Ada.Unchecked_Conversion
        (Source => timer_divisor, Target => number);

      PROCEDURE Set_Timer_Divisor IS NEW Write_LAPIC
        (generic_format => number);

      PROCEDURE Set_Timer IS NEW Write_LAPIC
        (generic_format => timer_register_format);

      PROCEDURE Set_Timer_Count IS NEW Write_LAPIC
        (generic_format => number);

      -- TODO: This is not calibrated properly. We need `CPUID` to tell us the
      -- core crystal's clock frequency. Try to aim for 100 Hz.
      Default_Count         : CONSTANT number                := 3000000;
      Default_Divisor       : CONSTANT timer_divisor         := divisor_4;
      Default_Configuration : CONSTANT timer_register_format :=
      (
         Interrupt_Vector   => IRQ_Base + 16,
         Delivery_Status    => false,
         Interrupt_Masked   => false,
         Current_Timer_Mode => periodic_mode,
         OTHERS             => 0
      );
   BEGIN
      Set_Timer_Divisor(timer_divisor_register, Enum_Rep(Default_Divisor));
      Set_Timer(timer_register, Default_Configuration);
      Set_Timer_Count(timer_initial_count_register, Default_Count);
   END Setup;

END HAVK_Kernel.Interrupts.APIC.Timer;
