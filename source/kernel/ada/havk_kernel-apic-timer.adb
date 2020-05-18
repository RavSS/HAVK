-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-apic-timer.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
  Ada.Unchecked_Conversion,
  HAVK_Kernel.PIT;

PACKAGE BODY HAVK_Kernel.APIC.Timer
IS
   PROCEDURE Setup
   IS
      -- TODO: Replace this with its attribute when switching to Ada 2020.
      FUNCTION Enum_Rep IS NEW Ada.Unchecked_Conversion
        (Source => timer_divisor, Target => number);

      PROCEDURE Set_Timer_Divisor IS NEW Write_LAPIC
        (generic_format => number,
         MSR            => timer_divisor_register);

      PROCEDURE Set_Timer         IS NEW Write_LAPIC
        (generic_format => timer_register_format,
         MSR            => timer_register);

      PROCEDURE Set_Timer_Count   IS NEW Write_LAPIC
        (generic_format => number,
         MSR            => timer_initial_count_register);

      PROCEDURE Get_Timer_Count   IS NEW  Read_LAPIC
        (generic_format => number,
         MSR            => timer_current_count_register);

      -- There's two leaves we can use for this: the TSC/core crystal leaf
      -- or the CPU frequency leaf. It depends on the platform as to which one
      -- is available with enough information to calibrate the timer, but in
      -- the worst case, you can cross over information from them and calculate
      -- each other's missing information (or at least it seems like it). The
      -- lower-end processors may not be able to use the CPU frequency leaf,
      -- but the higher-end processors may have zeroed fields in the TSC/core
      -- crystal leaf. The link below shows Linux developers figuring it out.
      -- Maybe try using both to make sure the timer is always to our liking.
      -- READ: https://lore.kernel.org/patchwork/patch/1063023/
      -- TODO: The problem is that they're not always present, so for now, I
      -- will just calibrate the APIC timer with the PIT. Use them when they
      -- are present some time later on when accuracy and precision matter.

      Current_Count         : number;
      Default_Count         : CONSTANT number        := 16#FFFFFFFF#;
      Default_Divisor       : CONSTANT timer_divisor := divisor_64;
      Default_Configuration : timer_register_format  :=
        (Interrupt_Vector   => IRQ_Base + 16,
         Delivery_Status    => false,
         Interrupt_Masked   => false,
         Current_Timer_Mode => periodic_mode,
         OTHERS             => 0);
   BEGIN
      Log("Calibrating a LAPIC timer using the PIT.", Tag => APIC_Timer_Tag);

      -- WARNING: I'm currently using the method on OSDev shown below.
      -- READ: https://wiki.osdev.org/APIC_timer#Example_code_in_C
      -- This should really be done in assembly for a worthwhile calibration,
      -- since my PIT code itself is not exactly optimised enough for this.
      -- Calibration using the PIT would be the worst case scenario, I think.

      Set_Timer_Divisor(Enum_Rep(Default_Divisor));
      Set_Timer_Count(Default_Count);
      Set_Timer(Default_Configuration);

      PIT.Sleep(10);
      Default_Configuration.Interrupt_Masked := true;
      Set_Timer(Default_Configuration);

      Get_Timer_Count(Current_Count);
      Set_Timer_Count(Default_Count - Current_Count);

      Default_Configuration.Interrupt_Masked := false;
      Set_Timer(Default_Configuration);

      Log("A LAPIC timer has been calibrated.", Tag => APIC_Timer_Tag);
   END Setup;

END HAVK_Kernel.APIC.Timer;
