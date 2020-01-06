-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts-pic.adb                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Interrupts.PIC
IS
   -- For the remapping function, I essentially copied my old x86/32-bit
   -- attempt's remapping function and adapted it to here.
   PROCEDURE Remap
   IS
   BEGIN
      -- Initialize the PICs.
      Output_Byte(Master_Command, 16#11#);
      Output_Byte(Slave_Command,  16#11#);

      -- Start the IRQ vector where IRQ 0 begins at according to us.
      -- The reasoning for this is because IRQ 0 to IRQ 7 are clashed with
      -- the CPU exception interrupts (8 to 15).
      Output_Byte(Master_Data, 32);

      -- We don't need to remap the slave PICs interrupt vector, as it goes
      -- from 112 to 120. I'm going to do it anyway so all IRQs line up.
      Output_Byte(Slave_Data,  40);

      -- Need to cascade interrupts.
      --     0 - 1 - 2 - 3 - 4 - 5 - 6 - 7
      --         |
      -- 0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8
      -- IRQ 2 cascades to IRQ 1 (second PIC IRQ 1 becomes IRQ 8).
      -- New layout and priority:
      -- 0 > 1 > 8 > 9 > 10 > 11 > 12 > 13 > 14 > 15 > 3 > 4 > 5 > 6 > 7

      -- Inform master PIC that there's another (slave) PIC at IRQ 2.
      -- (1 << [IRQ] 2) = [Binary Form] 100 = [Hex/Dec Form] 4.
      Output_Byte(Master_Data, 4);

      -- Notify the slave PIC that interrupts will cascade
      -- from the master PIC's IRQ 2.
      -- (1 << [IRQ] 1) = [Binary Form] 10  = [Hex/Dec Form] 2.
      Output_Byte(Slave_Data,  2);

      -- Put PICs into 8086 mode.
      Output_Byte(Master_Data, 1);
      Output_Byte(Slave_Data,  1);

      -- Clear any interrupt masks.
      Output_Byte(Master_Data, 0);
      Output_Byte(Slave_Data,  0);
   END Remap;
END HAVK_Kernel.Interrupts.PIC;
