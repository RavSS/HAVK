-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-ps2-mouse.ads                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- TODO: Add mouse support. As of now, it just clears out the buffer if
-- the mouse IRQ is fired.
PACKAGE HAVK_Kernel.PS2.Mouse
IS
   PRAGMA Preelaborate;

   PROCEDURE Interrupt_Manager
   WITH
      Inline => true;
END HAVK_Kernel.PS2.Mouse;
