-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2-mouse.ads                              --
-- License         -- GNU General Public License Version 3.0                 --
-- Original Author -- Ravjot Singh Samra (ravss@live.com), Copyright 2019    --
-------------------------------------------------------------------------------

-- TODO: Add mouse support. As of now, it just clears out the buffer if
-- the mouse IRQ is fired.
PACKAGE HAVK_Kernel.PS2.Mouse
IS
   PROCEDURE Interrupt_Manager
   WITH
      Inline_Always => true;
END HAVK_Kernel.PS2.Mouse;
