-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2-mouse.adb                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.PS2.Mouse
IS
   PROCEDURE Interrupt_Manager
   IS
   BEGIN
      Input_Controller.Flush;
   END Interrupt_Manager;
END HAVK_Kernel.PS2.Mouse;
