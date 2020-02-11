-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-ps2-keyboard.ads                           --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.User_Input;
USE
   HAVK_Kernel.User_Input;

-- This package revolves around the PS/2 keyboard. Anything specific to it
-- instead of generic PS/2 logic should be here.
PACKAGE HAVK_Kernel.PS2.Keyboard
IS
   PRAGMA Preelaborate;

   -- Becomes true when either shift key is held down.
   Shift_State     : boolean := false;

   -- Becomes true when caps lock has been pressed. Inverts on repeat.
   Caps_Lock_State : boolean := false;

   -- Becomes true when any key is released, after which it becomes false.
   Break_State     : boolean := false;

   -- Handles the PS/2 keyboard logic for all scancode sets. This should be
   -- called from the ISR handler so it occurs on an IRQ.
   PROCEDURE Interrupt_Manager;

PRIVATE
   -- Jump table that sets the current input record accordingly.
   FUNCTION Scancode_Set_2
     (Scancode : IN number)
      RETURN key_state
   WITH
      Inline => true,
      Pre    => Scancode <= 16#FF#;

END HAVK_Kernel.PS2.Keyboard;
