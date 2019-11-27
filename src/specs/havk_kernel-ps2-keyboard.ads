WITH
   HAVK_Kernel.User_Input;
USE
   HAVK_Kernel.User_Input;

-- This package revolves around the PS/2 keyboard. Anything specific to it
-- instead of generic PS/2 logic should be here.
PACKAGE HAVK_Kernel.PS2.Keyboard
IS
   -- Becomes true when either shift key is held down.
   Shift_State     : boolean := false;

   -- Becomes true when caps lock has been pressed. Inverts on repeat.
   Caps_Lock_State : boolean := false;

   -- Becomes true when any key is released, after which it becomes false.
   Break_State     : boolean := false;

   -- Handles the PS/2 keyboard logic for all scancode sets. This should be
   -- called from the ISR handler so it occurs on an IRQ.
   PROCEDURE Interrupt_Manager
   WITH
      Global => (In_Out => (Shift_State, Caps_Lock_State, Break_State,
                            Last_Key_State));

PRIVATE
   -- Jump table that sets the current input record accordingly.
   FUNCTION Scancode_Set_2(
      Scancode : IN num)
   RETURN key_state
   WITH
      Global => (Input  => (Shift_State, Caps_Lock_State, Break_State)),
      Inline => true,
      Pre    => Scancode <= 16#FF#;

END HAVK_Kernel.PS2.Keyboard;
