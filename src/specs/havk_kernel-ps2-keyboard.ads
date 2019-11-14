-- This package revolves around the PS/2 keyboard. Anything specific to it
-- instead of generic PS/2 logic should be here.
PACKAGE HAVK_Kernel.PS2.Keyboard
IS
   -- Handles raised IRQs. Should be placed directly into the ISR handler.
   PROCEDURE Interrupt_Manager
   WITH
      Inline_Always => true;

   -- Jump table that sets the current input record accordingly.
   PROCEDURE Scancode_Set_2(
      Scancode     : IN num;
      Shifted      : IN boolean;
      Break        : IN boolean)
   WITH
      Inline_Always => true;

END HAVK_Kernel.PS2.Keyboard;
