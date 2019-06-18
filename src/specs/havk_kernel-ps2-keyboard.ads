PACKAGE HAVK_Kernel.PS2.Keyboard IS
   Key : character
   WITH
      Volatile => true;

   TYPE scancode_sets IS(
      set_1,  -- IBM PC XT.
      set_2,  -- IBM PC AT (default).
      set_3); -- IBM 3270 PC.
   FOR  scancode_sets USE(
      set_1 => 1,
      set_2 => 2,
      set_3 => 3);

   Current_Scancode_Set : num RANGE 1 .. 3 := 2;
   Current_Shift_State  : boolean := false;

   PROCEDURE Interrupt_Manager
   WITH
      Inline => true;

   PROCEDURE Set_2_ASCII_Table(
      PS2_Code_Sum : IN num);
END HAVK_Kernel.PS2.Keyboard;
