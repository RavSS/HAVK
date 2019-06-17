PACKAGE HAVK_Kernel.PS2.Keyboard IS
   Key : character
   WITH
      Volatile => true;

   Current_Scancode_Set : num RANGE 1 .. 3 := 1;

   TYPE scancode_sets  IS(
      set_1,  -- IBM PC XT.
      set_2,  -- IBM PC AT.
      set_3); -- IBM 3270 PC.
   FOR  scancode_sets  USE(
      set_1 => 1,
      set_2 => 2,
      set_3 => 3);

   -- TYPE scancode_types IS(
      -- make,                 -- 0x00.
      -- break,                -- 0xF0.
      -- set_1_shift_make,     -- 0xE0, 0xAA.
      -- set_1_shift_break,    -- 0xE0, 0x2A.
      -- set_1_numlock_make,   -- 0xE0, 0x2A.
      -- set_1_numlock_break,  -- 0xE0, 0xAA.
      -- set_2_shift_make,     -- 0xE0, 0xF0, 0x12. 
      -- set_2_shift_break,    -- 0xE0, 0x12.
      -- set_2_numlock_make,   -- 0xE0, 0x12.
      -- set_2_numlock_break); -- 0xE0, 0xF0, 0x12.
   -- FOR  scancode_types USE(
      -- make                => 0,
      -- break               => 1,
      -- set_1_shift_make    => 2,
      -- set_1_shift_break   => 3,
      -- set_1_numlock_make  => 4,
      -- set_1_numlock_break => 5,
      -- set_2_shift_make    => 6,
      -- set_2_shift_break   => 7,
      -- set_2_numlock_make  => 8,
      -- set_2_numlock_break => 9);

   PROCEDURE Set_2_ASCII_Table(
      Code_Sum : num);
END HAVK_Kernel.PS2.Keyboard;
