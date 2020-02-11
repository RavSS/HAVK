-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-user_input.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.User_Input
IS
   FUNCTION Key_String_Format
     (Unformatted : IN string)
      RETURN key_string
   IS
      Formatted   : key_string := (OTHERS => character'val(0));
   BEGIN
      IF
         Unformatted'first >= Formatted'first AND THEN
         Unformatted'last  <= Formatted'last
      THEN
         FOR
            I IN Unformatted'range
         LOOP
            Formatted(I) := Unformatted(I);
         END LOOP;
      END IF;

      RETURN Formatted;
   END Key_String_Format;

   FUNCTION Get_Key
      RETURN character
   IS
      Current_Key_State : key_state;
   BEGIN
      Intrinsics.Disable_Interrupts;
      Current_Key_State := Last_Key_State;
      Intrinsics.Enable_Interrupts;

      IF
         Current_Key_State.Shifted
      THEN
         RETURN Current_Key_State.ASCII_Shifted;
      ELSE
         RETURN Current_Key_State.ASCII;
      END IF;
   END Get_Key;

   FUNCTION Get_Key_Name
      RETURN string
   IS
      Current_Key_State : key_state;
   BEGIN
      Intrinsics.Disable_Interrupts;
      Current_Key_State := Last_Key_State;
      Intrinsics.Enable_Interrupts;

      IF
         Current_Key_State.Shifted
      THEN
         RETURN Current_Key_State.Name_Shifted;
      ELSE
         RETURN Current_Key_State.Name;
      END IF;
   END Get_Key_Name;

   FUNCTION Key_Is_Visible
      RETURN boolean
   IS
     (Last_Key_State.Printable);

   PROCEDURE Invalidate_Key_State
   IS
      Null_Key_State : CONSTANT key_state :=
      (
         Name          => (OTHERS => character'val(0)),
         Name_Shifted  => (OTHERS => character'val(0)),
         ASCII         => character'val(0),
         ASCII_Shifted => character'val(0),
         Printable     => false,
         Break         => false,
         Shifted       => false
      );
   BEGIN
      Last_Key_State := Null_Key_State;
   END Invalidate_Key_State;

END HAVK_Kernel.User_Input;
