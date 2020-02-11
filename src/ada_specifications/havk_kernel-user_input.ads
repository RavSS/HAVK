-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-user_input.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package controls the state of input, regardless of origin.
-- TODO: Make input buffered by using a stack. Unbuffered input is not as good.
PACKAGE HAVK_Kernel.User_Input
IS
   PRAGMA Preelaborate;

   -- The name (string) of the keys can only be 32 characters long. This is
   -- a poor man's unbounded string replacement, but it works.
   SUBTYPE key_string IS string(1 .. 32);

   -- A simple key-press packet. The default values for the key name are all
   -- spaces, as using the "OTHER" keyword (loop) requires elaboration code.
   TYPE key_state IS RECORD
      -- The name of a key like "SPACEBAR".
      Name          : key_string := "                                ";
      -- Same as the above, but for the shifted version, like "BREAK".
      Name_Shifted  : key_string := "                                ";
      -- The ASCII format of a key.
      ASCII         : character  := character'val(0);
      -- The shifted version of the key.
      ASCII_Shifted : character  := character'val(0);
      -- Whether the key is actually printable, unlike e.g. "LEFT SHIFT".
      Printable     : boolean    := false;
      -- If the key was a break instead of a make.
      Break         : boolean    := false;
      -- Whether the key was pressed in a caps-lock state.
      Shifted       : boolean    := false;
   END RECORD;

   Last_Key_State   : key_state;

   -- Pads out the fixed string.
   FUNCTION Key_String_Format
     (Unformatted   : IN string)
      RETURN key_string
   WITH
      Inline => true,
      Pre    => Unformatted'length <= key_string'length;

   -- Shortcut function to retrieve the ASCII value of the key pressed.
   FUNCTION Get_Key
      RETURN character
   WITH
      Inline => true;

   -- Shortcut function to retrieve the name as a string of the key pressed.
   -- Remember that the string returned is padded to 32 characters, so an
   -- array slice is required if you wish to test for a specific key.
   FUNCTION Get_Key_Name
      RETURN string
   WITH
      Inline => true,
      Post   => Get_Key_Name'result'first = key_string'first AND THEN
                Get_Key_Name'result'last  = key_string'last;

   -- Returns true if the last key pressed is printable/visible.
   FUNCTION Key_Is_Visible
      RETURN boolean
   WITH
      Inline => true;

   -- This resets the key state to an invalid one. Useful for making the user
   -- redo a specific key press.
   PROCEDURE Invalidate_Key_State;

END HAVK_Kernel.User_Input;
