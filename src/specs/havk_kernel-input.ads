PACKAGE HAVK_Kernel.Input IS
   SUBTYPE key_string IS string(1 .. 32);

   TYPE key_state IS RECORD
      -- The name of a key like "PAUSE".
      Key_Name          : key_string;
      -- Same as the above, but for the shifted version, like "BREAK".
      Key_Name_Shifted  : key_string;
      -- The ASCII format of a key.
      Key_ASCII         : character;
      -- The shifted version of the key.
      Key_ASCII_Shifted : character;
      -- Whether the key is actually printable, unlike e.g. "LEFT SHIFT".
      Key_Printable     : boolean;
      -- If the key was a break instead of a make.
      Key_Break         : boolean;
      -- Whether the key was pressed in a caps-lock state.
      Key_Shifted       : boolean;
   END RECORD;

   -- Lock/blocking variable.
   Key_State_Requested  : boolean := false;

   -- Pads out the fixed string.
   FUNCTION Key_String_Format(
      Unformatted : IN string)
   RETURN key_string
   WITH
      Pre => Unformatted'length <= key_string'length;

   -- See the "Last_Key_State" variable for this function's purpose.
   FUNCTION Get_Key_State
   RETURN key_state;

   -- Purpose is the same as the above function's purpose.
   PROCEDURE Set_Key_State(
      Name          : key_string;
      Name_Shifted  : key_string;
      ASCII         : character;
      ASCII_Shifted : character;
      Printable     : boolean;
      Break         : boolean;
      Shifted       : boolean);

   -- Shortcut function.
   FUNCTION Get_Key
   RETURN character
   WITH
      Inline => true;

PRIVATE
   -- This is private so every single part of the record needs to be
   -- updated on every single key update.
   Last_Key_State : key_state :=
   (
      Key_Name          => (OTHERS => character'val(0)),
      Key_Name_Shifted  => (OTHERS => character'val(0)),
      Key_ASCII         => character'val(0),
      Key_ASCII_Shifted => character'val(0),
      Key_Printable     => false,
      Key_Break         => false,
      Key_Shifted       => false
   );
END HAVK_Kernel.Input;
