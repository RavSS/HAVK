PACKAGE BODY HAVK_Kernel.Input IS
   FUNCTION Key_String_Format(
      Unformatted : IN string)
   RETURN key_string IS
      Formatted   : key_string;
   BEGIN
      FOR I IN Unformatted'range LOOP
         Formatted(I) := Unformatted(I);
      END LOOP;

      FOR I IN Unformatted'last + 1 .. Formatted'last LOOP
         Formatted(I) := character'val(0);
      END LOOP;

      RETURN Formatted;
   END Key_String_Format;

   FUNCTION Get_Key_State
   RETURN key_state IS
   BEGIN
      RETURN Last_Key_State;
   END Get_Key_State;

   FUNCTION Get_Key
   RETURN character IS
      Current_Key_State : CONSTANT key_state := Get_Key_State;
   BEGIN
      IF Current_Key_State.Key_Shifted THEN
         RETURN Current_Key_State.Key_ASCII_Shifted;
      ELSE
         RETURN Current_Key_State.Key_ASCII;
      END IF;
   END Get_Key;

   FUNCTION Get_Key_Name
   RETURN string IS
      Current_Key_State : CONSTANT key_state := Get_Key_State;
   BEGIN
      IF Current_Key_State.Key_Shifted THEN
         RETURN Current_Key_State.Key_Name_Shifted;
      ELSE
         RETURN Current_Key_State.Key_Name;
      END IF;
   END Get_Key_Name;

   PROCEDURE Set_Key_State(
      Name          : key_string;
      Name_Shifted  : key_string;
      ASCII         : character;
      ASCII_Shifted : character;
      Printable     : boolean;
      Break         : boolean;
      Shifted       : boolean)
   IS
   BEGIN
      Last_Key_State.Key_Name          := Name;
      Last_Key_State.Key_Name_Shifted  := Name_Shifted;
      Last_Key_State.Key_ASCII         := ASCII;
      Last_Key_State.Key_ASCII_Shifted := ASCII_Shifted;
      Last_Key_State.Key_Printable     := Printable;
      Last_Key_State.Key_Break         := Break;
      Last_Key_State.Key_Shifted       := Shifted;
   END Set_Key_State;
END HAVK_Kernel.Input;
