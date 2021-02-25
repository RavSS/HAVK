-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System PS/2 Driver                      --
-- Filename        -- havk_ps2-keyboard.adb                                  --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.Utility;
USE
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.Utility;

PACKAGE BODY HAVK_PS2.Keyboard
IS
   -- TODO: This was prepared in mind for an interrupt handler, not a polling
   -- style of handling input.
   PROCEDURE Interrupt_Manager
   IS
      -- Remember that this procedure handles a single byte. A raised IRQ
      -- does not indicate that HAVK must read a sequence of bytes, but instead
      -- only a single byte. For example, multiple IRQs will be raised to
      -- handle key breaks, but only a single one will be raised for a press.
      Scancode : CONSTANT number RANGE 0 .. 2**8 - 1 :=
         Input_Byte(data_port'enum_rep);
      Is_Ready : boolean;
   BEGIN
      LOOP -- Blocking.
         Ready(Is_Ready);
         EXIT WHEN Is_Ready;
      END LOOP;

      IF -- TODO: This only supports a PS/2 keyboard on port 1.
         Port_1_Device /= standard_keyboard OR ELSE
         Scancode = 16#FA# -- Data is not for the keyboard.
      THEN
         RETURN;
      END IF;

      CASE
         Current_Scancode_Set -- TODO Set 1 and 3 are unimplemented.
      IS
         WHEN set_1 =>
            Log("PS/2 scancode set 1 is not supported.", Warn => true);
         WHEN set_2 =>
            IF
               Scancode = 16#F0# -- Break logic.
            THEN
               Break_State := true;
               RETURN; -- Don't change the key state, return immediately.
            ELSIF
               Scancode = 16#12# OR ELSE Scancode = 16#59# -- Shift logic.
            THEN
               Shift_State := NOT Break_State;
            ELSIF
               Scancode = 16#58# -- Caps lock logic.
            THEN
               IF
                  NOT Break_State
               THEN
                  Caps_Lock_State := NOT Caps_Lock_State;
               END IF;
            ELSE -- Standard key press logic. Resets break state.
               Break_State := false; -- Don't waste time evaluating it.
            END IF;

            -- Change the key state.
            Last_Key_State := Scancode_Set_2(Scancode);
         WHEN set_3 =>
            Log("PS/2 scancode set 3 is not supported.", Warn => true);
      END CASE;
   END Interrupt_Manager;

   FUNCTION Scancode_Set_2 -- TODO: This is not fully complete, I believe.
     (Scancode       : IN number)
      RETURN key_state
   IS
      Key            : key_state;
   BEGIN
      Key.Break      := Break_State;
      Key.Printable  := true; -- True by default, saves having to repeat it.

      IF -- Handle caps-lock state in relation with the shift state.
         Caps_Lock_State
      THEN
         Key.Shifted := NOT Shift_State;
      ELSE
         Key.Shifted := Shift_State;
      END IF;

      CASE
         Scancode
      IS
         -- Control keys.
         WHEN 16#66# =>
            Key.Name          := Key_String_Format("BACKSPACE");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(08);
            Key.ASCII_Shifted := character'val(08);
            Key.Printable     := false;
         WHEN 16#0D# =>
            Key.Name          := Key_String_Format("TAB");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(09);
            Key.ASCII_Shifted := character'val(09);
            Key.Printable     := false;
         WHEN 16#5A# =>
            Key.Name          := Key_String_Format("ENTER");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(10);
            Key.ASCII_Shifted := character'val(10);
            Key.Printable     := false;
         WHEN 16#76# =>
            Key.Name          := Key_String_Format("ESCAPE");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(27);
            Key.ASCII_Shifted := character'val(27);
            Key.Printable     := false;
         WHEN 16#12# =>
            Key.Name          := Key_String_Format("LEFT SHIFT");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(00);
            Key.ASCII_Shifted := character'val(00);
            Key.Printable     := false;
         WHEN 16#59# =>
            Key.Name          := Key_String_Format("RIGHT SHIFT");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(00);
            Key.ASCII_Shifted := character'val(00);
            Key.Printable     := false;
         WHEN 16#58# =>
            Key.Name          := Key_String_Format("CAPS LOCK");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(00);
            Key.ASCII_Shifted := character'val(00);
            Key.Printable     := false;
         WHEN 16#29# =>
            Key.Name          := Key_String_Format("SPACEBAR");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(32);
            Key.ASCII_Shifted := character'val(32);

         -- Letter keys.
         WHEN 16#15# =>
            Key.Name          := Key_String_Format("LOWERCASE Q");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE Q");
            Key.ASCII         := 'q';
            Key.ASCII_Shifted := 'Q';
         WHEN 16#1D# =>
            Key.Name          := Key_String_Format("LOWERCASE W");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE W");
            Key.ASCII         := 'w';
            Key.ASCII_Shifted := 'W';
         WHEN 16#24# =>
            Key.Name          := Key_String_Format("LOWERCASE E");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE E");
            Key.ASCII         := 'e';
            Key.ASCII_Shifted := 'E';
         WHEN 16#2D# =>
            Key.Name          := Key_String_Format("LOWERCASE R");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE R");
            Key.ASCII         := 'r';
            Key.ASCII_Shifted := 'R';
         WHEN 16#2C# =>
            Key.Name          := Key_String_Format("LOWERCASE T");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE T");
            Key.ASCII         := 't';
            Key.ASCII_Shifted := 'T';
         WHEN 16#35# =>
            Key.Name          := Key_String_Format("LOWERCASE Y");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE Y");
            Key.ASCII         := 'y';
            Key.ASCII_Shifted := 'Y';
         WHEN 16#3C# =>
            Key.Name          := Key_String_Format("LOWERCASE U");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE U");
            Key.ASCII         := 'u';
            Key.ASCII_Shifted := 'U';
         WHEN 16#43# =>
            Key.Name          := Key_String_Format("LOWERCASE I");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE I");
            Key.ASCII         := 'i';
            Key.ASCII_Shifted := 'I';
         WHEN 16#44# =>
            Key.Name          := Key_String_Format("LOWERCASE O");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE O");
            Key.ASCII         := 'o';
            Key.ASCII_Shifted := 'O';
         WHEN 16#4D# =>
            Key.Name          := Key_String_Format("LOWERCASE P");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE P");
            Key.ASCII         := 'p';
            Key.ASCII_Shifted := 'P';
         WHEN 16#1C# =>
            Key.Name          := Key_String_Format("LOWERCASE A");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE A");
            Key.ASCII         := 'a';
            Key.ASCII_Shifted := 'A';
         WHEN 16#1B# =>
            Key.Name          := Key_String_Format("LOWERCASE S");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE S");
            Key.ASCII         := 's';
            Key.ASCII_Shifted := 'S';
         WHEN 16#23# =>
            Key.Name          := Key_String_Format("LOWERCASE D");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE D");
            Key.ASCII         := 'd';
            Key.ASCII_Shifted := 'D';
         WHEN 16#2B# =>
            Key.Name          := Key_String_Format("LOWERCASE F");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE F");
            Key.ASCII         := 'f';
            Key.ASCII_Shifted := 'F';
         WHEN 16#34# =>
            Key.Name          := Key_String_Format("LOWERCASE G");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE G");
            Key.ASCII         := 'g';
            Key.ASCII_Shifted := 'G';
         WHEN 16#33# =>
            Key.Name          := Key_String_Format("LOWERCASE H");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE H");
            Key.ASCII         := 'h';
            Key.ASCII_Shifted := 'H';
         WHEN 16#3B# =>
            Key.Name          := Key_String_Format("LOWERCASE J");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE J");
            Key.ASCII         := 'j';
            Key.ASCII_Shifted := 'J';
         WHEN 16#42# =>
            Key.Name          := Key_String_Format("LOWERCASE K");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE K");
            Key.ASCII         := 'k';
            Key.ASCII_Shifted := 'K';
         WHEN 16#4B# =>
            Key.Name          := Key_String_Format("LOWERCASE L");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE L");
            Key.ASCII         := 'l';
            Key.ASCII_Shifted := 'L';
         WHEN 16#1A# =>
            Key.Name          := Key_String_Format("LOWERCASE Z");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE Z");
            Key.ASCII         := 'z';
            Key.ASCII_Shifted := 'Z';
         WHEN 16#22# =>
            Key.Name          := Key_String_Format("LOWERCASE X");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE X");
            Key.ASCII         := 'x';
            Key.ASCII_Shifted := 'X';
         WHEN 16#21# =>
            Key.Name          := Key_String_Format("LOWERCASE C");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE C");
            Key.ASCII         := 'c';
            Key.ASCII_Shifted := 'C';
         WHEN 16#2A# =>
            Key.Name          := Key_String_Format("LOWERCASE V");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE V");
            Key.ASCII         := 'v';
            Key.ASCII_Shifted := 'V';
         WHEN 16#32# =>
            Key.Name          := Key_String_Format("LOWERCASE B");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE B");
            Key.ASCII         := 'b';
            Key.ASCII_Shifted := 'B';
         WHEN 16#31# =>
            Key.Name          := Key_String_Format("LOWERCASE N");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE N");
            Key.ASCII         := 'n';
            Key.ASCII_Shifted := 'N';
         WHEN 16#3A# =>
            Key.Name          := Key_String_Format("LOWERCASE M");
            Key.Name_Shifted  := Key_String_Format("UPPERCASE M");
            Key.ASCII         := 'm';
            Key.ASCII_Shifted := 'M';

         -- Number and symbol keys.
         WHEN 16#16# =>
            Key.Name          := Key_String_Format("ONE");
            Key.Name_Shifted  := Key_String_Format("EXCLAIMATION MARK");
            Key.ASCII         := '1';
            Key.ASCII_Shifted := '!';
         WHEN 16#1E# =>
            Key.Name          := Key_String_Format("TWO");
            Key.Name_Shifted  := Key_String_Format("AT");
            Key.ASCII         := '2';
            Key.ASCII_Shifted := '@';
         WHEN 16#26# =>
            Key.Name          := Key_String_Format("THREE");
            Key.Name_Shifted  := Key_String_Format("HASH");
            Key.ASCII         := '3';
            Key.ASCII_Shifted := '#';
         WHEN 16#25# =>
            Key.Name          := Key_String_Format("FOUR");
            Key.Name_Shifted  := Key_String_Format("DOLLAR");
            Key.ASCII         := '4';
            Key.ASCII_Shifted := '$';
         WHEN 16#2E# =>
            Key.Name          := Key_String_Format("FIVE");
            Key.Name_Shifted  := Key_String_Format("PERCENTAGE MARK");
            Key.ASCII         := '5';
            Key.ASCII_Shifted := '%';
         WHEN 16#36# =>
            Key.Name          := Key_String_Format("SIX");
            Key.Name_Shifted  := Key_String_Format("CARET");
            Key.ASCII         := '6';
            Key.ASCII_Shifted := '^';
         WHEN 16#3D# =>
            Key.Name          := Key_String_Format("SEVEN");
            Key.Name_Shifted  := Key_String_Format("AMPERSAND");
            Key.ASCII         := '7';
            Key.ASCII_Shifted := '&';
         WHEN 16#3E# =>
            Key.Name          := Key_String_Format("EIGHT");
            Key.Name_Shifted  := Key_String_Format("ASTERIX");
            Key.ASCII         := '8';
            Key.ASCII_Shifted := '*';
         WHEN 16#46# =>
            Key.Name          := Key_String_Format("NINE");
            Key.Name_Shifted  := Key_String_Format("LEFT PARENTHESIS");
            Key.ASCII         := '9';
            Key.ASCII_Shifted := '(';
         WHEN 16#45# =>
            Key.Name          := Key_String_Format("ZERO");
            Key.Name_Shifted  := Key_String_Format("RIGHT PARENTHESIS");
            Key.ASCII         := '0';
            Key.ASCII_Shifted := ')';

         -- Symbol keys.
         WHEN 16#54# =>
            Key.Name          := Key_String_Format("LEFT SQUARE BRACKET");
            Key.Name_Shifted  := Key_String_Format("LEFT CURLY BRACKET");
            Key.ASCII         := '[';
            Key.ASCII_Shifted := '{';
         WHEN 16#5B# =>
            Key.Name          := Key_String_Format("RIGHT SQUARE BRACKET");
            Key.Name_Shifted  := Key_String_Format("RIGHT CURLY BRACKET");
            Key.ASCII         := ']';
            Key.ASCII_Shifted := '}';
         WHEN 16#41# =>
            Key.Name          := Key_String_Format("COMMA");
            Key.Name_Shifted  := Key_String_Format("LESS-THAN");
            Key.ASCII         := ',';
            Key.ASCII_Shifted := '<';
         WHEN 16#49# =>
            Key.Name          := Key_String_Format("FULL-STOP");
            Key.Name_Shifted  := Key_String_Format("GREATER-THAN");
            Key.ASCII         := '.';
            Key.ASCII_Shifted := '>';
         WHEN 16#4A# =>
            Key.Name          := Key_String_Format("FORWARD SLASH");
            Key.Name_Shifted  := Key_String_Format("QUESTION MARK");
            Key.ASCII         := '/';
            Key.ASCII_Shifted := '?';
         WHEN 16#5D# =>
            Key.Name          := Key_String_Format("BACKSLASH");
            Key.Name_Shifted  := Key_String_Format("PIPE");
            Key.ASCII         := '\';
            Key.ASCII_Shifted := '|';
         WHEN 16#4C# =>
            Key.Name          := Key_String_Format("SEMICOLON");
            Key.Name_Shifted  := Key_String_Format("COLON");
            Key.ASCII         := ';';
            Key.ASCII_Shifted := ':';
         WHEN 16#52# =>
            Key.Name          := Key_String_Format("APOSTROPHE");
            Key.Name_Shifted  := Key_String_Format("SPEECH MARK");
            Key.ASCII         := ''';
            Key.ASCII_Shifted := '"';
         WHEN 16#0E# =>
            Key.Name          := Key_String_Format("BACKTICK");
            Key.Name_Shifted  := Key_String_Format("TILDE");
            Key.ASCII         := '`';
            Key.ASCII_Shifted := '~';
         WHEN 16#4E# =>
            Key.Name          := Key_String_Format("MINUS");
            Key.Name_Shifted  := Key_String_Format("UNDERSCORE");
            Key.ASCII         := '-';
            Key.ASCII_Shifted := '_';
         WHEN 16#55# =>
            Key.Name          := Key_String_Format("EQUALS");
            Key.Name_Shifted  := Key_String_Format("PLUS");
            Key.ASCII         := '=';
            Key.ASCII_Shifted := '+';

         WHEN OTHERS =>
            Key.Name          := Key_String_Format("UNKNOWN");
            Key.Name_Shifted  := Key.Name;
            Key.ASCII         := character'val(0);
            Key.ASCII_Shifted := character'val(0);
            Key.Printable     := false;
      END CASE;

      RETURN Key;
   END Scancode_Set_2;

END HAVK_PS2.Keyboard;
