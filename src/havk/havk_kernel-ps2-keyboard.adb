WITH
   HAVK_Kernel.Input,
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Input,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PS2.Keyboard
IS
   PROCEDURE Interrupt_Manager -- This could be more efficient...
   IS
      Codes : ARRAY(num RANGE 1 .. 3) OF num RANGE 0 .. 16#FF# :=
         (OTHERS => 0);
   BEGIN
      CASE PS2.Input_Controller.Current_Scancode_Set IS
         WHEN set_1 => -- TODO: Set 1 is unimplemented.
            NULL;

         WHEN set_2 => -- TODO: Set 2 is mostly implemented.
            Codes(1) := INB(data'enum_rep);

            IF Codes(1) = 16#12#      THEN -- TODO: Can get stuck in reverse.
               IF Current_Shift_State THEN
                  Current_Shift_State := false;
               ELSE
                  Current_Shift_State := true;
               END IF;
            ELSIF Codes(1) /= 16#E0#  THEN -- TODO: Handle the special keys.
               IF Codes(1) /= 16#F0#  THEN -- Handle the break.
                  Codes(2) := INB(data'enum_rep);
                  Scancode_Set_2(Codes(2), Current_Shift_State, false);
               ELSE
                  Codes(2) := INB(data'enum_rep);
                  Scancode_Set_2(Codes(2), Current_Shift_State, true);
               END IF;
            END IF;

         WHEN set_3 => -- TODO: Set 3 is unimplemented.
            NULL;
      END CASE;

      PS2.Input_Controller.Flush;
   END Interrupt_Manager;

   PROCEDURE Scancode_Set_2( -- TODO: This is not fully complete, I believe.
      Scancode      : IN num;
      Shifted       : IN boolean;
      Break         : IN boolean)
   IS
      Name          : key_string;
      Name_Shifted  : key_string;
      ASCII         : character;
      ASCII_Shifted : character;
      Printable     : boolean := true; -- True by default, saves repeating it.
   BEGIN
      CASE Scancode IS
         WHEN 16#66# =>
            Name          := Key_String_Format("BACKSPACE");
            Name_Shifted  := Name;
            ASCII         := character'val(08);
            ASCII_Shifted := ASCII;
            Printable     := false;
         WHEN 16#0D# =>
            Name          := Key_String_Format("TAB");
            Name_Shifted  := Name;
            ASCII         := character'val(09);
            ASCII_Shifted := ASCII;
            Printable     := false;
         WHEN 16#5A# =>
            Name          := Key_String_Format("ENTER");
            Name_Shifted  := Name;
            ASCII         := character'val(10);
            ASCII_Shifted := ASCII;
            Printable     := false;
         WHEN 16#76# =>
            Name          := Key_String_Format("ESCAPE");
            Name_Shifted  := Name;
            ASCII         := character'val(27);
            ASCII_Shifted := ASCII;
            Printable     := false;

         WHEN 16#29# =>
            Name          := Key_String_Format("SPACEBAR");
            Name_Shifted  := Name;
            ASCII         := character'val(20);
            ASCII_Shifted := ASCII;
         WHEN 16#15# =>
            Name          := Key_String_Format("LOWERCASE Q");
            Name_Shifted  := Key_String_Format("UPPERCASE Q");
            ASCII         := 'q';
            ASCII_Shifted := 'Q';
         WHEN 16#1D# =>
            Name          := Key_String_Format("LOWERCASE W");
            Name_Shifted  := Key_String_Format("UPPERCASE W");
            ASCII         := 'w';
            ASCII_Shifted := 'W';
         WHEN 16#24# =>
            Name          := Key_String_Format("LOWERCASE E");
            Name_Shifted  := Key_String_Format("UPPERCASE E");
            ASCII         := 'e';
            ASCII_Shifted := 'E';
         WHEN 16#2D# =>
            Name          := Key_String_Format("LOWERCASE R");
            Name_Shifted  := Key_String_Format("UPPERCASE R");
            ASCII         := 'r';
            ASCII_Shifted := 'R';
         WHEN 16#2C# =>
            Name          := Key_String_Format("LOWERCASE T");
            Name_Shifted  := Key_String_Format("UPPERCASE T");
            ASCII         := 't';
            ASCII_Shifted := 'T';
         WHEN 16#35# =>
            Name          := Key_String_Format("LOWERCASE Y");
            Name_Shifted  := Key_String_Format("UPPERCASE Y");
            ASCII         := 'y';
            ASCII_Shifted := 'Y';
         WHEN 16#3C# =>
            Name          := Key_String_Format("LOWERCASE U");
            Name_Shifted  := Key_String_Format("UPPERCASE U");
            ASCII         := 'u';
            ASCII_Shifted := 'U';
         WHEN 16#43# =>
            Name          := Key_String_Format("LOWERCASE I");
            Name_Shifted  := Key_String_Format("UPPERCASE I");
            ASCII         := 'i';
            ASCII_Shifted := 'I';
         WHEN 16#44# =>
            Name          := Key_String_Format("LOWERCASE O");
            Name_Shifted  := Key_String_Format("UPPERCASE O");
            ASCII         := 'o';
            ASCII_Shifted := 'O';
         WHEN 16#4D# =>
            Name          := Key_String_Format("LOWERCASE P");
            Name_Shifted  := Key_String_Format("UPPERCASE P");
            ASCII         := 'p';
            ASCII_Shifted := 'P';
         WHEN 16#1C# =>
            Name          := Key_String_Format("LOWERCASE A");
            Name_Shifted  := Key_String_Format("UPPERCASE A");
            ASCII         := 'a';
            ASCII_Shifted := 'A';
         WHEN 16#1B# =>
            Name          := Key_String_Format("LOWERCASE S");
            Name_Shifted  := Key_String_Format("UPPERCASE S");
            ASCII         := 's';
            ASCII_Shifted := 'S';
         WHEN 16#23# =>
            Name          := Key_String_Format("LOWERCASE D");
            Name_Shifted  := Key_String_Format("UPPERCASE D");
            ASCII         := 'd';
            ASCII_Shifted := 'D';
         WHEN 16#2B# =>
            Name          := Key_String_Format("LOWERCASE F");
            Name_Shifted  := Key_String_Format("UPPERCASE F");
            ASCII         := 'f';
            ASCII_Shifted := 'F';
         WHEN 16#34# =>
            Name          := Key_String_Format("LOWERCASE G");
            Name_Shifted  := Key_String_Format("UPPERCASE G");
            ASCII         := 'g';
            ASCII_Shifted := 'G';
         WHEN 16#33# =>
            Name          := Key_String_Format("LOWERCASE H");
            Name_Shifted  := Key_String_Format("UPPERCASE H");
            ASCII         := 'h';
            ASCII_Shifted := 'H';
         WHEN 16#3B# =>
            Name          := Key_String_Format("LOWERCASE J");
            Name_Shifted  := Key_String_Format("UPPERCASE J");
            ASCII         := 'j';
            ASCII_Shifted := 'J';
         WHEN 16#42# =>
            Name          := Key_String_Format("LOWERCASE K");
            Name_Shifted  := Key_String_Format("UPPERCASE K");
            ASCII         := 'k';
            ASCII_Shifted := 'K';
         WHEN 16#4B# =>
            Name          := Key_String_Format("LOWERCASE L");
            Name_Shifted  := Key_String_Format("UPPERCASE L");
            ASCII         := 'l';
            ASCII_Shifted := 'L';
         WHEN 16#1A# =>
            Name          := Key_String_Format("LOWERCASE Z");
            Name_Shifted  := Key_String_Format("UPPERCASE Z");
            ASCII         := 'z';
            ASCII_Shifted := 'Z';
         WHEN 16#22# =>
            Name          := Key_String_Format("LOWERCASE X");
            Name_Shifted  := Key_String_Format("UPPERCASE X");
            ASCII         := 'x';
            ASCII_Shifted := 'X';
         WHEN 16#21# =>
            Name          := Key_String_Format("LOWERCASE C");
            Name_Shifted  := Key_String_Format("UPPERCASE C");
            ASCII         := 'c';
            ASCII_Shifted := 'C';
         WHEN 16#2A# =>
            Name          := Key_String_Format("LOWERCASE V");
            Name_Shifted  := Key_String_Format("UPPERCASE V");
            ASCII         := 'v';
            ASCII_Shifted := 'V';
         WHEN 16#32# =>
            Name          := Key_String_Format("LOWERCASE B");
            Name_Shifted  := Key_String_Format("UPPERCASE B");
            ASCII         := 'b';
            ASCII_Shifted := 'B';
         WHEN 16#31# =>
            Name          := Key_String_Format("LOWERCASE N");
            Name_Shifted  := Key_String_Format("UPPERCASE N");
            ASCII         := 'n';
            ASCII_Shifted := 'N';
         WHEN 16#3A# =>
            Name          := Key_String_Format("LOWERCASE M");
            Name_Shifted  := Key_String_Format("UPPERCASE M");
            ASCII         := 'm';
            ASCII_Shifted := 'M';

         WHEN 16#16# =>
            Name          := Key_String_Format("ONE");
            Name_Shifted  := Key_String_Format("EXCLAIMATION MARK");
            ASCII         := '1';
            ASCII_Shifted := '!';
         WHEN 16#1E# =>
            Name          := Key_String_Format("TWO");
            Name_Shifted  := Key_String_Format("AT");
            ASCII         := '2';
            ASCII_Shifted := '@';
         WHEN 16#26# =>
            Name          := Key_String_Format("THREE");
            Name_Shifted  := Key_String_Format("HASH");
            ASCII         := '3';
            ASCII_Shifted := '#';
         WHEN 16#25# =>
            Name          := Key_String_Format("FOUR");
            Name_Shifted  := Key_String_Format("DOLLAR");
            ASCII         := '4';
            ASCII_Shifted := '$';
         WHEN 16#2E# =>
            Name          := Key_String_Format("FIVE");
            Name_Shifted  := Key_String_Format("PERCENTAGE MARK");
            ASCII         := '5';
            ASCII_Shifted := '%';
         WHEN 16#36# =>
            Name          := Key_String_Format("SIX");
            Name_Shifted  := Key_String_Format("CARET");
            ASCII         := '6';
            ASCII_Shifted := '^';
         WHEN 16#3D# =>
            Name          := Key_String_Format("SEVEN");
            Name_Shifted  := Key_String_Format("AMPERSAND");
            ASCII         := '7';
            ASCII_Shifted := '&';
         WHEN 16#3E# =>
            Name          := Key_String_Format("EIGHT");
            Name_Shifted  := Key_String_Format("ASTERIX");
            ASCII         := '8';
            ASCII_Shifted := '*';
         WHEN 16#46# =>
            Name          := Key_String_Format("NINE");
            Name_Shifted  := Key_String_Format("LEFT PARENTHESIS");
            ASCII         := '9';
            ASCII_Shifted := '(';
         WHEN 16#45# =>
            Name          := Key_String_Format("ZERO");
            Name_Shifted  := Key_String_Format("RIGHT PARENTHESIS");
            ASCII         := '0';
            ASCII_Shifted := ')';

         WHEN 16#54# =>
            Name          := Key_String_Format("LEFT SQUARE BRACKET");
            Name_Shifted  := Key_String_Format("LEFT CURLY BRACKET");
            ASCII         := '[';
            ASCII_Shifted := '{';
         WHEN 16#5B# =>
            Name          := Key_String_Format("RIGHT SQUARE BRACKET");
            Name_Shifted  := Key_String_Format("RIGHT CURLY BRACKET");
            ASCII         := ']';
            ASCII_Shifted := '}';
         WHEN 16#41# =>
            Name          := Key_String_Format("COMMA");
            Name_Shifted  := Key_String_Format("LESS-THAN");
            ASCII         := ',';
            ASCII_Shifted := '<';
         WHEN 16#49# =>
            Name          := Key_String_Format("FULL-STOP");
            Name_Shifted  := Key_String_Format("GREATER-THAN");
            ASCII         := '.';
            ASCII_Shifted := '>';
         WHEN 16#4A# =>
            Name          := Key_String_Format("FORWARD SLASH");
            Name_Shifted  := Key_String_Format("QUESTION MARK");
            ASCII         := '/';
            ASCII_Shifted := '?';
         WHEN 16#5D# =>
            Name          := Key_String_Format("BACKSLASH");
            Name_Shifted  := Key_String_Format("PIPE");
            ASCII         := '\';
            ASCII_Shifted := '|';
         WHEN 16#4C# =>
            Name          := Key_String_Format("SEMICOLON");
            Name_Shifted  := Key_String_Format("COLON");
            ASCII         := ';';
            ASCII_Shifted := ':';
         WHEN 16#52# =>
            Name          := Key_String_Format("APOSTROPHE");
            Name_Shifted  := Key_String_Format("SPEECH MARK");
            ASCII         := ''';
            ASCII_Shifted := '"';
         WHEN 16#0E# =>
            Name          := Key_String_Format("BACKTICK");
            Name_Shifted  := Key_String_Format("TILDE");
            ASCII         := '`';
            ASCII_Shifted := '~';
         WHEN 16#4E# =>
            Name          := Key_String_Format("MINUS");
            Name_Shifted  := Key_String_Format("UNDERSCORE");
            ASCII         := '-';
            ASCII_Shifted := '_';
         WHEN 16#55# =>
            Name          := Key_String_Format("EQUALS");
            Name_Shifted  := Key_String_Format("PLUS");
            ASCII         := '=';
            ASCII_Shifted := '+';

         WHEN OTHERS =>
            Name          := Key_String_Format("UNKNOWN");
            Name_Shifted  := Key_String_Format("UNKNOWN");
            ASCII         := character'val(0);
            ASCII_Shifted := ASCII;
      END CASE;

      Set_Key_State(Name, Name_Shifted, ASCII, ASCII_Shifted,
         Printable, Break, Shifted);
   END Scancode_Set_2;

END HAVK_Kernel.PS2.Keyboard;
