PACKAGE BODY HAVK_Kernel.PS2.Keyboard IS
   PROCEDURE Set_2_ASCII_Table(
      PS2_Code_Sum : num)
   IS
   BEGIN
      -- 0x1E2 is added to the scancode for shift case (0xE0 + 0xF0 + 0x12).
      CASE PS2_Code_Sum IS
         WHEN 16#66# => Key := character'val(08); -- Backspace.
         WHEN 16#0D# => Key := character'val(09); -- Tab.
         WHEN 16#58# => Key := character'val(10); -- Enter (Unix terminator).
         WHEN 16#76# => Key := character'val(27); -- Escape.

         WHEN 16#15# => Key := 'q'; WHEN 16#1F7# => Key := 'Q';
         WHEN 16#1D# => Key := 'w'; WHEN 16#1FF# => Key := 'W';
         WHEN 16#24# => Key := 'e'; WHEN 16#206# => Key := 'E';
         WHEN 16#2D# => Key := 'r'; WHEN 16#20F# => Key := 'R';
         WHEN 16#2C# => Key := 't'; WHEN 16#20E# => Key := 'T';
         WHEN 16#35# => Key := 'y'; WHEN 16#217# => Key := 'Y';
         WHEN 16#3C# => Key := 'u'; WHEN 16#21E# => Key := 'U';
         WHEN 16#43# => Key := 'i'; WHEN 16#225# => Key := 'I';
         WHEN 16#44# => Key := 'o'; WHEN 16#226# => Key := 'O';
         WHEN 16#4D# => Key := 'p'; WHEN 16#22F# => Key := 'P';
         WHEN 16#1C# => Key := 'a'; WHEN 16#1FE# => Key := 'A';
         WHEN 16#1B# => Key := 's'; WHEN 16#1FD# => Key := 'S';
         WHEN 16#23# => Key := 'd'; WHEN 16#205# => Key := 'D';
         WHEN 16#2B# => Key := 'f'; WHEN 16#20D# => Key := 'F';
         WHEN 16#34# => Key := 'g'; WHEN 16#216# => Key := 'G';
         WHEN 16#33# => Key := 'h'; WHEN 16#215# => Key := 'H';
         WHEN 16#3B# => Key := 'j'; WHEN 16#21D# => Key := 'J';
         WHEN 16#42# => Key := 'k'; WHEN 16#224# => Key := 'K';
         WHEN 16#4B# => Key := 'l'; WHEN 16#22D# => Key := 'L';
         WHEN 16#1A# => Key := 'z'; WHEN 16#1FC# => Key := 'Z';
         WHEN 16#22# => Key := 'x'; WHEN 16#204# => Key := 'X';
         WHEN 16#21# => Key := 'c'; WHEN 16#203# => Key := 'C';
         WHEN 16#2A# => Key := 'v'; WHEN 16#20C# => Key := 'V';
         WHEN 16#32# => Key := 'b'; WHEN 16#214# => Key := 'B';
         WHEN 16#31# => Key := 'n'; WHEN 16#213# => Key := 'N';
         WHEN 16#3A# => Key := 'm'; WHEN 16#21C# => Key := 'M';

         WHEN 16#16# => Key := '1'; WHEN 16#1F8# => Key := '!';
         WHEN 16#1E# => Key := '2'; WHEN 16#200# => Key := '@';
         WHEN 16#26# => Key := '3'; WHEN 16#208# => Key := '#';
         WHEN 16#25# => Key := '4'; WHEN 16#207# => Key := '$';
         WHEN 16#2E# => Key := '5'; WHEN 16#210# => Key := '%';
         WHEN 16#36# => Key := '6'; WHEN 16#218# => Key := '^';
         WHEN 16#3D# => Key := '7'; WHEN 16#21F# => Key := '&';
         WHEN 16#3E# => Key := '8'; WHEN 16#220# => Key := '*';
         WHEN 16#46# => Key := '9'; WHEN 16#228# => Key := '(';
         WHEN 16#45# => Key := '0'; WHEN 16#227# => Key := ')';

         WHEN 16#54# => Key := '['; WHEN 16#236# => Key := '{';
         WHEN 16#5B# => Key := ']'; WHEN 16#23D# => Key := '}';
         WHEN 16#41# => Key := ','; WHEN 16#223# => Key := '<';
         WHEN 16#49# => Key := '.'; WHEN 16#22B# => Key := '>';
         WHEN 16#4A# => Key := '/'; WHEN 16#22C# => Key := '?';
         WHEN 16#5D# => Key := '\'; WHEN 16#23F# => Key := '|';
         WHEN 16#4C# => Key := ';'; WHEN 16#22E# => Key := ':';
         WHEN 16#52# => Key := '''; WHEN 16#23C# => Key := '"';
         WHEN 16#0E# => Key := '`'; WHEN 16#1F0# => Key := '~';
         WHEN 16#4E# => Key := '-'; WHEN 16#230# => Key := '_';
         WHEN 16#55# => Key := '='; WHEN 16#237# => Key := '+';

         -- WHEN 16## => Key := ''; WHEN 16## => Key := '';
         WHEN OTHERS => Key := character'val(0);
      END CASE:
   END Set_2_ASCII_Table;
END HAVK_Kernel.PS2.Keyboard;
