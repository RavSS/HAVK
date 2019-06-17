PACKAGE HAVK_Kernel.PS2 IS

   TYPE PS2_configuration IS RECORD
      Port_1_Enabled     : boolean; -- Enables port 1.
      Port_2_Enabled     : boolean; -- Enables port 2 if present.
      System_POST_Pass   : boolean; -- If the system passed POST (always 1).
      Zeroed_1           : num RANGE 0 .. 0;
      Port_1_Clock       : boolean; -- Enables port 1's clock signal.
      Port_2_Clock       : boolean; -- Enables port 2's clock signal.
      Port_1_Translation : boolean; -- Translate AT to XT for the keyboard.
      Zeroed_2           : num RANGE 0 .. 0;
   END RECORD;
   FOR PS2_configuration USE RECORD
      Port_1_Enabled      AT 0 RANGE 0 .. 0;
      Port_2_Enabled      AT 0 RANGE 1 .. 1;
      System_POST_Pass    AT 0 RANGE 2 .. 2;
      Zeroed_1            AT 0 RANGE 3 .. 3;
      Port_1_Clock        AT 0 RANGE 4 .. 4;
      Port_2_Clock        AT 0 RANGE 5 .. 5;
      Port_1_Translation  AT 0 RANGE 6 .. 6;
      Zeroed_2            AT 0 RANGE 7 .. 7;
   END RECORD;

   PROCEDURE Controller;
END HAVK_Kernel.PS2;
