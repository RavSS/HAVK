WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Exceptions
WITH
   -- If anything in this package is called, then it's already game over.
   -- Utilising SPARK outside of here should mean nothing here matters, or in
   -- the case of a kernel panic, a program failure is intended on purpose.
   SPARK_Mode => off
IS
   PROCEDURE Last_Chance_Handler(
      Source_Location : IN System.Address;
      Line            : IN integer)
   IS
      Mnemonic        : CONSTANT num := 16#ADAC0DE_8_DEADC0DE#;

      PROCEDURE Debug_Crash
      WITH
         Inline    => true,
         No_Return => true;

      PROCEDURE Debug_Crash
      IS
         FUNCTION strlen(
            Pointer   : IN System.Address)
         RETURN integer
         WITH
            Import        => true,
            Inline        => true,
            Convention    => NASM,
            External_Name => "strlen";

         C_String     : CONSTANT string(1 .. strlen(Source_Location) + 1)
         WITH
            Import        => true,
            Convention    => C,
            Address       => Source_Location;
      BEGIN
         Log("Crashed - """ & C_String & ":" & Line'img & """!", fatal);
         LOOP
            Asm(  -- Works for now as a quick indicator.
               "MOV RAX, %0;" &
               "MOV RBX, %0;" &
               "MOV RCX, %0;" &
               "MOV RDX, %0;" &
               "MOV RSI, %1;" & -- Character array of source file's name.
               "MOV EDI, %2;" & -- Line destination in the source file.
               "MOV R8,  %0;" &
               "MOV R9,  %0;" &
               "MOV R10, %0;" &
               "MOV R11, %0;" &
               "MOV R12, %0;" &
               "MOV R13, %0;" &
               "MOV R14, %0;" &
               "MOV R15, %0;" &
               "PAUSE;",
               Inputs   => (num'Asm_Input("g", Mnemonic),
                           System.Address'Asm_Input("g", Source_Location),
                           integer'Asm_Input("g", Line)),
               Clobber  => "rax, rbx, rcx, rdx, rsi, edi," &
                           "r8,  r9,  r10, r11, r12, r13, r14, r15",
               Volatile => true);
         END LOOP;
      END Debug_Crash;
   BEGIN
      Asm("CLI;", Volatile => true);
      PRAGMA Debug(Debug_Crash);
      LOOP -- Halting loop only for final version so Bochs with GDB works.
         Asm("HLT; CLI;", Volatile => true);
      END LOOP;
   END Last_Chance_Handler;

   PROCEDURE Stack_Smash_Handler
   IS
      Message : CONSTANT string := "stack smashed" & character'val(0);
   BEGIN
      Log("The stack has been smashed.", fatal);
      Last_Chance_Handler(Message'address, 0);
      -- Do not continue going.
   END Stack_Smash_Handler;

   PROCEDURE Tears_In_Rain(
      Message : IN string;
      File    : IN string;
      Line    : IN integer)
   IS
      Fatal_Message : CONSTANT string := Message & " - " & File &
         character'val(0);
   BEGIN
      -- TODO: After logging has been fully completed, use the functions here.
      Log("Manual kernel panic called.", fatal);
      Last_Chance_Handler(Fatal_Message'address, Line);
      -- Do not continue going.
   END Tears_In_Rain;
END HAVK_Kernel.Exceptions;
