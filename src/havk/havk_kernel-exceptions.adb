WITH
   System.Machine_Code,
   HAVK_Kernel;
USE
   System.Machine_Code,
   HAVK_Kernel;

PACKAGE BODY HAVK_Kernel.Exceptions IS
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
         Debug_Message("Crashed - """ & C_String & ":" & Line'img & """!");
         LOOP
            Asm(  -- Works for now as a quick indicator.
               "MOV RAX, %0;" &
               "MOV RBX, %0;" &
               "MOV RCX, %0;" &
               "MOV RDX, %0;" &
               "MOV RSI, %1;" & -- Character array of source file's name.
               "MOV EDI, %2;" & -- Line destination in the source file.
               "MOV RSP, %0;" &
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
               Clobber  => "rax, rbx, rcx, rdx, rsi, edi, rsp," &
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
      PRAGMA Debug(Debug_Message("The stack has been smashed."));
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
      -- TODO: After logging support has been added, use the functions here.
      PRAGMA Debug(Debug_Message("Manual kernel panic called."));
      Last_Chance_Handler(Fatal_Message'address, Line);
      -- Do not continue going.
   END Tears_In_Rain;
END HAVK_Kernel.Exceptions;
