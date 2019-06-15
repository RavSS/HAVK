WITH
   System.Machine_Code,
   HAVK_Kernel;
USE
   System.Machine_Code,
   HAVK_Kernel;

PACKAGE BODY HAVK_Kernel.Exceptions
IS
   PROCEDURE Last_Chance_Handler(
      Source_Location : System.Address;
      Line            : integer)
   IS
      Mnemonic : CONSTANT u64 := 16#ADAC0DE8DEADC0DE#;
   BEGIN
      LOOP
         Asm(  -- Works for now as a quick indicator.
            "CLI;"         &
            "MOV RAX, %0;" &
            "MOV RBX, %0;" &
            "MOV RCX, %0;" &
            "MOV RDX, %0;" &
            "MOV RSI, %1;" & -- Pointer to source file name string.
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
            "HLT;"         ,
            Inputs   =>
            (
               u64'Asm_Input("g", Mnemonic),
               System.Address'Asm_Input("g", Source_Location),
               integer'Asm_Input("g", Line)
            ),
            Clobber  => "rax, rbx, rcx, rdx, rsi, edi, rsp," &
                        "r8,  r9,  r10, r11, r12, r13, r14, r15",
            Volatile => True);
      END LOOP;
   END Last_Chance_Handler;

   -- Just a wrapper for the last chance handler for now, as a
   -- symbol is created for the stack check fail if stack protection
   -- is enabled during compilation.
   PROCEDURE Stack_Smash_Handler
   IS
   BEGIN
      Last_Chance_Handler(System'To_Address(16#AAAAAAAA#), integer'last);
   END Stack_Smash_Handler;
END HAVK_Kernel.Exceptions;
