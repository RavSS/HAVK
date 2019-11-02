WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Intrinsics
WITH
   SPARK_Mode => off -- This entire package's body is centred around assembly.
IS
   FUNCTION SHL(
      Value    : IN num;
      Shifts   : IN num)
   RETURN num    IS
      Shift_By : CONSTANT num RANGE 0 .. 127 := Shifts;
      Shifted  : num := Value;
   BEGIN
      Asm(
         -- The bits to shift by must be in the RCX register's
         -- lower 8 bits, so that's the CL register. Shows
         -- how old x86 is, but there should be no reason to
         -- shift beyond the size of a byte. Just going to clobber
         -- the actual register for clarity and performance.
         "MOV RCX, %1;" &
         -- Fun fact: there's also the SHLX/SHRX instructions which do not
         -- affect any flags, but they belong to the BMI2 instruction set
         -- and are only present on Intel's Haswell and AMD's Excavator
         -- architectures or newer.
         "SHL %0, CL;",
         Outputs  => num'asm_output("+r", Shifted),
         Inputs   => num'asm_input("g",  Shift_By),
         Clobber  => "rcx, cc", -- Carry flag gets modified.
         Volatile => true);
      RETURN Shifted;
   END SHL;

   -- Same as `SHL()`, only difference is that "SHR"
   -- is being used instead of "SHL".
   FUNCTION SHR(
      Value    : IN num;
      Shifts   : IN num)
   RETURN num    IS
      Shift_By : CONSTANT num RANGE 0 .. 127 := Shifts;
      Shifted  : num := Value;
   BEGIN
      Asm(
         "MOV RCX, %1;" &
         "SHR %0, CL;",
         Outputs  => num'asm_output("+r", Shifted),
         Inputs   => num'asm_input("g",  Shift_By),
         Clobber  => "rcx, cc",
         Volatile => true);
      RETURN Shifted;
   END SHR;

   FUNCTION BT(
      Value    :  IN num;
      Bit      :  IN num)
   RETURN boolean IS
      Result   : boolean;
   BEGIN
      Asm(
         "BT %1, %2;",
         Outputs  => boolean'asm_output("=@ccc", Result), -- Changes flags.
         Inputs   => (num'asm_input("r", Value),
                      num'asm_input("r",  Bit)),
         Clobber  => "cc",
         Volatile => true);
      RETURN Result;
   END BT;

   PROCEDURE OUTB(
      Port     : IN num;
      Value    : IN num)
   IS
      TYPE  word IS MOD 2 ** 16
      WITH
         Size => 16;

      TYPE  byte IS MOD 2 **  8
      WITH
         Size =>  8;
   BEGIN
      Asm(
         "OUTB %1, %0;",
         Inputs   => (byte'asm_input("a",  byte(Value)),
                      word'asm_input("Nd", word(Port))),
         Volatile => true);
   END OUTB;

   FUNCTION INB(
      Port     : IN num)
   RETURN num    IS
      TYPE word  IS MOD 2 ** 16
      WITH
         Size => 16;

      TYPE byte  IS MOD 2 **  8
      WITH
         Size =>  8;

      Read : byte := 0;
   BEGIN
      Asm(
         "INB %0, %1;",
         Outputs  => byte'asm_output("=a",      Read),
         Inputs   => word'asm_input("Nd", word(Port)),
         Volatile => true);
      RETURN num(Read);
   END INB;

   PROCEDURE HLT
   IS
   BEGIN
      Asm("HLT;", Volatile => true);
   END HLT;

   PROCEDURE STI
   IS
   BEGIN
      Asm("STI;", Volatile => true);
   END STI;

   PROCEDURE CLI
   IS
   BEGIN
      Asm("CLI;", Volatile => true);
   END CLI;
END HAVK_Kernel.Intrinsics;
