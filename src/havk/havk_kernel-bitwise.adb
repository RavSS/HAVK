WITH
   System.Machine_Code;
USE
   System.Machine_Code;

PACKAGE BODY HAVK_Kernel.Bitwise
IS
   FUNCTION Lsh64(
      Value : IN u64;
      Shifts : IN u8)
   RETURN u64 IS
      Shifted : u64 := Value;
   BEGIN
      Asm(  -- The bits to shift by MUST be in the RCX register's
         -- lower 8 bits, so that's the CL register. Shows
         -- how old x86 is, but there should be no reason to
         -- shift beyond the size of a byte.
         "MOV CL, %1;" &
         "SHL %0, CL;",
         Outputs => u64'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc", -- Carry flag gets modified.
         Volatile => True);
      RETURN Shifted;
   END Lsh64;

   -- Same as `Lsh()`, only difference
   -- is that "SHR" is being used instead of "SHL".
   FUNCTION Rsh64(
      Value : IN u64;
      Shifts : IN u8)
   RETURN u64 IS
      Shifted : u64 := Value;
   BEGIN
      Asm(  "MOV CL, %1;" &
         "SHR %0, CL;",
         Outputs => u64'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc",
         Volatile => True);
      RETURN Shifted;
   END Rsh64;

   -- The shift operations below accomodate different types.
   -- I don't know how to use Ada generics as GNAT won't let me call
   -- them, but I have a feeling that I probably shouldn't use them
   -- with inline assembly being involved in a ZFP RTS.

   FUNCTION Lsh32(
      Value : IN u32;
      Shifts : IN u8)
   RETURN u32 IS
      Shifted : u32 := Value;
   BEGIN
      Asm(  "MOV CL, %1;" &
         "SHL %0, CL;",
         Outputs => u32'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc",
         Volatile => True);
      RETURN Shifted;
   END Lsh32;

   FUNCTION Rsh32(
      Value : IN u32;
      Shifts : IN u8)
   RETURN u32 IS
      Shifted : u32 := Value;
   BEGIN
      Asm(  "MOV CL, %1;" &
         "SHR %0, CL;",
         Outputs => u32'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc",
         Volatile => True);
      RETURN Shifted;
   END Rsh32;

   FUNCTION Lsh8(
      Value : IN u8;
      Shifts : IN u8)
   RETURN u8 IS
      Shifted : u8 := Value;
   BEGIN
      Asm(  "MOV CL, %1;" &
         "SHL %0, CL;",
         Outputs => u8'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc",
         Volatile => True);
      RETURN Shifted;
   END Lsh8;

   FUNCTION Rsh8(
      Value : IN u8;
      Shifts : IN u8)
   RETURN u8 IS
      Shifted : u8 := Value;
   BEGIN
      Asm(  "MOV CL, %1;" &
         "SHR %0, CL;",
         Outputs => u8'asm_output("+r", Shifted),
         Inputs => u8'asm_input("g", Shifts),
         Clobber => "cl, cc",
         Volatile => True);
      RETURN Shifted;
   END Rsh8;

   FUNCTION Bt(
      Value : IN u64;
      Bit : IN u8)
   RETURN boolean IS
      Result : boolean;
   BEGIN
      Asm(  "BT %1, %2;",
         Outputs => boolean'asm_output("=@ccc", Result),
         Inputs => (u64'asm_input("g", Value),
            u64'asm_input("g", u64(Bit))),
         Volatile => True);
      RETURN Result;
   END Bt;
END HAVK_Kernel.Bitwise;
