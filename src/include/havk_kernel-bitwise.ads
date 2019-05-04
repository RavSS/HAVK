PACKAGE HAVK_Kernel.Bitwise
IS
   -- For some reason, shift operators are not built into the language
   -- itself. They're in the Interfaces package, and even then, GNAT
   -- has to provide its own specific "intrinsic" version of it.
   -- I'll just provide my own slower less confusing functions instead.
   FUNCTION Lsh64(
      Value : IN u64;
      Shifts : IN u8)
   RETURN u64;

   FUNCTION Rsh64(
      Value : IN u64;
      Shifts : IN u8)
   RETURN u64;

   FUNCTION Lsh32(
      Value : IN u32;
      Shifts : IN u8)
   RETURN u32;

   FUNCTION Rsh32(
      Value : IN u32;
      Shifts : IN u8)
   RETURN u32;

   FUNCTION Lsh8(
      Value : IN u8;
      Shifts : IN u8)
   RETURN u8;

   FUNCTION Rsh8(
      Value : IN u8;
      Shifts : IN u8)
   RETURN u8;

   FUNCTION Bt(
      Value : IN u64;
      Bit : IN u8)
   RETURN boolean;
END HAVK_Kernel.Bitwise;
