-- This is the main package for the kernel itself, it just contains
-- some types for usage everywhere else.
WITH
   System,
   Ada.Unchecked_Conversion;

PACKAGE HAVK_Kernel IS
   -- These types are to be used when the size really does not matter.
   -- Because HAVK is a 64-bit kernel, I'll make the "default" types 64-bit.
   TYPE num  IS MOD 2 ** 64; -- Natural number, assuming you include zero.
   TYPE nums IS ARRAY(num RANGE <>) OF num;
   FOR  num'size USE 64;
   PRAGMA Provide_Shift_Operators(num);

   TYPE int  IS RANGE -(2 ** 63) .. +(2 ** 63 - 1); -- Integer.
   TYPE ints IS ARRAY(num RANGE <>) OF int;
   FOR  int'size USE 64;
   PRAGMA Provide_Shift_Operators(int);

   -- These types should be avoided, but sometimes you need a new type
   -- with a specific range. Don't use these for array/record packing, please.
   TYPE  u64 IS MOD 2 ** 64;
   TYPE  u32 IS MOD 2 ** 32;
   TYPE  u16 IS MOD 2 ** 16;
   TYPE   u8 IS MOD 2 **  8;

   TYPE  s64 IS RANGE -(2 ** 63) .. +(2 ** 63 - 1);
   TYPE  s32 IS RANGE -(2 ** 31) .. +(2 ** 31 - 1);
   TYPE  s16 IS RANGE -(2 ** 15) .. +(2 ** 15 - 1);
   TYPE   s8 IS RANGE -(2 **  7) .. +(2 **  7 - 1);

   -- Generic arrays.
   TYPE u64s IS ARRAY(num RANGE <>) OF u64;
   TYPE u32s IS ARRAY(num RANGE <>) OF u32;
   TYPE u16s IS ARRAY(num RANGE <>) OF u16;
   TYPE  u8s IS ARRAY(num RANGE <>) OF  u8;

   TYPE s64s IS ARRAY(num RANGE <>) OF s64;
   TYPE s32s IS ARRAY(num RANGE <>) OF s32;
   TYPE s16s IS ARRAY(num RANGE <>) OF s16;
   TYPE  s8s IS ARRAY(num RANGE <>) OF  s8;

   TYPE  str IS ARRAY(num RANGE <>) OF character;

   -- This is for converting "System.Address" to "num."
   FUNCTION Address_To_num IS NEW Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => num);
END HAVK_Kernel;
