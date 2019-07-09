-- This is the main package spec for the kernel itself, it just contains
-- some types for usage everywhere else, along with some shortcuts.
WITH
   System,
   Ada.Unchecked_Conversion;

PACKAGE HAVK_Kernel IS
   -- These types are to be used when the size really does not matter.
   -- Because HAVK is a 64-bit kernel, I'll make the "default" types 64-bit.
   TYPE num  IS MOD 2 ** 64; -- Natural number, assuming you include zero.
   TYPE nums IS ARRAY(num RANGE <>) OF num;
   FOR  num'size USE 64;
   PRAGMA Provide_Shift_Operators(num); -- GNAT shift intrinsics are provided.

   TYPE int  IS RANGE -(2 ** 63) .. +(2 ** 63 - 1); -- Integer.
   TYPE ints IS ARRAY(num RANGE <>) OF int;
   FOR  int'size USE 64;
   PRAGMA Provide_Shift_Operators(int);

   -- These types should be avoided, but sometimes you need a new type
   -- with a specific range. Don't use these for array/record packing, please.
   TYPE u64  IS MOD 2 ** 64;
   TYPE u32  IS MOD 2 ** 32;
   TYPE u16  IS MOD 2 ** 16;
   TYPE  u8  IS MOD 2 **  8;

   TYPE s64  IS RANGE -(2 ** 63) .. +(2 ** 63 - 1);
   TYPE s32  IS RANGE -(2 ** 31) .. +(2 ** 31 - 1);
   TYPE s16  IS RANGE -(2 ** 15) .. +(2 ** 15 - 1);
   TYPE  s8  IS RANGE -(2 **  7) .. +(2 **  7 - 1);

   -- Generic arrays. These will rarely be used to reference memory (aliased).
   TYPE u64s IS ARRAY(num RANGE <>) OF ALIASED u64;
   TYPE u32s IS ARRAY(num RANGE <>) OF ALIASED u32;
   TYPE u16s IS ARRAY(num RANGE <>) OF ALIASED u16;
   TYPE  u8s IS ARRAY(num RANGE <>) OF ALIASED u8;

   TYPE s64s IS ARRAY(num RANGE <>) OF ALIASED s64;
   TYPE s32s IS ARRAY(num RANGE <>) OF ALIASED s32;
   TYPE s16s IS ARRAY(num RANGE <>) OF ALIASED s16;
   TYPE  s8s IS ARRAY(num RANGE <>) OF ALIASED s8;

   -- Size configurations for those types.
   FOR u64'size USE 64;
   FOR u32'size USE 32;
   FOR u16'size USE 16;
   FOR  u8'size USE 8;

   FOR s64'size USE 64;
   FOR s32'size USE 32;
   FOR s16'size USE 16;
   FOR  s8'size USE 8;

   -- Now the array component sizes.
   FOR u64s'component_size USE 64;
   FOR u32s'component_size USE 32;
   FOR u16s'component_size USE 16;
   FOR  u8s'component_size USE  8;

   FOR s64s'component_size USE 64;
   FOR s32s'component_size USE 32;
   FOR s16s'component_size USE 16;
   FOR  s8s'component_size USE  8;

   -- This is for converting "System.Address" to "num."
   FUNCTION Address_To_num IS NEW Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => num);

   -- The `Debug_*()` calls are just wrappers for the "HAVK_Kernel.Debug"
   -- package. Mostly so I don't have to "WITH" it everywhere.
   PROCEDURE Debug_Initialize
   WITH
      Inline => true;

   PROCEDURE Debug_Message(
      Message : IN string)
   WITH
      Inline => true;
END HAVK_Kernel;
