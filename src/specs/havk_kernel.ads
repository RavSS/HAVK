-- This is the main package spec for the kernel itself, it just contains
-- some types for usage everywhere else, along with some shortcuts.
WITH
   System,
   Ada.Unchecked_Conversion;

PACKAGE HAVK_Kernel
IS
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

   -- This is for converting "System.Address" to "num."
   FUNCTION Address_To_num IS NEW Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => num);

   -- The `Debug_*()` calls are just wrappers for the "HAVK_Kernel.Debug"
   -- package. Mostly so I don't have to "WITH" it everywhere.
   PROCEDURE Debug_Initialise;

   PROCEDURE Debug_Message(
      Message : IN string);
END HAVK_Kernel;
