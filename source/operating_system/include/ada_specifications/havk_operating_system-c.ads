-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PRAGMA Warnings(off, "type of argument ""*"" is unconstrained array",
   Reason => "C arrays are a pointer to the first element. This is normal.");
PRAGMA Warnings(off, "foreign caller must pass bounds explicitly",
   Reason => "This depends on the C function. This can mostly be ignored.");

-- This is the base package for my libc, which I will write in Ada for the sake
-- of doing something a bit bizarre. This essentially replaces the standard
-- "Interfaces.C" package by adding in some C99 "stdint.h" types. The package
-- is obviously only for x86-64, so the values match that fact.
-- TODO: Complete this with all the constants and types.
PACKAGE HAVK_Operating_System.C
WITH
   Preelaborate => true
IS
   CHAR_BIT    : CONSTANT := Storage_Unit;

   SCHAR_MIN   : CONSTANT := -(2**07);
   SHRT_MIN    : CONSTANT := -(2**15);
   INT_MIN     : CONSTANT := -(2**31);
   LONG_MIN    : CONSTANT := -(2**63);
   LLONG_MIN   : CONSTANT := -(2**63);

   SCHAR_MAX   : CONSTANT := 2**07 - 1;
   SHRT_MAX    : CONSTANT := 2**15 - 1;
   INT_MAX     : CONSTANT := 2**31 - 1;
   LONG_MAX    : CONSTANT := 2**63 - 1;
   LLONG_MAX   : CONSTANT := 2**63 - 1;
   INTMAX_MAX  : CONSTANT := LLONG_MAX;

   UCHAR_MAX   : CONSTANT := 2**08 - 1;
   USHRT_MAX   : CONSTANT := 2**16 - 1;
   UINT_MAX    : CONSTANT := 2**32 - 1;
   ULONG_MAX   : CONSTANT := 2**64 - 1;
   ULLONG_MAX  : CONSTANT := 2**64 - 1;
   UINTMAX_MAX : CONSTANT := ULLONG_MAX;

   -- Signed standard types.
   TYPE signed_char IS RANGE SCHAR_MIN .. SCHAR_MAX
   WITH
      Size       => CHAR_BIT,
      Convention => C;
   TYPE int         IS RANGE   INT_MIN .. INT_MAX
   WITH
      Convention => C;
   TYPE short       IS RANGE  SHRT_MIN .. SHRT_MAX
   WITH
      Convention => C;
   TYPE long        IS RANGE  LONG_MIN .. LONG_MAX
   WITH
      Convention => C;
   TYPE long_long   IS RANGE LLONG_MIN .. LLONG_MAX
   WITH
      Convention => C;

   -- Unsigned standard types.
   TYPE unsigned_char      IS MOD  UCHAR_MAX + 1
   WITH
      Size       => CHAR_BIT,
      Convention => C;
   TYPE unsigned_int       IS MOD   UINT_MAX + 1
   WITH
      Convention => C;
   TYPE unsigned_short     IS MOD  USHRT_MAX + 1
   WITH
      Convention => C;
   TYPE unsigned_long      IS MOD  ULONG_MAX + 1
   WITH
      Convention => C;
   TYPE unsigned_long_long IS MOD ULLONG_MAX + 1
   WITH
      Convention => C;

   PRAGMA Provide_Shift_Operators(unsigned_char);
   PRAGMA Provide_Shift_Operators(unsigned_int);
   PRAGMA Provide_Shift_Operators(unsigned_short);
   PRAGMA Provide_Shift_Operators(unsigned_long);
   PRAGMA Provide_Shift_Operators(unsigned_long_long);

   -- Signed "stdint.h" types.
   TYPE int8_t  IS RANGE -(2**07) .. 2**07 - 1
   WITH
      Size       => 08,
      Convention => C;
   TYPE int16_t IS RANGE -(2**15) .. 2**15 - 1
   WITH
      Size       => 16,
      Convention => C;
   TYPE int32_t IS RANGE -(2**31) .. 2**31 - 1
   WITH
      Size       => 32,
      Convention => C;
   TYPE int64_t IS RANGE -(2**63) .. 2**63 - 1
   WITH
      Size       => 64,
      Convention => C;
   TYPE intmax_t IS NEW long_long
   WITH
      Convention => C;

   -- Unsigned "stdint.h" types.
   TYPE uint8_t  IS MOD 2**08
   WITH
      Size       => 08,
      Convention => C;
   TYPE uint16_t IS MOD 2**16
   WITH
      Size       => 16,
      Convention => C;
   TYPE uint32_t IS MOD 2**32
   WITH
      Size       => 32,
      Convention => C;
   TYPE uint64_t IS MOD 2**64
   WITH
      Size       => 64,
      Convention => C;
   TYPE uintmax_t IS NEW unsigned_long_long
   WITH
      Convention => C;

   PRAGMA Provide_Shift_Operators(uint8_t);
   PRAGMA Provide_Shift_Operators(uint16_t);
   PRAGMA Provide_Shift_Operators(uint32_t);
   PRAGMA Provide_Shift_Operators(uint64_t);

   -- Miscellaneous types.
   TYPE void   IS NULL RECORD
   WITH
      Convention => C;
   TYPE char   IS NEW unsigned_char
   WITH
      Convention => C;
   TYPE size_t IS MOD 2**address'size
   WITH
      Convention => C;
   SUBTYPE bool IS boolean;

   PRAGMA Provide_Shift_Operators(size_t);

   -- Access types intended to be used like C pointers.
   TYPE void_pointer IS ACCESS ALL void
   WITH
      Convention => C;
   TYPE char_pointer IS ACCESS ALL char
   WITH
      Convention => C;

   PRAGMA No_Strict_Aliasing(void_pointer);
   PRAGMA No_Strict_Aliasing(char_pointer);

   -- Array types. These are used when you need a pointer to more than a single
   -- element. Try avoid making them zero-based or else it gets confusing.
   TYPE void_array IS ARRAY(size_t RANGE <>) OF ALIASED void_pointer
   WITH
      Component_Size    => Word_Size,
      Convention        => C;
   TYPE char_array IS ARRAY(size_t RANGE <>) OF ALIASED char
   WITH
      Component_Size    => CHAR_BIT,
      Convention        => C;

END HAVK_Operating_System.C;