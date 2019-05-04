PACKAGE HAVK_Kernel IS
   -- I am going to define my own types instead of using Interfaces,
   -- so that way, I can be sure the compiler is not doing something
   -- other than what I want it to do for the current x86-64 environment.

   TYPE u64 IS MOD 2 ** 64;
   TYPE u64s IS ARRAY(u64 RANGE <>) OF u64;
   PRAGMA Pack(u64s);

   TYPE u32 IS MOD 2 ** 32;
   TYPE u32s IS ARRAY(u64 RANGE <>) OF u32;
   PRAGMA Pack(u32s);

   TYPE u8 IS MOD 2 ** 8;
   TYPE u8s IS ARRAY(u64 RANGE <>) OF u8;
   PRAGMA Pack(u8s);

   TYPE s32 IS RANGE -(2 ** 32) .. +(2 ** 32);
   TYPE s32s IS ARRAY(u64 RANGE <>) OF s32;
   PRAGMA Pack(s32s);

   TYPE s64 IS RANGE -(2 ** 63) .. +(2 ** 63 - 1);
   TYPE s64s IS ARRAY(u64 RANGE <>) OF s64;
   PRAGMA Pack(s64s);

   TYPE str IS ARRAY(u64 RANGE <>) OF character;
END HAVK_Kernel;
