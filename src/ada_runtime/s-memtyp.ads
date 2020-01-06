------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                   S Y S T E M .  M E M O R Y _ T Y P E S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--              Copyright (C) 2017-2019, Free Software Foundation, Inc.     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

WITH Ada.Unchecked_Conversion;

PACKAGE System.Memory_Types IS
   PRAGMA No_Elaboration_Code_All;
   PRAGMA Preelaborate;

   TYPE size_t IS MOD 2**Standard'Address_Size;
   --  The type corresponding to size_t in C. We cannot reuse the one defined
   --  in Interfaces.C as we want this package not to have any elaboration
   --  code.

   TYPE IA IS MOD System.memory_size;
   --  The type used to provide the actual desired operations

   FUNCTION To_IA IS NEW Ada.Unchecked_Conversion (address, ia);
   --  The operations are implemented by unchecked conversion to type IA,
   --  followed by doing the intrinsic operation on the IA values, followed
   --  by converting the result back to type Address.

   TYPE Byte IS MOD 2**8;
   FOR byte'Size USE 8;
   --  Byte is the storage unit

   TYPE Byte_Ptr IS ACCESS byte;
   --  Access to a byte

   FUNCTION To_Byte_Ptr IS NEW Ada.Unchecked_Conversion (ia, byte_ptr);
   --  Conversion between an integer address and access to byte

   Byte_Unit : CONSTANT := 1;
   --  Number of storage unit in a byte

   TYPE Word IS MOD 2**System.word_size;
   FOR word'Size USE System.word_size;
   --  Word is efficiently loaded and stored by the processor, but has
   --  alignment constraints.

   TYPE Word_Ptr IS ACCESS word;
   --  Access to a word.

   FUNCTION To_Word_Ptr IS NEW Ada.Unchecked_Conversion (ia, word_ptr);
   --  Conversion from an integer adddress to word access

   Word_Unit : CONSTANT := word'Size / storage_unit;
   --  Number of storage unit per word
END System.Memory_Types;
