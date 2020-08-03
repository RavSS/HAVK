------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

PRAGMA Compiler_Unit_Warning;

WITH Ada.Unchecked_Conversion;

PACKAGE BODY System.Storage_Elements IS

   PRAGMA Suppress (All_Checks);

   --  Conversion to/from address

   --  Note qualification below of To_Address to avoid ambiguities systems
   --  where Address is a visible integer type.

   FUNCTION To_Address IS NEW Ada.Unchecked_Conversion (storage_offset,
       address);
   FUNCTION To_Offset IS NEW Ada.Unchecked_Conversion (address,
       storage_offset);

   --  Conversion to/from integers

   --  These functions must be place first because they are inlined_always
   --  and are used and inlined in other subprograms defined in this unit.

   ----------------
   -- To_Address --
   ----------------

   FUNCTION To_Address
      (Value : integer_address)
      RETURN address
   IS
   BEGIN
      RETURN address (Value);
   END To_Address;

   ----------------
   -- To_Integer --
   ----------------

   FUNCTION To_Integer
      (Value : address)
      RETURN integer_address
   IS
   BEGIN
      RETURN integer_address (Value);
   END To_Integer;

   --  Address arithmetic

   ---------
   -- "+" --
   ---------

   FUNCTION "+"
      (Left  : address;
       Right : storage_offset)
      RETURN address
   IS
   BEGIN
      RETURN Storage_Elements.To_Address
            (To_Integer (Left) + To_Integer (To_Address (Right)));
   END "+";

   FUNCTION "+"
      (Left  : storage_offset;
       Right : address)
      RETURN address
   IS
   BEGIN
      RETURN Storage_Elements.To_Address
            (To_Integer (To_Address (Left)) + To_Integer (Right));
   END "+";

   ---------
   -- "-" --
   ---------

   FUNCTION "-"
      (Left  : address;
       Right : storage_offset)
      RETURN address
   IS
   BEGIN
      RETURN Storage_Elements.To_Address
            (To_Integer (Left) - To_Integer (To_Address (Right)));
   END "-";

   FUNCTION "-"
      (Left, Right : address)
      RETURN storage_offset
   IS
   BEGIN
      RETURN To_Offset
            (Storage_Elements.To_Address
                (To_Integer (Left) - To_Integer (Right)));
   END "-";

   -----------
   -- "mod" --
   -----------

   FUNCTION "mod"
      (Left  : address;
       Right : storage_offset)
      RETURN storage_offset
   IS
   BEGIN
      IF Right > 0
      THEN
         RETURN storage_offset (To_Integer (Left) MOD integer_address (Right));

         --  The negative case makes no sense since it is a case of a mod where
         --  the left argument is unsigned and the right argument is signed. In
         --  accordance with the (spirit of the) permission of RM 13.7.1(16),
         --  we raise CE, and also include the zero case here. Yes, the RM says
         --  PE, but this really is so obviously more like a constraint error.

      ELSE
         RAISE Constraint_Error;
      END IF;
   END "mod";

END System.Storage_Elements;
