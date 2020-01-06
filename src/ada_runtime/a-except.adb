------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       A D A . E X C E P T I O N S                        --
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

PACKAGE BODY Ada.Exceptions IS

   Empty_C_String : ALIASED CONSTANT string :=
      (1 => character'val (0));

      ---------------------
      -- Raise_Exception --
      ---------------------

   PROCEDURE Raise_Exception
      (E       : exception_id;
       Message : string := "")
   IS
      PRAGMA Unreferenced (E);

      PROCEDURE Last_Chance_Handler
         (Msg  : System.address;
          Line : integer);
      PRAGMA Import (C, Last_Chance_Handler, "__gnat_last_chance_handler");
      PRAGMA No_Return (Last_Chance_Handler);

   BEGIN
      --  The last chance handler is expecting a C string as Msg parameter,
      --  which means a NUL terminated string.
      --  We thus need to ensure here that this is always the case.

      IF Message'Length = 0
      THEN
         --  Use the Empty_C_String constant
         Last_Chance_Handler (Empty_C_String'Address, 0);

      ELSE
         --  Check null termination.
         --  Literals are supposed to be statically generated with a trailing
         --  NUL character, but in this generic Raise_Exception case we
         --  can't assume this is always true. So in case we don't have such
         --  character to terminate the C string representation, we need to
         --  copy the string on the stack and add it.
         DECLARE
            S : string (Message'First .. Message'Last + 1) WITH
               Import,
               Address => Message'Address;
         BEGIN
            IF S (S'Last) = character'val (0)
            THEN
               --  We have a proper C string representation
               Last_Chance_Handler (Message'Address, 0);
            ELSE
               DECLARE
                  --  Dynamic stack allocated string: may be problematic in
                  --  case of Storage Error due to a stack overflow, but it's
                  --  ok here as those Storage_Error have an empty message.
                  C_Message : ALIASED string := S;
               BEGIN
                  C_Message (C_Message'Last) := character'val (0);
                  Last_Chance_Handler (C_Message'Address, 0);
               END;
            END IF;
         END;
      END IF;
   END Raise_Exception;

END Ada.Exceptions;
