------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--                               S Y S T E M                                --
--                                                                          --
--                                 S p e c                                  --
--                            (x86/64 Version)                              --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

PRAGMA Restrictions (No_Implicit_Dynamic_Code);
--  Pointers to nested subprograms are not allowed in this run time, in order
--  to prevent the compiler from building "trampolines".

PACKAGE System
IS
   PRAGMA Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada
   --  2005, this is Pure in any case (AI-362).

   PRAGMA No_Elaboration_Code_All;
   --  Allow the use of that restriction in units that WITH this unit

   TYPE Name IS (SYSTEM_NAME_GNAT);
   System_Name : CONSTANT Name := SYSTEM_NAME_GNAT;

   --  System-Dependent Named Numbers

   Min_Int               : CONSTANT := Long_Long_Integer'First;
   Max_Int               : CONSTANT := Long_Long_Integer'Last;

   Max_Binary_Modulus    : CONSTANT := 2 ** Long_Long_Integer'Size;
   Max_Nonbinary_Modulus : CONSTANT := 2 ** Integer'Size - 1;

   Max_Base_Digits       : CONSTANT := Long_Long_Float'Digits;
   Max_Digits            : CONSTANT := Long_Long_Float'Digits;

   Max_Mantissa          : CONSTANT := 63;
   Fine_Delta            : CONSTANT := 2.0 ** (-Max_Mantissa);

   Tick                  : CONSTANT := 0.0;

   --  Storage-related Declarations

   Storage_Unit : CONSTANT := 8;
   Word_Size    : CONSTANT := 64;
   Memory_Size  : CONSTANT := 2 ** 64;

   TYPE address IS MOD Memory_Size;
   PRAGMA Provide_Shift_Operators(address);
   Null_Address : CONSTANT address := 0;

   --  Address comparison

   FUNCTION "<"  (Left, Right : address) RETURN boolean;
   FUNCTION "<=" (Left, Right : address) RETURN boolean;
   FUNCTION ">"  (Left, Right : address) RETURN boolean;
   FUNCTION ">=" (Left, Right : address) RETURN boolean;
   FUNCTION "="  (Left, Right : address) RETURN boolean;

   PRAGMA Import (Intrinsic, "<");
   PRAGMA Import (Intrinsic, "<=");
   PRAGMA Import (Intrinsic, ">");
   PRAGMA Import (Intrinsic, ">=");
   PRAGMA Import (Intrinsic, "=");

   --  Other System-Dependent Declarations

   TYPE bit_order IS (high_order_first, low_order_first);
   Default_Bit_Order : CONSTANT bit_order := low_order_first;
   PRAGMA Warnings (off, Default_Bit_Order); -- kill constant condition warning

   --  Priority-related Declarations (RM D.1)

   Max_Priority           : CONSTANT positive := 30;
   Max_Interrupt_Priority : CONSTANT positive := 31;

   SUBTYPE any_priority       IS integer      RANGE  0 .. 31;
   SUBTYPE priority           IS any_priority RANGE  0 .. 30;
   SUBTYPE interrupt_priority IS any_priority RANGE 31 .. 31;

   Default_Priority : CONSTANT Priority := 15;

PRIVATE
   Run_Time_Name : CONSTANT string := "HAVK Runtime System";

   --------------------------------------
   -- System Implementation Parameters --
   --------------------------------------

   --  These parameters provide information about the target that is used
   --  by the compiler. They are in the private part of System, where they
   --  can be accessed using the special circuitry in the Targparm unit
   --  whose source should be consulted for more detailed descriptions
   --  of the individual switch values.
   --  RavSS: These are specially parsed. Case sensitive enumeration values.
   Atomic_Sync_Default       : CONSTANT boolean := False;
   Backend_Divide_Checks     : CONSTANT boolean := False;
   Backend_Overflow_Checks   : CONSTANT boolean := True;
   Command_Line_Args         : CONSTANT boolean := False;
   Configurable_Run_Time     : CONSTANT boolean := True;
   Denorm                    : CONSTANT boolean := True;
   Duration_32_Bits          : CONSTANT boolean := False;
   Exit_Status_Supported     : CONSTANT boolean := False;
   Fractional_Fixed_Ops      : CONSTANT boolean := False;
   Frontend_Layout           : CONSTANT boolean := False;
   Machine_Overflows         : CONSTANT boolean := False;
   Machine_Rounds            : CONSTANT boolean := True;
   Preallocated_Stacks       : CONSTANT boolean := True;
   Signed_Zeros              : CONSTANT boolean := True;
   Stack_Check_Default       : CONSTANT boolean := True;
   Stack_Check_Probes        : CONSTANT boolean := True;
   Stack_Check_Limits        : CONSTANT boolean := False;
   Support_Aggregates        : CONSTANT boolean := True;
   Support_Atomic_Primitives : CONSTANT boolean := True;
   Support_Composite_Assign  : CONSTANT boolean := True;
   Support_Composite_Compare : CONSTANT boolean := True;
   Support_Long_Shifts       : CONSTANT boolean := True;
   Always_Compatible_Rep     : CONSTANT boolean := True;
   Suppress_Standard_Library : CONSTANT boolean := True;
   Use_Ada_Main_Program_Name : CONSTANT boolean := True;
   Frontend_Exceptions       : CONSTANT boolean := False;
   ZCX_By_Default            : CONSTANT boolean := True;

end System;
