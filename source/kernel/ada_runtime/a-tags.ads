------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                             A D A . T A G S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
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

--  This is the HI-E version of this file. It provides full object oriented
--  semantics (including dynamic dispatching and support for abstract interface
--  types), assuming that tagged types are declared at the library level. Some
--  functionality has been removed in order to simplify this run-time unit.
--  Compared to the full version of this package, the following subprograms
--  have been removed:

--     Internal_Tag, Register_Tag, Descendant_Tag, Is_Descendant_At_Same_Level:
--     These subprograms are used for cross-referencing the external and
--     internal representation of tags. The implementation of these routines
--     was considered neither simple nor esential for this restricted run-time,
--     and hence these functions were removed.

--     Get_Entry_Index, Get_Offset_Index, Get_Prim_Op_Kind, Get_Tagged_Kind,
--     SSD, Set_Entry_Index, Set_Prim_Op_Kind, OSD: They are used with types
--     that implement limited interfaces and are only invoked when there are
--     selective waits and ATC's where the trigger is a call to an interface
--     operation. These functions have been removed because selective waits
--     and ATC's are not supported by the restricted run-time.

--     Displace, IW_Membership, Offset_To_Top, Set_Dynamic_Offset_To_Top,
--     Base_Address, Register_Interface_Offset: They are used with extended
--     support for interface types that is not part of the zfp runtime
--     (membership test applied to interfaces, tagged types with variable
--     size components covering interfaces, explicit dereference through
--     access to interfaces, and unchecked deallocation through access to
--     interfaces).

--     The operations in this package provide the guarantee that all
--     dispatching calls on primitive operations of tagged types and
--     interfaces take constant time (in terms of source lines executed),
--     that is to say, the cost of these calls is independent of the number
--     of primitives of the type or interface, and independent of the number
--     of ancestors or interface progenitors that a tagged type may have.

WITH System;
WITH System.Storage_Elements;

PACKAGE Ada.Tags IS
   PRAGMA Preelaborate;
   --  In accordance with Ada 2005 AI-362

   TYPE tag IS PRIVATE;
   PRAGMA Preelaborable_Initialization (Tag);
   No_Tag : CONSTANT Tag;
   FUNCTION Expanded_Name
      (T : Tag)
       RETURN String;
   FUNCTION External_Tag
      (T : Tag)
       RETURN String;
   FUNCTION Parent_Tag
      (T : Tag)
       RETURN Tag;
   PRAGMA Ada_05 (Parent_Tag);
   Tag_Error : EXCEPTION;

PRIVATE
   --  Structure of the GNAT Primary Dispatch Table

   --          +--------------------+
   --          |    Predef_Prims ---------------------------> +------------+
   --          +--------------------+                         |  table of  |
   --          |Typeinfo_Ptr/TSD_Ptr --> Type Specific Data   | predefined |
   --  Tag --> +--------------------+ +-------------------+ | primitives |
   --          |      table of      |  | inheritance depth |  +------------+
   --          :   primitive ops    :  +-------------------+
   --          |      pointers      |  |   access level    |
   --          +--------------------+  +-------------------+
   --                                  |     alignment     |
   --                                  +-------------------+
   --                                  |   expanded name   |
   --                                  +-------------------+
   --                                  |   external tag    |
   --                                  +-------------------+
   --                                  |   hash table link |
   --                                  +-------------------+
   --                                  |   transportable   |
   --                                  +-------------------+
   --                                  | needs finalization|
   --                                  +-------------------+
   --                                  | table of          |
   --                                  :    ancestor       :
   --                                  |       tags        |
   --                                  +-------------------+

   --  The runtime information kept for each tagged type is separated into
   --  three objects: the Dispatch Table of predefined primitives, the dispatch
   --  table of user-defined primitives and the Type_Specific_Data record.

   PACKAGE SSE RENAMES System.Storage_Elements;
   SUBTYPE cstring IS String (Positive);

   TYPE cstring_ptr IS ACCESS ALL Cstring;
   PRAGMA No_Strict_Aliasing (Cstring_Ptr);

   TYPE tag_table IS ARRAY (Natural RANGE <>) OF Tag;

   TYPE prim_ptr IS ACCESS PROCEDURE;

   TYPE address_array IS ARRAY (Positive RANGE <>) OF Prim_Ptr;
   SUBTYPE dispatch_table IS Address_Array (1 .. 1);
   --  Used by GDB to identify the _tags and traverse the run-time structure
   --  associated with tagged types. For compatibility with older versions of
   --  gdb, its name must not be changed.

   TYPE tag IS ACCESS ALL Dispatch_Table;
   PRAGMA No_Strict_Aliasing (Tag);

   TYPE interface_tag IS ACCESS ALL Dispatch_Table;
   No_Tag : CONSTANT Tag := NULL;
   --  The expander ensures that Tag objects reference the Prims_Ptr component
   --  of the wrapper.

   TYPE tag_ptr IS ACCESS ALL Tag;
   PRAGMA No_Strict_Aliasing (Tag_Ptr);

   TYPE offset_to_top_ptr IS ACCESS ALL SSE.Storage_Offset;
   PRAGMA No_Strict_Aliasing (Offset_To_Top_Ptr);

   TYPE type_specific_data (Idepth : Natural) IS RECORD
      --  Inheritance Depth Level: Used to implement the membership test
      --  associated with single inheritance of tagged types in constant-time.
      --  It also indicates the size of the Tags_Table component.

      Access_Level : Natural;
      --  Accessibility level required to give support to Ada 2005 nested
      --  type extensions. This feature allows safe nested type extensions by
      --  shifting the accessibility checks to certain operations, rather than
      --  being enforced at the type declaration. In particular, by performing
      --  run-time accessibility checks on class-wide allocators, class-wide
      --  function return, and class-wide stream I/O, the danger of objects
      --  outliving their type declaration can be eliminated (Ada 2005: AI-344)

      Alignment     : Natural;
      Expanded_Name : Cstring_Ptr;
      External_Tag  : Cstring_Ptr;
      --  Components used to support to the Ada.Tags subprograms in ARM 3.9

      --  Note: Expanded_Name is referenced by GDB to determine the actual name
      --  of the tagged type. Its requirements are: 1) it must have this exact
      --  name, and 2) its contents must point to a C-style Nul terminated
      --  string containing its expanded name. GDB has no requirement on a
      --  given position inside the record.

      Transportable : Boolean;
      --  Used to check RM E.4(18), set for types that satisfy the requirements
      --  for being used in remote calls as actuals for classwide formals or as
      --  return values for classwide functions.

      Needs_Finalization : Boolean;
      --  Used to dynamically check whether an object is controlled or not

      Tags_Table : Tag_Table (0 .. Idepth);
      --  Table of ancestor tags. Its size actually depends on the inheritance
      --  depth level of the tagged type.
   END RECORD;

   TYPE type_specific_data_ptr IS ACCESS ALL Type_Specific_Data;
   PRAGMA No_Strict_Aliasing (Type_Specific_Data_Ptr);

   TYPE dispatch_table_wrapper (Num_Prims : Natural) IS RECORD
      Predef_Prims : System.Address;
      --  Pointer to the dispatch table of predefined Ada primitives

      --  According to the C++ ABI the components Offset_To_Top and TSD are
      --  stored just "before" the dispatch table (that is, the Prims_Ptr
      --  table), and they are referenced with negative offsets referring to
      --  the base of the dispatch table. The _Tag (or the VTable_Ptr in C++
      --  terminology) must point to the base of the virtual table, just after
      --  these components, to point to the Prims_Ptr table.

      Offset_To_Top : SSE.Storage_Offset;
      TSD           : System.Address;
      Prims_Ptr     : Address_Array (1 .. Num_Prims);
      --  The size of the Prims_Ptr array actually depends on the tagged type
      --  to which it applies. For each tagged type, the expander computes the
      --  actual array size, allocates the Dispatch_Table record accordingly.
   END RECORD;
      --  The following type declaration is used by the compiler when the
      --  program is compiled with restriction No_Dispatching_Calls

   TYPE no_dispatch_table_wrapper IS RECORD
      NDT_TSD       : System.Address;
      NDT_Prims_Ptr : Natural;
   END RECORD;
   DT_Predef_Prims_Size : CONSTANT SSE.Storage_Count :=
      SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the Predef_Prims field of the Dispatch_Table

   DT_Offset_To_Top_Size : CONSTANT SSE.Storage_Count :=
      SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the Offset_To_Top field of the Dispatch Table

   DT_Typeinfo_Ptr_Size : CONSTANT SSE.Storage_Count :=
      SSE.Storage_Count (1 * (Standard'Address_Size / System.Storage_Unit));
   --  Size of the Typeinfo_Ptr field of the Dispatch Table

   USE TYPE System.Storage_Elements.Storage_Offset;
   DT_Offset_To_Top_Offset : CONSTANT SSE.Storage_Count :=
      DT_Typeinfo_Ptr_Size + DT_Offset_To_Top_Size;
   DT_Predef_Prims_Offset : CONSTANT SSE.Storage_Count :=
      DT_Typeinfo_Ptr_Size + DT_Offset_To_Top_Size + DT_Predef_Prims_Size;
   --  Offset from Prims_Ptr to Predef_Prims component

   Max_Predef_Prims : CONSTANT Positive := 10;
   --  Number of reserved slots for predefined ada primitives: Size, Read,
   --  Write, Input, Output, "=", assignment, deep adjust, deep finalize,
   --  and Put_Image. The compiler checks that this value is correct.

   SUBTYPE predef_prims_table IS Address_Array (1 .. Max_Predef_Prims);

   TYPE predef_prims_table_ptr IS ACCESS Predef_Prims_Table;
   PRAGMA No_Strict_Aliasing (Predef_Prims_Table_Ptr);

   TYPE addr_ptr IS ACCESS System.Address;
   PRAGMA No_Strict_Aliasing (Addr_Ptr);
END Ada.Tags;
