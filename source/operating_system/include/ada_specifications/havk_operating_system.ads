-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system.ads                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   System;
USE
   System;

-- This package and its child packages make up a set of libraries that are used
-- by user tasks/programs. All user programs are advised to link with it.
PACKAGE HAVK_Operating_System
WITH
   Pure => true
IS
   TYPE number IS MOD 2**64
   WITH
      Size => 64;
   PRAGMA Provide_Shift_Operators(number);

   TYPE addressable_length IS
     (byte_length,
      word_length,
      doubleword_length,
      quadword_length);
   FOR addressable_length USE
     (byte_length       => 08,
      word_length       => 16,
      doubleword_length => 32,
      quadword_length   => 64);

   TYPE bit        IS MOD 2**1
   WITH
      Size        => 1,
      Object_Size => byte_length'enum_rep;
   TYPE byte       IS MOD 2**byte_length'enum_rep
   WITH
      Size        => byte_length'enum_rep,
      Object_Size => byte_length'enum_rep;
   TYPE word       IS MOD 2**word_length'enum_rep
   WITH
      Size        => word_length'enum_rep,
      Object_Size => word_length'enum_rep;
   TYPE doubleword IS MOD 2**doubleword_length'enum_rep
   WITH
      Size        => doubleword_length'enum_rep,
      Object_Size => doubleword_length'enum_rep;
   TYPE quadword   IS MOD 2**quadword_length'enum_rep
   WITH
      Size        => quadword_length'enum_rep,
      Object_Size => quadword_length'enum_rep;

   TYPE bits        IS ARRAY(number RANGE <>) OF bit
   WITH
      Component_Size          => 1,
      Default_Component_Value => bit'first;
   TYPE bytes       IS ARRAY(number RANGE <>) OF ALIASED byte
   WITH
      Component_Size          => byte_length'enum_rep,
      Default_Component_Value => byte'first;
   TYPE words       IS ARRAY(number RANGE <>) OF ALIASED word
   WITH
      Component_Size          => word_length'enum_rep,
      Default_Component_Value => word'first;
   TYPE doublewords IS ARRAY(number RANGE <>) OF ALIASED doubleword
   WITH
      Component_Size          => doubleword_length'enum_rep,
      Default_Component_Value => doubleword'first;
   TYPE quadwords   IS ARRAY(number RANGE <>) OF ALIASED quadword
   WITH
      Component_Size          => quadword_length'enum_rep,
      Default_Component_Value => quadword'first;
   TYPE addresses   IS ARRAY(number RANGE <>) OF ALIASED address
   WITH
      Component_Size          => address'size,
      Default_Component_Value => address'first;

   TYPE error IS
     (no_error,         -- The operation succeeded.
      attempt_error,    -- The respective attempt failed; try again.
      memory_error,     -- An error to do with memory i.e. allocation.
      permission_error, -- Invalid permissions to do something.
      size_error,       -- An incorrect size or length was specified.
      index_error,      -- A wrong index was specified.
      format_error,     -- The format of a piece of data is incorrect.
      hardware_error,   -- Used for when there's any hardware issues.
      path_error)       -- A path is invalid or the file/directory is missing.
   WITH
      Default_Value => no_error,
      Convention    => C;

   NUL : CONSTANT character := character'val(00);
   BS  : CONSTANT character := character'val(08);
   HT  : CONSTANT character := character'val(09);
   LF  : CONSTANT character := character'val(10);
   CR  : CONSTANT character := character'val(13);

   FUNCTION Source_Location
      RETURN string
   WITH
      Import     => true,
      Convention => Intrinsic;

END HAVK_Operating_System;