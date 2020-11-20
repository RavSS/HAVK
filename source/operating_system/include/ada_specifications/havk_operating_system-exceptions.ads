-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-exceptions.ads                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE HAVK_Operating_System.Exceptions
WITH
   Preelaborate => true
IS
   PROCEDURE Last_Chance_Handler
     (String_Address : IN address;
      Line           : IN natural)
   WITH
      No_Return     => true,
      Export        => true,
      External_Name => "__gnat_last_chance_handler";

   PROCEDURE Stack_Check_Failure
   WITH
      No_Return     => true,
      Export        => true,
      External_Name => "__stack_chk_fail";

PRIVATE
   -- This is used by the stack protection GCC generates. The value should be
   -- 64 bits.
   -- TODO: This will have to be better randomised at either compile-time or
   -- runtime.
   Stack_Canary : ALIASED number := 16#DEADC0DE#
   WITH
      Volatile      => true,
      Export        => true,
      External_Name => "__stack_chk_guard";

END HAVK_Operating_System.Exceptions;