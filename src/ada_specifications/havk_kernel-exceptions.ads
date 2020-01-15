-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-exceptions.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Kernel.Exceptions
IS
   -- This is is entered upon any and all Ada exceptions. We simply crash.
   PROCEDURE Last_Chance_Handler
     (Source_Location : IN address;
      Line            : IN integer)
   WITH
      Export        => true,
      No_Return     => true,
      External_Name => "__gnat_last_chance_handler";

   -- Just a wrapper for the last chance handler for now, as a
   -- symbol is created for the stack check fail if stack protection
   -- is enabled during compilation.
   PROCEDURE Stack_Smash_Handler
   WITH
      Export        => true,
      No_Return     => true,
      External_Name => "__stack_chk_fail";

   -- If an error occurs during elaboration, then make an attempt to
   -- signify that fact.
   Elaborated : boolean := false
   WITH
      Volatile => true;

END HAVK_Kernel.Exceptions;
