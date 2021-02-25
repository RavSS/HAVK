-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-exceptions.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Kernel.Exceptions
WITH
   Preelaborate => true
IS
   -- If an error occurs during elaboration, then make an attempt to
   -- signify that fact.
   Elaborated : boolean := false
   WITH
      Volatile       => true,
      Linker_Section => ".isolated_bss";

PRIVATE
   Exceptions_Tag : CONSTANT string := "EXCPTINS";

   -- This is is entered upon any and all Ada exceptions. We simply crash.
   PROCEDURE Last_Chance_Handler
     (String_Pointer : IN address;
      Line           : IN natural)
   WITH
      Export         => true,
      No_Return      => true,
      External_Name  => "__gnat_last_chance_handler",
      Linker_Section => ".isolated_text";

   -- Just a wrapper for the last chance handler for now, as a
   -- symbol is created for the stack check fail if stack protection
   -- is enabled during compilation.
   PROCEDURE Stack_Smash_Handler
   WITH
      Export         => true,
      No_Return      => true,
      External_Name  => "__stack_chk_fail",
      Linker_Section => ".isolated_text";

END HAVK_Kernel.Exceptions;
