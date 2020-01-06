-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-exceptions.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Kernel.Exceptions
IS
   PROCEDURE Last_Chance_Handler
     (Source_Location : IN address;
      Line            : IN integer)
   WITH
      Export     => true,
      No_Return  => true,
      Convention => Ada,
      Link_Name  => "__gnat_last_chance_handler";

   -- Just a wrapper for the last chance handler for now, as a
   -- symbol is created for the stack check fail if stack protection
   -- is enabled during compilation.
   PROCEDURE Stack_Smash_Handler
   WITH
      Export     => true,
      No_Return  => true,
      Convention => Ada,
      Link_Name  => "__stack_chk_fail";

   -- A way to raise a fatal exception without using actual Ada exceptions.
   -- For now, just use this for unimplemented features etc. Only use this very
   -- sparingly, like if the hardware doesn't do what we want it to do e.g. no
   -- PS/2 input or a UEFI boot service bit block transfer only framebuffer.
   PROCEDURE Tears_In_Rain
     (Message : IN string;
      File    : IN string;
      Line    : IN integer)
   WITH
      No_Return => true;
END HAVK_Kernel.Exceptions;
