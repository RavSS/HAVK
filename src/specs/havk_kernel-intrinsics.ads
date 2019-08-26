-- This package contains x86(-64) procedures and functions utilizing
-- inline assembly instructions for whatever random purpose.
PACKAGE HAVK_Kernel.Intrinsics
IS
   -- For some reason, shift operators are not built into the language
   -- itself. They're in the Interfaces package, and even then, GNAT
   -- has to provide its own specific "intrinsic" version of it.
   -- I'll just provide my own slower yet more concise functions instead.
   -- One change to note is that the shifts are not overloaded (unlike
   -- GNAT's ones), which I think means they can be used with SPARK, but then
   -- again they utilize inline assembly, which is not allowed in SPARK...

   -- Shift a value to the left (<<).
   FUNCTION SHL(
      Value  : IN num;
      Shifts : IN num)
   RETURN num
   WITH
      Inline_Always => true;

   -- Shift a value to the right (>>).
   FUNCTION SHR(
      Value  : IN num;
      Shifts : IN num)
   RETURN num
   WITH
      Inline_Always => true;

   -- Does a bit test on a specific value and returns true for a set bit etc.
   -- Note that the BT instruction might be slow when factoring in
   -- CPU micro-operations, and it's slow in itself to call a function, but
   -- it's much clearer to use it instead of messing around with GNAT's shifts
   -- or my own handwritten assembly shifts and use AND operators with them.
   FUNCTION BT(
      Value  : IN num;
      Bit    : IN num)
   RETURN boolean
   WITH
      Inline_Always => true;

   -- Outputs a byte to an IO port.
   PROCEDURE OUTB(
      Port   : IN num;
      Value  : IN num)
   WITH
      Inline_Always => true;

   -- Reads a byte from an IO port.
   FUNCTION INB(
      Port   : IN num)
   RETURN num
   WITH
      Inline_Always => true;

   -- Halts the CPU.
   PROCEDURE HLT
   WITH
      Inline_Always => true;

   -- Enables interrupts.
   PROCEDURE STI
   WITH
      Inline_Always => true;

   -- Disables interrupts.
   PROCEDURE CLI
   WITH
      Inline_Always => true;
END HAVK_Kernel.Intrinsics;
