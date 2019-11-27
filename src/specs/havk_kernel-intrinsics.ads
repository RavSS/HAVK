-- This package contains x86(-64) procedures and functions utilizing
-- inline assembly instructions for whatever random purpose.
PACKAGE HAVK_Kernel.Intrinsics
IS
   -- Shift a value to the left (<<). Alternate to `Shift_Left()`.
   FUNCTION SHL(
      Value  : IN num;
      Shifts : IN num)
   RETURN num
   WITH
      Inline => true,
      Pre    => Shifts <= 63;

   -- Shift a value to the right (>>). Alternate to `Shift_Right()`.
   FUNCTION SHR(
      Value  : IN num;
      Shifts : IN num)
   RETURN num
   WITH
      Inline => true,
      Pre    => Shifts <= 63;

   -- Does a bit test on a specific value and returns true for a set bit etc.
   -- The second argument refers to the bits in the first argument from zero.
   -- Note that the BT instruction might be slow when factoring in
   -- CPU micro-operations, and it's slow in itself to call a function, but
   -- it's much clearer to use it instead of messing around with GNAT's shifts
   -- or my own handwritten assembly shifts and use AND operators with them.
   FUNCTION BT(
      Value  : IN num;
      Bit    : IN num)
   RETURN boolean
   WITH
      Inline => true,
      Pre    => Bit <= 63;

   -- Outputs a byte to an IO port.
   PROCEDURE OUTB(
      Port   : IN num;
      Value  : IN num)
   WITH
      Inline => true,
      Pre    => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   -- Reads a byte from an IO port.
   FUNCTION INB(
      Port   : IN num)
   RETURN num
   WITH
      Inline => true,
      Pre    => Port       <= 16#FFFF#,
      Post   => INB'result <=   16#FF#;

   -- Halts the CPU.
   PROCEDURE HLT
   WITH
      Inline => true;

   -- Enables interrupts.
   PROCEDURE STI
   WITH
      Inline => true;

   -- Disables interrupts.
   PROCEDURE CLI
   WITH
      Inline => true;

   -- Spinlock hint. Uses GCC's internal intrinsic, as it additionally
   -- adds and provides a compiler memory barrier/fence.
   PROCEDURE PAUSE
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__builtin_ia32_pause";
END HAVK_Kernel.Intrinsics;
