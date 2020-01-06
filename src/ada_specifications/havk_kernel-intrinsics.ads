-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-intrinsics.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package contains x86(-64) procedures and functions that are imported
-- from external sources. See the "intrinsics.S" file in the assembly folder.
-- The reason for not including inline assembly via the machine code package
-- is due to the limitations of it in SPARK, the constraints requiring careful
-- usage, and it being easier to write assembly in GAS as opposed to in Ada.
-- It's also implementation defined, whereas importation is in the standard.
-- With contracts which are proven to be fulfilled, this should be quite safe.
-- The only drawback is inlining, which no longer seems to work. I've still
-- specified the inline aspect if I want to use link-time optimisations later.
PACKAGE HAVK_Kernel.Intrinsics
IS
   -- Does a bit test on a specific value and returns true for a set bit etc.
   -- The second argument refers to the bits in the first argument from zero.
   FUNCTION Bit_Test
     (Value : IN number;
      Bit   : IN number)
      RETURN boolean
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__bit_test",
      Pre           => Bit <= 63;

   -- Regarding x86 CPU ports and SPARK, I've decided not to try model them, as
   -- I don't see it being important to stability or even possible due to them
   -- not being memory mapped.

   -- Outputs/writes a byte to an I/O port.
   PROCEDURE Output_Byte
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__output_byte",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   -- Inputs/reads a byte from an I/O port.
   FUNCTION Input_Byte
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => NULL,
      Inline            => true,
      Import            => true,
      Convention        => Assembler,
      External_Name     => "assembly__input_byte",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Byte'result <= 16#00FF#;

   -- Halts the CPU.
   PROCEDURE Halt
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__halt";

   -- Enables interrupts.
   PROCEDURE Enable_Interrupts
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__enable_interrupts";

   -- Disables interrupts.
   PROCEDURE Disable_Interrupts
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__disable_interrupts";

   -- A hint for a spinlock. Uses GCC's internal intrinsic, as it additionally
   -- adds and provides a compiler memory barrier/fence.
   PROCEDURE Spinlock_Pause
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__builtin_ia32_pause";
END HAVK_Kernel.Intrinsics;
