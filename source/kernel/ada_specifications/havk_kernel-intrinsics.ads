-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-intrinsics.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package contains x86(-64) procedures and functions that are imported
-- from external sources or are small and universal enough to do a variety of
-- goals. See the "intrinsics.S" file in the assembly folder. The reason for
-- not including inline assembly via the machine code package is due to SPARK
-- disallowing it, constraints being hard to manage than GAS, and importation
-- being in the standard. Compile-time inlining is however not functional.
PACKAGE HAVK_Kernel.Intrinsics
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
     (CPU_Port_State
      WITH
         External => (Async_Readers, Async_Writers,
                         Effective_Reads, Effective_Writes)),
     (CPU_MSR_State
      WITH
         External => (Async_Readers, Async_Writers,
                         Effective_Reads, Effective_Writes))
   )
IS
   -- For usage with the special registers. MSRs (indices) are only 32-bits.
   SUBTYPE model_specific_register IS number RANGE 0 .. 16#FFFFFFFF#;

   -- Does a bit test on a specific value and returns true for a set bit etc.
   -- The second argument refers to the bits in the first argument from zero.
   FUNCTION Bit_Test
     (Value : IN number;
      Bit   : IN number)
      RETURN boolean
   WITH
      Inline => true,
      Pre    => Bit <= 63;

   -- Outputs/writes a byte to an I/O port.
   PROCEDURE Output_Byte
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__output_byte",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   -- Outputs/writes a word to an I/O port.
   PROCEDURE Output_Word
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__output_word",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FFFF#;

   -- Writes a 64-bit value to a model-specific register.
   PROCEDURE Write_MSR
     (MSR   : IN model_specific_register;
      Value : IN number)
   WITH
      Global        => (Output => CPU_MSR_State),
      Inline        => true,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__write_model_specific_register";

   -- Inputs/reads a byte from an I/O port.
   FUNCTION Input_Byte
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Inline            => true,
      Import            => true,
      Convention        => Assembler,
      External_Name     => "assembly__input_byte",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Byte'result <= 16#00FF#;

   -- Inputs/reads a word from an I/O port.
   FUNCTION Input_Word
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Inline            => true,
      Import            => true,
      Convention        => Assembler,
      External_Name     => "assembly__input_word",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Word'result <= 16#FFFF#;

   -- Reads a 64-bit value from a model-specific register.
   FUNCTION Read_MSR
     (MSR   : IN model_specific_register)
     RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_MSR_State),
      Inline            => true,
      Import            => true,
      Convention        => Assembler,
      External_Name     => "assembly__read_model_specific_register";

   -- Swaps the byte order (endianness) of a 64-bit value around. This uses a
   -- GCC internal intrinsic function, but it can be replaced with just three
   -- instructions. There's other sizes for `__builtin_bswap*()` as well, but
   -- I would need to create new types to match the built-in's prototype.
   -- Instead, just do right shifts e.g. `Shift_Right(Swapped, 32)` for an
   -- equivalent of `__builtin_bswap32()` and so on.
   FUNCTION Byte_Swap
     (Value : IN number)
      RETURN number
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__builtin_bswap64";

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

   -- Forces both a compiler fence and a memory fence. Should be equivalent
   -- to the `MFENCE` instruction without needing a full assembly routine.
   PROCEDURE Memory_Fence
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__sync_synchronize";

END HAVK_Kernel.Intrinsics;
