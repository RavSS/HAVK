-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-utility.ads                      --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Operating_System.Utility
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      CPU_Port_State
      WITH
         External
   )
IS
   FUNCTION Bit_Test
     (Value : IN number;
      Bit   : IN number)
      RETURN boolean
   WITH
      Inline => true,
      Pre    => Bit <= 63;

   SUBTYPE image_string IS string(1 .. 64);
   FUNCTION Image
     (Value   : IN number;
      Base    : IN number   := 10;
      Padding : IN positive := 01)
      RETURN image_string
   WITH
      Pre => Base IN 10 | 16 AND THEN
             Padding <= image_string'last;

   FUNCTION Byte_Swap
     (Value : IN number)
      RETURN number
   WITH
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__builtin_bswap64";

   PROCEDURE Output_Byte
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Import        => true,
      Convention    => C,
      External_Name => "assembly__output_byte",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   PROCEDURE Output_Word
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Import        => true,
      Convention    => C,
      External_Name => "assembly__output_word",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FFFF#;

   FUNCTION Input_Byte
     (Port : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Import            => true,
      Convention        => C,
      External_Name     => "assembly__input_byte",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Byte'result <= 16#00FF#;

   FUNCTION Input_Word
     (Port : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Import            => true,
      Convention        => C,
      External_Name     => "assembly__input_word",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Word'result <= 16#FFFF#;

END HAVK_Operating_System.Utility;
