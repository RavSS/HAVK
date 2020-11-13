-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system.ads                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   System,
   Ada.Unchecked_Conversion;
USE
   System;

-- A package with some simple types for general style unity between programs
-- and definitions for system calls. Keep this file up to date with the kernel
-- itself.
-- TODO: This package (especially its body) is very rushed and needs
-- documentation etc.
PACKAGE HAVK_Operating_System
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      CPU_Port_State
      WITH
         External
   )
IS
   TYPE number IS MOD 2**64
   WITH
      Size        => 64,
      Object_Size => 64;
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

   TYPE operation IS
     (null_operation,
      exit_task_operation,
      receive_message_operation,
      send_message_operation,
      identify_task_operation,
      load_elf_operation,
      heap_increase_operation,
      yield_operation,
      log_operation,
      irq_statistics_operation,
      io_port_operation,
      buffer_operation,
      framebuffer_access_operation)
   WITH
      Convention => C;

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

   TYPE general_register IS MOD 2**64
   WITH
      Size        => 64,
      Object_Size => 64,
      Annotate    => (GNATprove, No_Wrap_Around);
   PRAGMA Provide_Shift_Operators(general_register);

   PRAGMA Warnings(off, "no component clause given for ""Data_Length"" *",
      Reason => "It's not a real component.");
   TYPE XMM_register
     (Data_Length : addressable_length := quadword_length)
   IS RECORD
      CASE
         Data_Length
      IS
         WHEN byte_length       => XMM_Bytes       :       bytes(1 .. 16);
         WHEN word_length       => XMM_Words       :       words(1 .. 08);
         WHEN doubleword_length => XMM_Doublewords : doublewords(1 .. 04);
         WHEN quadword_length   => XMM_Quadwords   :   quadwords(1 .. 02);
      END CASE;
   END RECORD
   WITH
      Unchecked_Union => true,
      Object_Size     => 128;
   FOR XMM_register USE RECORD
      XMM_Bytes       AT 0 RANGE 0 .. 127;
      XMM_Words       AT 0 RANGE 0 .. 127;
      XMM_Doublewords AT 0 RANGE 0 .. 127;
      XMM_Quadwords   AT 0 RANGE 0 .. 127;
   END RECORD;

   TYPE XMM_registers IS ARRAY(number RANGE 0 .. 15) OF ALIASED XMM_register
   WITH
      Size           => 128 * 16,
      Object_Size    => 128 * 16,
      Component_Size => 128;

   TYPE XMM_string IS NEW string(1 .. 256)
   WITH
      Size           => 128 * 16,
      Object_Size    => 128 * 16,
      Component_Size => 8;

   FUNCTION To_XMM_Registers IS NEW Ada.Unchecked_Conversion
     (source => XMM_string, target => XMM_registers);

   TYPE arguments IS RECORD
      Operation_Call : operation := null_operation;                -- RDI.
      Argument_1     : general_register := general_register'first; -- RSI.
      Argument_2     : general_register := general_register'first; -- RDX.
      Argument_3     : general_register := general_register'first; -- R8.
      Argument_4     : general_register := general_register'first; -- R9.
      Argument_5     : general_register := general_register'first; -- R10.
   END RECORD
   WITH
      Convention => C;
   FOR arguments USE RECORD
      Operation_Call AT 00 RANGE 0 .. 63;
      Argument_1     AT 08 RANGE 0 .. 63;
      Argument_2     AT 16 RANGE 0 .. 63;
      Argument_3     AT 24 RANGE 0 .. 63;
      Argument_4     AT 32 RANGE 0 .. 63;
      Argument_5     AT 40 RANGE 0 .. 63;
   END RECORD;

   NUL : CONSTANT character := character'val(00);
   BS  : CONSTANT character := character'val(08);
   HT  : CONSTANT character := character'val(09);
   LF  : CONSTANT character := character'val(10);
   CR  : CONSTANT character := character'val(13);

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

   PROCEDURE Output_Byte
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Import        => true,
      Convention    => C,
      External_Name => "output_byte",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   PROCEDURE Output_Word
     (Port  : IN number;
      Value : IN number)
   WITH
      Global        => (Output => CPU_Port_State),
      Import        => true,
      Convention    => C,
      External_Name => "output_word",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FFFF#;

   FUNCTION Input_Byte
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Import            => true,
      Convention        => C,
      External_Name     => "input_byte",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Byte'result <= 16#00FF#;

   FUNCTION Input_Word
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Global            => (Input => CPU_Port_State),
      Import            => true,
      Convention        => C,
      External_Name     => "input_word",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Word'result <= 16#FFFF#;

   FUNCTION Byte_Swap
     (Value : IN number)
      RETURN number
   WITH
      Global        => NULL,
      Inline        => true,
      Import        => true,
      Convention    => Intrinsic,
      External_Name => "__builtin_bswap64";

   IRQ_Base : CONSTANT := 32;

   PROCEDURE Last_Chance_Handler
     (String_Address : IN address;
      Line           : IN natural)
   WITH
      Export        => true,
      No_Return     => true,
      External_Name => "__gnat_last_chance_handler";

   FUNCTION Source_Location
      RETURN string
   WITH
      Import     => true,
      Convention => Intrinsic;

   PROCEDURE Log
     (Information : IN string;
      Tag         : IN string  := "N/A";
      Warn        : IN boolean := false;
      Critical    : IN boolean := false)
   WITH
      Pre => Information'length <= XMM_string'length - 10 AND THEN
             Tag'first = positive'first                   AND THEN
             Tag'last IN positive'first .. 8;

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall";

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments;
      XMM_Data      : IN OUT XMM_registers)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall_data";

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments;
      String_Data   : IN OUT XMM_string)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall_data";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments)
      RETURN error
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments;
      XMM_Data      : IN OUT XMM_registers)
      RETURN error
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall_data";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments;
      String_Data   : IN OUT XMM_string)
      RETURN error
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "syscall_data";

   -- See the "HAVK_Kernel.Tasking" package for details about the tasking data
   -- structures.
   SUBTYPE task_name_string IS string(1 .. 64);
   TYPE task_status IS RECORD
      Index   : number := 0;
      Alive   : boolean := false;
      Name    : task_name_string := (OTHERS => NUL);
   END RECORD;
   FOR task_status USE RECORD
      Index   AT 00 RANGE 0 .. 63;
      Alive   AT 08 RANGE 0 .. 07;
      Name    AT 09 RANGE 0 .. (8 * task_name_string'length) - 1;
   END RECORD;
   TYPE padded_task_status IS RECORD
      Data   : task_status;
      Zeroed : bytes(1 .. 176);
   END RECORD
   WITH
      Size        => 2048,
      Object_Size => 2048;
   FUNCTION To_Status IS NEW Ada.Unchecked_Conversion
     (source => XMM_string, target => padded_task_status);

   -- Finds the specified task if it can and returns information about it if it
   -- can. If it can't, then the returned index will be zero.
   PROCEDURE Task_Finder
     (Task_Name : IN string;
      Status    : OUT task_status);

   PROCEDURE Exit_Task
     (Return_Code : IN number)
   WITH
      Import        => true,
      No_Return     => true,
      Convention    => C,
      External_Name => "exit";

END HAVK_Operating_System;
