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
-- TODO: Either add this to the runtime or let programs use it regularly.
-- TODO: This package (especially its body) is very rushed and needs
-- documentation etc.
PACKAGE HAVK_Operating_System
IS
   TYPE number IS MOD 2**64;
   PRAGMA Provide_Shift_Operators(number);

   TYPE bits        IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**01 - 1
   WITH
      Component_Size          => 01,
      Default_Component_Value => 00;
   TYPE bytes       IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**08 - 1
   WITH
      Component_Size          => 08,
      Default_Component_Value => 00;
   TYPE words       IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**16 - 1
   WITH
      Component_Size          => 16,
      Default_Component_Value => 00;
   TYPE doublewords IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**32 - 1
   WITH
      Component_Size          => 32,
      Default_Component_Value => 00;
   TYPE quadwords   IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**64 - 1
   WITH
      Component_Size          => 64,
      Default_Component_Value => 00;
   TYPE addresses   IS ARRAY(number RANGE <>) OF ALIASED address
   WITH
      Component_Size          => 64,
      Default_Component_Value => 00;

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

   TYPE XMM_register IS RECORD
      High : number := 0;
      Low  : number := 0;
   END RECORD
   WITH
      Object_Size => 128;
   FOR XMM_register USE RECORD
      High AT 0 RANGE 0 .. 63;
      Low  AT 8 RANGE 0 .. 63;
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
      Import        => true,
      Convention    => C,
      External_Name => "output_byte",
      Pre           => Port <= 16#FFFF# AND THEN Value <= 16#FF#;

   FUNCTION Input_Byte
     (Port  : IN number)
      RETURN number
   WITH
      Volatile_Function => true,
      Import            => true,
      Convention        => C,
      External_Name     => "input_byte",
      Pre               => Port              <= 16#FFFF#,
      Post              => Input_Byte'result <= 16#00FF#;

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
