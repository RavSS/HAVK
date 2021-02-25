-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call.ads                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE HAVK_Operating_System.Call
WITH
   Preelaborate => true
IS
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
      buffer_operation,
      framebuffer_access_operation)
   WITH
      Convention => C;

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
   FUNCTION To_XMM_String IS NEW Ada.Unchecked_Conversion
     (source => XMM_registers, target => XMM_string);

   TYPE arguments IS RECORD
      Operation_Call : operation := null_operation;                -- RAX.
      Argument_1     : general_register := general_register'first; -- RDI.
      Argument_2     : general_register := general_register'first; -- RSI.
      Argument_3     : general_register := general_register'first; -- RDX.
      Argument_4     : general_register := general_register'first; -- R8.
      Argument_5     : general_register := general_register'first; -- R9.
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

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "system_call";

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments;
      XMM_Data      : IN OUT XMM_registers)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "system_call_xmm";

   PROCEDURE System_Call
     (Argument_Data : IN OUT arguments;
      String_Data   : IN OUT XMM_string)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "system_call_xmm";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments)
      RETURN error
   WITH
      Volatile_Function => true,
      Import            => true,
      Convention        => C,
      External_Name     => "system_call";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments;
      XMM_Data      : IN OUT XMM_registers)
      RETURN error
   WITH
      Volatile_Function => true,
      Import            => true,
      Convention        => C,
      External_Name     => "system_call_xmm";

   FUNCTION System_Call
     (Argument_Data : IN OUT arguments;
      String_Data   : IN OUT XMM_string)
      RETURN error
   WITH
      Volatile_Function => true,
      Import            => true,
      Convention        => C,
      External_Name     => "system_call_xmm";

   -- What follows below are generic subprograms so you don't have to keep
   -- converting data into an XMM representation; however, the data must be
   -- exactly 2,048 bits or 256 bytes in space. Too short and you will most
   -- likely crash the task. Too long and you will not send the data properly.
   GENERIC
      TYPE generic_data IS PRIVATE;
   FUNCTION System_Call_Generic_Data_Function
     (Argument_Data : IN OUT arguments;
      String_Data   : NOT NULL ACCESS generic_data)
      RETURN error
   WITH
      Volatile_Function => true,
      Import            => true,
      Convention        => C,
      External_Name     => "system_call_xmm";

   GENERIC
      TYPE generic_data IS PRIVATE;
   PROCEDURE System_Call_Generic_Data_Procedure
     (Argument_Data : IN OUT arguments;
      String_Data   : NOT NULL ACCESS generic_data)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "system_call_xmm";

END HAVK_Operating_System.Call;
