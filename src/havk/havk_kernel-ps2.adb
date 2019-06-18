WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.PS2 IS
   FUNCTION Configuration_To_Byte(
      Configuration : IN PS2_configuration)
   RETURN num IS
      Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Configuration'address;
   BEGIN
      RETURN Byte;
   END Configuration_To_Byte;

   FUNCTION Byte_To_Status(
      Status_Byte  : IN num)
   RETURN PS2_status IS
      Status : CONSTANT PS2_status
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Status_Byte'address;
   BEGIN
      RETURN Status;
   END Byte_To_Status;

   PROCEDURE Controller_Flush
   IS
      Discard : num; -- Magic variable name for GNAT.
   BEGIN
      WHILE Byte_To_Status(INB(IO_Command_Port)).Output_Ready LOOP
         Discard := INB(IO_Data_Port);
      END LOOP;
   END Controller_Flush;

   PROCEDURE Controller_Initialize
   IS
      Current_Configuration : CONSTANT PS2_configuration :=
      (
         Port_1_Enabled     => true,
         Port_2_Enabled     => false, -- Disable port 2 for now.
         System_POST_Pass   => true,
         Zeroed_1           => 0,
         Port_1_Clock       => true,
         Port_2_Clock       => false,
         Port_1_Translation => false, -- Do not translate (at least now).
         Zeroed_2           => 0
      );

      Current_Typematics    : CONSTANT PS2_typematics    :=
      (
         Repeat_Rate        => 16#1F#, -- Slowest repeat rate.
         Delay_Rate         => 3,      -- Largest delay.
         Zeroed             => 0
      );

      Current_Typematics_Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Current_Typematics'address;
   BEGIN -- TODO: Lacks comprehensive error handling and checking.
      -- First disable both PS2 ports.
      OUTB(IO_Command_Port, Disable_Port_1);
      OUTB(IO_Command_Port, Disable_Port_2);

      -- Flush anything in the output buffer.
      Controller_Flush;

      -- Write my configuration.
      OUTB(IO_Command_Port, Configuration_Write);
      OUTB(IO_Data_Port, Configuration_To_Byte(Current_Configuration));

      -- Write my typematic features configuration.
      OUTB(IO_Command_Port, Typematics_Write);
      OUTB(IO_Data_Port, Current_Typematics_Byte);

      -- Enable the first PS2 port.
      OUTB(IO_Command_Port, Enable_Port_1);

      -- Set the package's global state variable to functional.
      Controller_State := functional;
   END Controller_Initialize;
END HAVK_Kernel.PS2;
