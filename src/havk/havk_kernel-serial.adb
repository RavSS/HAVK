WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Serial
WITH
   SPARK_Mode => off -- "enum_rep" is utilised. Enable SPARK later.
IS
   PROCEDURE Interface_Initialise(
      Object                  : IN serial_connection)
   IS
      -- TODO: Perhaps make these into functions.
      Interrupt_Settings_Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Object.Interrupt_Settings'address;

      Line_Settings_Byte      : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Object.Line_Settings'address;

      -- Only a baud rate between 50 to 115200 is supported.
      Divisor_Latch_Value     : CONSTANT num RANGE 1 .. 2304 :=
         115200 / Object.Baud_Rate;

      -- Base port number. Offsets from it indicate different registers
      Port_Base               : CONSTANT num := Object.Port'enum_rep;
   BEGIN
      -- Disable interrupt handling of the connection.
      OUTB(Port_Base + 1, Interrupt_Settings_Byte);

      -- Set the divisor latch access bit to true for the purposes of setting
      -- the baud rate. Offsets +0 and +1 are now used for setting the divisor.
      OUTB(Port_Base + 3, 16#80#);

      -- Send the low byte and then the high byte.
      OUTB(Port_Base + 0,     Divisor_Latch_Value     AND 16#FF#);
      OUTB(Port_Base + 1, SHR(Divisor_Latch_Value, 8) AND 16#FF#);

      -- Apply the line control settings.
      OUTB(Port_Base + 3, Line_Settings_Byte);

      -- TODO: Make records for these.
      OUTB(Port_Base + 2, 16#C7#); -- 11000111 -  FIFO control register.
      OUTB(Port_Base + 4, 16#0B#); --     1011 - modem control register.
   END Interface_Initialise;

   PROCEDURE Write(
      Object      : IN serial_connection;
      Data        : IN character)
   IS
   BEGIN
      WHILE NOT Object.Get_Status.Empty_Buffer LOOP
         PAUSE; -- Wait for the buffer to become empty.
      END LOOP;
      -- Send data.
      OUTB(Object.Port'enum_rep, character'pos(Data));
   END Write;

   PROCEDURE Print(
      Object      : IN serial_connection;
      Data        : IN string)
   IS
   BEGIN
      FOR C IN Data'range LOOP
         IF Data(C) /= character'val(0) THEN -- Never write null bytes in here.
            Object.Write(Data(C));
         END IF;
      END LOOP;
   END Print;

   FUNCTION Get_Status(
      Object      : IN serial_connection)
   RETURN line_status_register IS
      Status_Port : CONSTANT num := Object.Port'enum_rep + 5;
      Status_Byte : CONSTANT num RANGE 0 .. 16#FF# := INB(Status_Port);
      Status      : CONSTANT line_status_register
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Status_Byte'address;
   BEGIN
      RETURN Status;
   END Get_Status;
END HAVK_Kernel.Serial;
