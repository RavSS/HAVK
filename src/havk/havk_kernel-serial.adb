WITH
   HAVK_Kernel.Intrinsics;
USE
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Serial IS
   PROCEDURE Interface_Initialize(
      Connection : IN serial_connection)
   IS
      -- TODO: Perhaps make these into functions.
      Line_Control_Register_Byte     : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Connection.Line_Settings'address;

      Interrupt_Enable_Register_Byte : CONSTANT num RANGE 0 .. 16#FF#
      WITH
         Import     => true,
         Convention => Ada,
         Size       => 8,
         Address    => Connection.Interrupt_Settings'address;

      -- Only a baud rate between 50 to 115200 is supported.
      Divisor_Latch_Value            : CONSTANT num RANGE 1 .. 2304 :=
         115200 / Connection.Baud_Rate;
   BEGIN
      -- Disable interrupt handling of the connection.
      OUTB(Connection.Port + 1, Interrupt_Enable_Register_Byte);

      -- Set the divisor latch access bit to true for the purposes of setting
      -- the baud rate. Offsets +0 and +1 are now used for setting the divisor.
      OUTB(Connection.Port + 3, 16#80#);

      -- Send the low byte and then the high byte.
      OUTB(Connection.Port + 0,     Divisor_Latch_Value     AND 16#FF#);
      OUTB(Connection.Port + 1, SHR(Divisor_Latch_Value, 8) AND 16#FF#);

      -- Apply the line control settings.
      OUTB(Connection.Port + 3, Line_Control_Register_Byte);

      -- TODO: Make records for these.
      OUTB(Connection.Port + 2, 16#C7#); -- 11000111 -  FIFO control register.
      OUTB(Connection.Port + 4, 16#0B#); --     1011 - modem control register.
   END Interface_Initialize;

   PROCEDURE Write(
      Connection : IN serial_connection;
      Data       : IN character)
   IS
   BEGIN
      LOOP -- Wait for the buffer to become empty.
         EXIT WHEN Connection.Get_Status.Empty_Buffer;
      END LOOP;
      -- Send data.
      OUTB(Connection.Port, character'pos(Data));
   END Write;

   PROCEDURE Print(
      Connection : IN serial_connection;
      Data       : IN string)
   IS
   BEGIN
      FOR C IN Data'range LOOP
         IF Data(C) /= character'val(0) THEN -- Never write null bytes in here.
            Connection.Write(Data(C));
         END IF;
      END LOOP;
   END Print;

   FUNCTION Get_Status(
      Connection  : IN serial_connection)
   RETURN line_status_register IS
      Status_Byte : CONSTANT num RANGE 0 .. 16#FF# := INB(Connection.Port + 5);
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