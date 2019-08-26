-- The purpose of this package is really just to send debugging information
-- to the serial ports, not so much actual communication. I do however intend
-- on adding most of the functionality required, maybe in the future I could
-- do GDB stub debugging via a serial port on an actual real machine. For now,
-- there will be a lot of unused records and whatnot.
-- READ: https://en.wikibooks.org/wiki/Serial_Programming/8250_UART_Programming

PACKAGE HAVK_Kernel.Serial
IS
   -- A type that contains a few of the COM ports that may be present.
   -- I believe only COM1 and COM2 are actually guaranteed on the hardware.
   COM : CONSTANT nums(1 .. 4) := (16#3F8#, 16#2F8#, 16#3E8#, 16#2E8#);

   -- 2 bit values that control the length of the inputted words (data).
   -- Reminder that ASCII only needs 7 bits.
   TYPE length_select  IS(word_5_bit, word_6_bit, word_7_bit, word_8_bit);
   FOR  length_select USE(         0,          1,          2,          3);

   -- 3 bit values that are used to set the parity of serial connection.
   TYPE parity_select  IS(  none,    odd,   even,   mark,  space);
   FOR  parity_select USE(2#000#, 2#001#, 2#011#, 2#101#, 2#111#);

   -- Controls the line connection itself. Offset is 3 bytes after the
   -- port's base IO address (+3). Mostly used for setting the protocol.
   -- 8 bits - no parity - 1 stop bit is the most common line protocol to use.
   TYPE line_control_register IS RECORD
      -- Set the size of the data transmitted from 5 bits to 8 bits.
      Data_Size            : length_select;
      -- True if there's more than one stop bit.
      Extra_Stop           : boolean;
      -- Controls the parity bit.
      Parity               : parity_select;
      -- Simply enables the break when set to true.
      Data_Breaking        : boolean;
      -- Essentially lets you set the baud rate by changing the meaning
      -- of port offsets, particularly +0 and +1, respectively for LSB and
      -- MSB of the divisor value.
      Divisor_Latch_Access : boolean;
   END RECORD;
   FOR line_control_register USE RECORD
      Data_Size            AT 0 RANGE 0 .. 1;
      Extra_Stop           AT 0 RANGE 2 .. 2;
      Parity               AT 0 RANGE 3 .. 5;
      Data_Breaking        AT 0 RANGE 6 .. 6;
      Divisor_Latch_Access AT 0 RANGE 7 .. 7;
   END RECORD;

   -- The following bits control when to interrupt the CPU. This register
   -- must be written to a byte after the serial connection's port (+1).
   -- Making everything false means the serial chip will never raise an
   -- interrupt; thus, disabling them entirely. Interrupts are raised at
   -- IRQ3 (COM2) and IRQ4 (COM1).
   TYPE interrupt_enable_register IS RECORD
      -- Tells the interrupt handler that data is ready to be retrieved.
      Data_Received           : boolean;
      -- The transmitter has finished outputting data and the buffer is empty.
      Output_Finished         : boolean;
      -- The line status register has been modified.
      Line_Status_Changed     : boolean;
      -- The status has been changed for the external modem etc. This is
      -- seriously outdated stuff, it can even tell you if the connection is
      -- going to be interrupted by a telephone call akin to dial-up!
      External_Status_Changed : boolean;
      -- If the controller is currently sleeping. May not exist in all chips.
      Sleep_Mode              : boolean;
      -- If the controller is using as little power as it can. Again, this
      -- may not exist in every serial transmission chip.
      Low_Power_Mode          : boolean;
      -- TODO: Does a boolean always represent true as a single bit no
      -- matter what? Or does the compiler do whatever it wants freely with
      -- the rest of the extra bits? I'd like to make this into a boolean.
      -- There's two reserved bits, but I've combined them into one variable.
      Zeroed                  : num RANGE 0 .. 0;
   END RECORD;
   FOR interrupt_enable_register USE RECORD
      Data_Received            AT 0 RANGE 0 .. 0;
      Output_Finished          AT 0 RANGE 1 .. 1;
      Line_Status_Changed      AT 0 RANGE 2 .. 2;
      External_Status_Changed  AT 0 RANGE 3 .. 3;
      Sleep_Mode               AT 0 RANGE 4 .. 4;
      Low_Power_Mode           AT 0 RANGE 5 .. 5;
      Zeroed                   AT 0 RANGE 6 .. 7;
   END RECORD;

   -- An serial port object that contains the port address and more.
   TYPE serial_connection IS TAGGED RECORD
      -- 16-bit IO address for the COM port.
      Port               : num RANGE  0 .. 16#FFFF#;
      -- The baud rate used by the connection upon initialisation.
      Baud_Rate          : num RANGE 50 .. 115200;
      -- For the convenience of the data receiver, especially when just
      -- sending ASCII for textual purposes.
      Line_Ender         : string(1 .. 2);
      -- Holds the settings for the line control register.
      Line_Settings      : line_control_register;
      -- Holds the settings for when to interrupt the system.
      Interrupt_Settings : interrupt_enable_register;
   END RECORD;

-- PRIVATE
   -- The structure of the line status byte in the line status register,
   -- which can be found 5 bytes after the serial port base's address (+5).
   TYPE line_status_register IS RECORD
      -- Data is ready to be read from the port.
      Data_Ready       : boolean;
      -- Happens if data has been lost due to an overrun.
      Data_Lost        : boolean;
      -- Parity bit indicates an error.
      Error_Detected   : boolean;
      -- Occurs when the stop bit is completely missing.
      Incomplete       : boolean;
      -- Set when there's a break or a pause in the transmission of data.
      Data_Break       : boolean;
      -- True when data is ready to be sent.
      Empty_Buffer     : boolean;
      -- True when the serial transmitter is not transmitting anything.
      Not_Transmitting : boolean;
      -- Occurs when there's an issue with the inputted data.
      Data_Error       : boolean;
   END RECORD;
   FOR line_status_register USE RECORD
      Data_Ready       AT 0 RANGE 0 .. 0;
      Data_Lost        AT 0 RANGE 1 .. 1;
      Error_Detected   AT 0 RANGE 2 .. 2;
      Incomplete       AT 0 RANGE 3 .. 3;
      Data_Break       AT 0 RANGE 4 .. 4;
      Empty_Buffer     AT 0 RANGE 5 .. 5;
      Not_Transmitting AT 0 RANGE 6 .. 6;
      Data_Error       AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Sets up the serial port debugging functionality.
   PROCEDURE Interface_Initialise(
      Connection : IN serial_connection);

   -- Prints a string to the serial connection Connection's port.
   PROCEDURE Print(
      Connection : IN serial_connection;
      Data       : IN string)
   WITH
      Inline => true;

   -- Writes a character to the serial connection object's port.
   PROCEDURE Write(
      Connection : IN serial_connection;
      Data       : IN character)
   WITH
      Inline => true;

   -- Get the current line status register.
   FUNCTION Get_Status(
      Connection : IN serial_connection)
   RETURN line_status_register;
END HAVK_Kernel.Serial;