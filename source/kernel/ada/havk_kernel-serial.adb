-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-serial.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE BODY HAVK_Kernel.Serial
WITH
   Refined_State => (UART_State => NULL)
IS
   PROCEDURE Prepare_Connection
     (Context : IN connection)
   IS
      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => interrupt_enable_register, target => byte);

      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => line_control_register,     target => byte);

      -- Only a baud rate between 50 to 115200 is supported.
      Divisor_Latch_Value : CONSTANT number RANGE 1 .. 2304 :=
         115200 / Context.Baud_Rate;
   BEGIN
      -- Disable interrupt handling of the connection.
      Output_Byte(Context.Port + 1,
         number(Convert(Context.Interrupt_Settings)));

      -- Set the divisor latch access bit to true for the purposes of setting
      -- the baud rate. Offsets +0 and +1 are now used for setting the divisor.
      Output_Byte(Context.Port + 3, 16#80#);

      -- Send the low byte and then the high byte.
      Output_Byte
        (Context.Port + 0,             Divisor_Latch_Value     AND 16#FF#);
      Output_Byte
        (Context.Port + 1, Shift_Right(Divisor_Latch_Value, 8) AND 16#FF#);

      -- Apply the line control settings.
      Output_Byte(Context.Port + 3, number(Convert(Context.Line_Settings)));

      -- TODO: Make records for these.
      Output_Byte(Context.Port + 2, 2#11000111#); --  FIFO control register.
      Output_Byte(Context.Port + 4, 2#00001011#); -- Modem control register.
   END Prepare_Connection;

   PROCEDURE Write
     (Context : IN connection;
      Data    : IN character)
   IS
      Empty_Buffer : boolean := Get_Status(Context).Empty_Buffer;
   BEGIN
      WHILE
         NOT Empty_Buffer
      LOOP
         Empty_Buffer := Get_Status(Context).Empty_Buffer;
         Spinlock_Pause; -- Wait for the buffer to become empty.
      END LOOP;

      Output_Byte(Context.Port, character'pos(Data)); -- Send data.
   END Write;

   PROCEDURE Print
     (Context : IN connection;
      Data    : IN string)
   IS
   BEGIN
      FOR
         ASCII OF Data
      LOOP
         IF
            ASCII /= NUL -- Never write null bytes in here.
         THEN
            Write(Context, ASCII);
         END IF;
      END LOOP;
   END Print;

   FUNCTION Get_Status
     (Context : IN connection)
      RETURN line_status_register
   IS
      FUNCTION Convert IS NEW Ada.Unchecked_Conversion
        (source => byte, target => line_status_register);

      Data : CONSTANT byte := byte(Input_Byte(Context.Port + 5));
   BEGIN
      RETURN Convert(Data);
   END Get_Status;
END HAVK_Kernel.Serial;
