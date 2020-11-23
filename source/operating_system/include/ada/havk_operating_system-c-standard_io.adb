-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-standard_io.adb                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion,
   Ada.Unchecked_Deallocation,
   HAVK_Operating_System.C.Standard_Library,
   HAVK_Operating_System.C.String;

PACKAGE BODY HAVK_Operating_System.C.Standard_IO
IS
   FUNCTION File_Open -- `fopen()`.
     (File_Path : ALIASED IN char_array;
      File_Mode : ALIASED IN char_array)
      RETURN FILE_pointer
   IS
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object => FILE, name => FILE_pointer);

      Call_Arguments : arguments :=
        (Operation_Call => send_message_operation,
         Argument_1     => general_register(Storage_Task),
         Argument_2     => general_register(Storage_Task_Port),
         OTHERS         => <>);

      New_File       : FILE_pointer := NEW FILE;
   BEGIN
      PRAGMA Warnings(off, "condition is always False",
         Reason => "The allocator can't raise a ""Storage_Error"" exception.");
      IF -- Heap exhaustion check.
         New_File = NULL
      THEN
         PRAGMA Warnings(on);
         RETURN NULL;
      ELSIF -- Maximum file path length check.
         New_File.File_Path'length <
            C.String.String_Length(File_Path, New_File.File_Path'length + 1)
      THEN
         Free(New_File);
         RETURN NULL;
      END IF;

      FOR
         Index IN New_File.File_Path'range
      LOOP
         EXIT WHEN File_Path(Index) = 0;
         New_File.File_Path(Index) := File_Path(Index);
      END LOOP;

      FOR
         Index IN New_File.File_Mode'range
      LOOP
         EXIT WHEN File_Mode(Index) = 0;
         New_File.File_Mode(Index) := File_Mode(Index);
      END LOOP;

      IF -- Send the file state.
         System_Call(Call_Arguments, New_File) /= no_error
      THEN
         Free(New_File);
         RETURN NULL;
      END IF;

      LOOP -- TODO: Need much better message handling than this...
         Call_Arguments.Operation_Call := receive_message_operation;
         Call_Arguments.Argument_1 := general_register(Storage_Task);
         Call_Arguments.Argument_2 := general_register(Storage_Task_Port);

         IF
            System_Call(Call_Arguments, New_File) /= attempt_error     AND THEN
            Call_Arguments.Argument_1 = general_register(Storage_Task) AND THEN
            Call_Arguments.Argument_2 = general_register(Storage_Task_Port)
         THEN
            RETURN New_File;
         END IF;

         Call_Arguments.Operation_Call := yield_operation;
         System_Call(Call_Arguments);
      END LOOP;
   END File_Open;

   FUNCTION File_Close -- `fclose()`.
     (Open_File : IN FILE_pointer)
      RETURN int
   IS
      -- Just need the address. The file information will not be required.
      FUNCTION To_Void_Pointer IS NEW Ada.Unchecked_Conversion
        (source => FILE_pointer, target => void_pointer);
   BEGIN
      C.Standard_Library.Free_Memory(To_Void_Pointer(Open_File));
      RETURN 0;
   END File_Close;

   FUNCTION File_Read -- `fread()`.
     (Data_Buffer   : IN void_pointer;
      Element_Size  : IN size_t;
      Element_Count : IN size_t;
      Open_File     : IN FILE_pointer)
      RETURN size_t
   IS
      Sending_Arguments   : arguments :=
        (Operation_Call => send_message_operation,
         Argument_1     => general_register(Storage_Task),
         Argument_2     => general_register(Storage_Task_Port),
         OTHERS         => <>);

      Receiving_Arguments : arguments;

      Buffer              : char_array(0 .. (Element_Size * Element_Count) - 1)
      WITH
         Import  => true,
         Address => Data_Buffer.ALL'address;

      New_File_State      : ALIASED FILE;
      Buffer_Offset       : size_t := Buffer'first;
   BEGIN
      IF
         Open_File.File_Error /= no_error OR ELSE
         Open_File.File_Size = 0          OR ELSE
         Open_File.File_Offset >= Open_File.File_Size - 1
      THEN
         RETURN 0;
      END IF;

      Open_File.Buffer_Length := Open_File.Buffer'last + 1;

      WHILE
         Buffer_Offset < Buffer'last
      LOOP
         IF
            Open_File.File_Offset + (Open_File.Buffer_Length - 1) >=
               Open_File.File_Size
         THEN
            Open_File.Buffer_Length :=
               Open_File.File_Size - Open_File.File_Offset;
         END IF;

         New_File_State := Open_File.ALL;

         IF
            System_Call(Sending_Arguments, New_File_State'access) /= no_error
         THEN
            RETURN Buffer_Offset / Element_Size;
         END IF;

         Blocking_Receive : LOOP
            Receiving_Arguments.Operation_Call := receive_message_operation;
            Receiving_Arguments.Argument_1 := general_register(Storage_Task);
            Receiving_Arguments.Argument_2 :=
               general_register(Storage_Task_Port);

            EXIT Blocking_Receive WHEN
               System_Call(Receiving_Arguments, New_File_State'access) =
                  no_error AND THEN
               Receiving_Arguments.Argument_1 = general_register(Storage_Task)
                  AND THEN
               Receiving_Arguments.Argument_2 =
                  general_register(Storage_Task_Port);

            -- Reuse the variable for yielding.
            Receiving_Arguments.Operation_Call := yield_operation;
            System_Call(Receiving_Arguments);
         END LOOP Blocking_Receive;

         Open_File.ALL := New_File_State;

         IF
            Open_File.File_Error /= no_error
         THEN
            RETURN Buffer_Offset / Element_Size; -- Current element count read.
         END IF;

         IF
            Buffer_Offset + (Open_File.Buffer_Length - 1) NOT IN Buffer'range
         THEN
            Open_File.Buffer_Length := (Buffer'last - Buffer_Offset) + 1;
         END IF;

         Buffer
           (Buffer_Offset .. Buffer_Offset + (Open_File.Buffer_Length - 1)) :=
               Open_File.Buffer(0 .. Open_File.Buffer_Length - 1);

         -- Update the offset relative to the file.
         Open_File.File_Offset :=
            Open_File.File_Offset + (IF Open_File.Buffer_Length > 1 THEN
               Open_File.Buffer_Length - 1 ELSE 1);

         -- Update the offset relative to the user's buffer.
         Buffer_Offset := Buffer_Offset + (IF Open_File.Buffer_Length > 1 THEN
            Open_File.Buffer_Length - 1 ELSE 1);
      END LOOP;

      RETURN Element_Count;
   END File_Read;

   FUNCTION File_Seek -- `fseek()`.
     (Open_File   : IN FILE_pointer;
      File_Offset : IN long;
      Origin      : IN int)
      RETURN int
   IS
   BEGIN
      CASE
         Origin
      IS
         WHEN SEEK_SET =>
            IF
               size_t(File_Offset) > Open_File.File_Size - 1
            THEN
               RETURN -1;
            END IF;

            Open_File.File_Offset := size_t(File_Offset);
         WHEN SEEK_CUR =>
            IF
               Open_File.File_Offset + size_t(File_Offset) NOT IN
                  Open_File.File_Offset .. Open_File.File_Size - 1
            THEN
               RETURN -1;
            END IF;

            Open_File.File_Offset :=
               Open_File.File_Offset + size_t(File_Offset);
         WHEN SEEK_END =>
            IF
              (Open_File.File_Size - 1) - size_t(File_Offset) >
                  Open_File.File_Size - 1
            THEN
               RETURN -1;
            END IF;

            Open_File.File_Offset :=
              (Open_File.File_Size - 1) - size_t(File_Offset);
         WHEN OTHERS =>
            RETURN -1;
      END CASE;

      RETURN 0;
   END File_Seek;

   FUNCTION File_Tell -- `ftell()`.
     (Open_File : IN FILE_pointer)
      RETURN long
   IS
     (long(Open_File.File_Offset));

END HAVK_Operating_System.C.Standard_IO;