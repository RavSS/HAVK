-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA PIO Driver                   --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation,
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.C,
   HAVK_Operating_System.C.Standard_IO,
   HAVK_Operating_System.Global,
   HAVK_FAT;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.C,
   HAVK_Operating_System.C.Standard_IO,
   HAVK_FAT;

-- TODO: The way file handling is implemented needs to be almost entirely
-- reworked for a server-like model. This is currently just using an
-- intermediate buffer for the sake of speed and is not safe.
PROCEDURE Main
WITH
   No_Return => true
IS
   USE
      Global.Drive_Manager;

   TYPE access_bytes IS ACCESS bytes;
   PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
     (object => bytes, name => access_bytes);

   FUNCTION System_Call IS NEW System_Call_Generic_Data_Function
     (generic_data => partitioned_PIO_request);

   Main_Tag           : CONSTANT string := "MAIN";
   Partition_Request  : ALIASED partitioned_PIO_request;
   Drive_Manager_Task : CONSTANT general_register := 3;
   Call_Arguments     : arguments;

   Main_FAT           : file_system;
   Task_File          : ALIASED Standard_IO.FILE;
   FAT_File           : HAVK_FAT.file;
   File_Name          : string(1 .. 255);

   Buffer_Space       : access_bytes := NULL;
   Buffer_Offset      : number;
   XMM_State          : XMM_registers;
   Error_Check        : error;
BEGIN
   -- TODO: For now, the drive manager just sends us partition information for
   -- the ESP and allows use to access sectors within the partition on the
   -- drive that contains it. This will most certainly change in the future
   -- once work on the security model begins to finally start.

   -- Send the blank message for wanting some partition information.
   Call_Arguments := (send_message_operation, Drive_Manager_Task,
      OTHERS => <>);
   LOOP
      EXIT WHEN
         System_Call(Call_Arguments, Partition_Request'access) = no_error;
   END LOOP;

   -- Now receive the partition information.
   Call_Arguments := (receive_message_operation, Drive_Manager_Task,
      OTHERS => <>);
   LOOP
      EXIT WHEN
         System_Call(Call_Arguments, Partition_Request'access) = no_error;
   END LOOP;

   IF
      NOT Partition_Request.Partition_Data.Present
   THEN
      RAISE Program_Error
      WITH
         Source_Location & " - Could not obtain partition information.";
   END IF;

   Log("The FAT driver has received the EFI System Partition's information.",
      Tag => Main_Tag);
   Get_File_System(Main_FAT, Partition_Request.Partition_Data);

   IF -- TODO: Only FAT16 is supported. My ESP is also FAT16 formatted.
      Get_FAT_Version(Main_FAT) /= FAT16
   THEN
      RAISE Program_Error
      WITH
         Source_Location & " - The FAT16 partition could not be read.";
   END IF;

   Log("The FAT driver is ready to respond to file management requests.",
      Tag => Main_Tag);

   LOOP
      Call_Arguments.Operation_Call := receive_message_operation;
      Call_Arguments.Argument_1 := 0;

      IF
         System_Call(Call_Arguments, Task_File'access) = no_error
      THEN
         File_Name := (OTHERS => NUL);

         FOR -- Convert it to a standard string object.
            ASCII_Index IN Task_File.File_Path'range
         LOOP
            File_Name(positive(ASCII_Index + 1)) :=
               character'val(Task_File.File_Path(ASCII_Index));
         END LOOP;

         IF -- They're requesting file information.
            Task_File.File_Size = 0 OR ELSE
            Task_File.Buffer_Size = 0
         THEN
            Check_File(Main_FAT, File_Name, Error_Check, FAT_File);

            IF
               Error_Check = no_error
            THEN
               Task_File.File_Size := size_t(FAT_File.Size);
            END IF;

            Call_Arguments :=
              (send_message_operation,
               Call_Arguments.Argument_1,
               Error_Check'enum_rep,
               OTHERS => <>);
            System_Call(Call_Arguments, Task_File'access);
         ELSIF -- They're requesting file data. Sanity check the offsets.
            Task_File.File_Offset < Task_File.File_Size AND THEN
            Task_File.File_Offset + (Task_File.Buffer_Size - 1) <
               Task_File.File_Size
         THEN
            IF -- TODO: Write requests not yet supported.
               NOT Task_File.Write_Request
            THEN
               Buffer_Space := NEW bytes(1 .. number(Task_File.Buffer_Size));

               Read_File(Main_FAT, File_Name,
                  Buffer_Space.ALL(1)'address, Error_Check,
                  Base_Byte => number(Task_File.File_Offset) + 1,
                  Byte_Size => number(Task_File.Buffer_Size));

               IF
                  Error_Check = no_error
               THEN
                  Buffer_Offset := Buffer_Space'first;

                  WHILE
                     Buffer_Offset /= number(Task_File.File_Offset +
                       (Task_File.Buffer_Size - 1))
                  LOOP
                     FOR
                        Data_Block OF XMM_State
                     LOOP
                        FOR
                           Byte_Index IN Data_Block.XMM_Bytes'range
                        LOOP
                           IF
                              Buffer_Offset /= number(Task_File.File_Offset +
                                (Task_File.Buffer_Size - 1))
                           THEN
                              Data_Block.XMM_Bytes(Byte_Index) :=
                                 Buffer_Space(Buffer_Offset);

                              Buffer_Offset := Buffer_Offset + 1;
                           ELSE
                              Data_Block.XMM_Bytes(Byte_Index) := 0;
                           END IF;
                        END LOOP;
                     END LOOP;

                     Call_Arguments.Operation_Call := send_message_operation;
                     LOOP
                        EXIT WHEN
                           System_Call(Call_Arguments, XMM_State) = no_error;
                     END LOOP;
                  END LOOP;
               ELSE
                  Call_Arguments :=
                    (send_message_operation,
                     Call_Arguments.Argument_1,
                     Error_Check'enum_rep,
                     OTHERS => <>);
                  System_Call(Call_Arguments, Task_File'access);
               END IF;

               Free(Buffer_Space);
            ELSE
               Call_Arguments :=
                 (send_message_operation,
                  Call_Arguments.Argument_1,
                  permission_error'enum_rep,
                  OTHERS => <>);
               System_Call(Call_Arguments, Task_File'access);
            END IF;
         ELSE -- Bad values were detected.
            Call_Arguments :=
              (send_message_operation,
               Call_Arguments.Argument_1,
               index_error'enum_rep,
               OTHERS => <>);
            System_Call(Call_Arguments, Task_File'access);
         END IF;
      END IF;
   END LOOP;
END Main;
