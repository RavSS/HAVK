-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA PIO Driver                   --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.C,
   HAVK_Operating_System.C.Standard_IO,
   HAVK_ATA_PIO,
   HAVK_ATA_PIO.GPT,
   HAVK_ATA_PIO.FAT;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Call.Logging,
   HAVK_Operating_System.C,
   HAVK_Operating_System.C.Standard_IO,
   HAVK_ATA_PIO;

-- TODO: As of now, this can only read files off the drive. There is also no
-- permission checking etc. Another task will have to be consulted for that.
PROCEDURE Main
WITH
   No_Return => true
IS
   USE TYPE
      FAT.version;

   Main_Tag             : CONSTANT string := "MAIN";
   Error_Check          : error;

   EFI_System_Partition : GPT.partition;
   ESP_File_System      : FAT.file_system;
   ESP_File             : FAT.file;

   Call_Arguments       : arguments;
   Temporary_File_State : CONSTANT FILE_pointer := NEW FILE;
   Temporary_File_Path  : string(1 .. 128);
BEGIN
   -- TODO: Does not do drive error checking or drive resets. Last time I
   -- checked, this will freeze up the driver if a non-existent drive is
   -- accessed.
   FOR -- Check both buses and both drives for the (U)EFI boot partition.
      I IN 1 .. 4
   LOOP
      GPT.Get_Partition(EFI_System_Partition, "EFI",
         Secondary_Bus => (I IN 3 | 4), Secondary_Drive => (I IN 2 | 4));
      EXIT WHEN EFI_System_Partition.Present;
   END LOOP;

   IF
      NOT EFI_System_Partition.Present
   THEN
      RAISE Program_Error
      WITH
         "No EFI System Partition (ESP) detected.";
   END IF;

   FAT.Get_File_System(ESP_File_System, EFI_System_Partition);

   IF
      FAT.Get_FAT_Version(ESP_File_System) /= FAT.FAT16
   THEN
      RAISE Program_Error
      WITH
         "No FAT16 partition detected.";
   END IF;

   Log("EFI System Partition (ESP) found. Now receiving file requests.",
      Tag => Main_Tag);

   LOOP -- Begin the request loop.
      Call_Arguments.Operation_Call := receive_message_operation;
      Error_Check := System_Call(Call_Arguments, Temporary_File_State);

      IF
         Error_Check = no_error                    AND THEN
         Call_Arguments.Argument_3 = general_register(Storage_Task_Port)
            AND THEN
         Temporary_File_State.ALL.File_Error'valid AND THEN
         Temporary_File_State.Buffer_Length IN
            Temporary_File_State.Buffer'first + 1 ..
               Temporary_File_State.Buffer'last + 1
      THEN
         FOR
            Index IN Temporary_File_State.File_Path'range
         LOOP
            Temporary_File_Path(positive(Index + 1)) := character'val
              (Temporary_File_State.File_Path(Index));
         END LOOP;

         IF -- If they give a size, then they want to do a file read/write.
            Temporary_File_State.File_Size /= 0
         THEN
            IF
               NOT Temporary_File_State.Write_Request
            THEN
               FAT.Read_File(ESP_File_System, Temporary_File_Path,
                  Temporary_File_State.Buffer'address, Error_Check,
                  Base_Byte => number(Temporary_File_State.File_Offset + 1),
                  Byte_Size => number(Temporary_File_State.Buffer_Length));

               Temporary_File_State.File_Error := Error_Check;
            ELSE
               -- TODO: Writing is not yet implemented in my FAT driver.
               Temporary_File_State.File_Error := attempt_error;
            END IF;
         ELSE -- Otherwise, just check the file.
            FAT.Check_File(ESP_File_System, Temporary_File_Path,
               Error_Check, ESP_File);

            IF
               Error_Check = no_error
            THEN
               Temporary_File_State.File_Size := size_t(ESP_File.Size);
            ELSE
               Temporary_File_State.File_Error := Error_Check;
            END IF;
         END IF;

         Call_Arguments.Operation_Call := send_message_operation;
         Call_Arguments.Argument_2 := FILE'size / 8;

         System_Call(Call_Arguments, Temporary_File_State);
      ELSE
         Call_Arguments.Operation_Call := yield_operation;
         System_Call(Call_Arguments);
      END IF;
   END LOOP;
END Main;