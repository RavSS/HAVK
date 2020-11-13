-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA PIO Driver                   --
-- Filename        -- havk_ata_pio.adb                                       --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_ATA_PIO
WITH
   Refined_State => (Drive_State => NULL)
IS
   PRAGMA Warnings(off, "types for unchecked conversion have different sizes",
      Reason => "The I/O port wrappers can't return beyond their sizes.");

   PROCEDURE PIO_Input
     (Data          : OUT generic_format;
      Secondary_Bus : IN boolean := false)
   IS
      FUNCTION Value_To_Format IS NEW Ada.Unchecked_Conversion
        (Source => number, Target => generic_format);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "The retrieved format is manually checked.");

      -- Calculate the true port value.
      Port_Value : CONSTANT number RANGE 0 .. 2**16 - 1 :=
      (
         IF
            Port <= command_status_port
         THEN
           (IF Secondary_Bus THEN Disk_IO_Base_2 ELSE Disk_IO_Base_1) +
               Port'enum_rep
         ELSE -- "control_status_port"/"select_port" offset correction.
           (IF Secondary_Bus THEN Control_Base_2 ELSE Control_Base_1) +
              (Port'enum_rep - 8)
      );

      -- No masking is necessary as the I/O instruction wrappers/intrinsics
      -- will zero out everything higher than the byte or word.
      Temporary : number;
   BEGIN
      IF
         NOT Word_Length
      THEN
         Temporary := Input_Byte(Port_Value);
      ELSE
         Temporary := Input_Word(Port_Value);
      END IF;

      Data := Value_To_Format(Temporary);
   END PIO_Input;

   PROCEDURE PIO_Output
     (Data          : IN generic_format;
      Secondary_Bus : IN boolean := false)
   IS
      FUNCTION Format_To_Value IS NEW Ada.Unchecked_Conversion
        (Source => generic_format, Target => number);
      PRAGMA Annotate(GNATprove, False_Positive,
         "type with constraints on bit representation *",
         "As long as the format fits inside 64 bits, this is safe.");

      -- The value we must send to the port. This is converted using
      -- `Ada.Unchecked_Conversion` to silence nearly all the warnings in one
      -- go, as opposed to doing it with an import. It is not erronous unless
      -- the "Data" type is greater than 16 bits. We can cover any higher junk
      -- bits for e.g. an 8-bit value via a mask, which I will do later.
      Unmasked_Data_Value : CONSTANT number := Format_To_Value(Data);

      -- Calculate the true port value.
      Port_Value          : CONSTANT number RANGE 0 .. 2**16 - 1 :=
      (
         IF
            Port <= command_status_port
         THEN
           (IF Secondary_Bus THEN Disk_IO_Base_2 ELSE Disk_IO_Base_1) +
               Port'enum_rep
         ELSE -- "control_status_port"/"select_port" offset correction.
           (IF Secondary_Bus THEN Control_Base_2 ELSE Control_Base_1) +
              (Port'enum_rep - 8)
      );
   BEGIN
      IF
         NOT Word_Length
      THEN
         Output_Byte(Port_Value, Unmasked_Data_Value AND 16#00FF#);
      ELSE
         Output_Word(Port_Value, Unmasked_Data_Value AND 16#FFFF#);
      END IF;
   END PIO_Output;

   PROCEDURE PIO_Status
     (Current_Status : OUT drive_status;
      Secondary_Bus  : IN boolean := false)
   IS
      -- Don't use the interrupt version if we're doing this manually.
      PROCEDURE Get_Bus_Status IS NEW PIO_Input
        (generic_format => status_register, Port => control_status_port);

      Status : status_register;
   BEGIN
      FOR -- It's recommended to read 5 times and ignore the previous returns.
         Retry IN 1 .. 5
      LOOP
         Get_Bus_Status(Status, Secondary_Bus);
      END LOOP;

      Current_Status :=
        (Drive_Ready    => (NOT Status.Busy AND THEN Status.Active),
         Drive_Error    => (NOT Status.Active AND THEN Status.Error),
         Transfer_Ready => (Status.Ready AND THEN Status.Active));
   END PIO_Status;

   PROCEDURE PIO_Read
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Destination     : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   IS
      PROCEDURE Set_Bus_Drive    IS NEW PIO_Output
        (generic_format => drive_register, Port => drive_port);

      PROCEDURE Set_Sector_Count IS NEW PIO_Output
        (generic_format => number, Port => sector_count_port);

      PROCEDURE Set_Low_LBA      IS NEW PIO_Output
        (generic_format => number, Port => LBA_low_port);
      PROCEDURE Set_Middle_LBA   IS NEW PIO_Output
        (generic_format => number, Port => LBA_middle_port);
      PROCEDURE Set_High_LBA     IS NEW PIO_Output
        (generic_format => number, Port => LBA_high_port);

      PROCEDURE Send_Command     IS NEW PIO_Output
        (generic_format => ATA_command, Port => command_status_port);

      -- ATA PIO reads (and writes) data in 16-bit values.
      SUBTYPE word IS number RANGE 0 .. 2**16 - 1;

      PROCEDURE Read_Data IS NEW PIO_Input
        (generic_format => word, Port => data_port, Word_Length => true);

      LBA48_Mode       : CONSTANT boolean :=
         Sector_Base > 2**28 - 1 OR ELSE Sector_Count > 255;
      Status           : drive_status;

      -- There is no need to zero it out first, so an import as opposed to just
      -- an address overlay is more efficient. Assumes 512-byte sectors.
      Destination_Area : sectors(1 .. Sector_Count, 1 .. 512 / 2)
      WITH
         Import   => true,
         Address  => Destination,
         Annotate => (GNATprove, False_Positive,
                      "object with constraints on bit representation *",
                      "The destination area has no fixed representation.");

      Drive_Selection  : CONSTANT drive_register :=
        (Block_Number   => (IF NOT LBA48_Mode THEN
            Shift_Right(Sector_Base, 24) AND 16#F# ELSE 0),
         Drive_Number   => boolean'pos(Secondary_Drive),
         Reserved_1     => 1,
         LBA_Addressing => true,
         Reserved_2     => 1);
   BEGIN
      LOOP -- Wait for the drive to get ready for commands before we begin.
         PIO_Status(Status, Secondary_Bus);

         IF
            Status.Drive_Error
         THEN
            RETURN;
         END IF;

         EXIT WHEN Status.Drive_Ready;
      END LOOP;

      Set_Bus_Drive(Drive_Selection, Secondary_Bus);

      IF -- Send the higher bytes if the LBA value is greater than 28 bits.
         LBA48_Mode
      THEN
         Set_Sector_Count
           (Shift_Right(Sector_Count, 8) AND 16#FF#, Secondary_Bus);
         Set_Low_LBA
           (Shift_Right(Sector_Base, 24) AND 16#FF#, Secondary_Bus);
         Set_Middle_LBA
           (Shift_Right(Sector_Base, 32) AND 16#FF#, Secondary_Bus);
         Set_High_LBA
           (Shift_Right(Sector_Base, 40) AND 16#FF#, Secondary_Bus);
      END IF;

      Set_Sector_Count
        (Sector_Count                 AND 16#FF#, Secondary_Bus);
      Set_Low_LBA
        (Sector_Base                  AND 16#FF#, Secondary_Bus);
      Set_Middle_LBA
        (Shift_Right(Sector_Base,  8) AND 16#FF#, Secondary_Bus);
      Set_High_LBA
        (Shift_Right(Sector_Base, 16) AND 16#FF#, Secondary_Bus);

      Send_Command((IF LBA48_Mode THEN sector_read_extra
         ELSE sector_read_retry), Secondary_Bus);

      FOR
         Sector IN Destination_Area'range(1)
      LOOP
         LOOP -- Wait for the drive to get ready for commands and data.
            PIO_Status(Status, Secondary_Bus);

            IF
               Status.Drive_Error
            THEN
               RETURN;
            END IF;

            EXIT WHEN Status.Drive_Ready AND THEN Status.Transfer_Ready;
         END LOOP;

         FOR
            Word IN Destination_Area'range(2)
         LOOP
            PRAGMA Warnings(GNATprove, off, "unused assignment",
               Reason => "It's modifying memory elsewhere directly.");
            Read_Data(Destination_Area(Sector, Word), Secondary_Bus);
         END LOOP;
      END LOOP;
   END PIO_Read;

   PROCEDURE PIO_Write
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Source          : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   IS
      PROCEDURE Set_Bus_Drive    IS NEW PIO_Output
        (generic_format => drive_register, Port => drive_port);

      PROCEDURE Set_Sector_Count IS NEW PIO_Output
        (generic_format => number, Port => sector_count_port);

      PROCEDURE Set_Low_LBA      IS NEW PIO_Output
        (generic_format => number, Port => LBA_low_port);
      PROCEDURE Set_Middle_LBA   IS NEW PIO_Output
        (generic_format => number, Port => LBA_middle_port);
      PROCEDURE Set_High_LBA     IS NEW PIO_Output
        (generic_format => number, Port => LBA_high_port);

      PROCEDURE Send_Command     IS NEW PIO_Output
        (generic_format => ATA_command, Port => command_status_port);

      PROCEDURE Send_Data        IS NEW PIO_Output
        (generic_format => number, Port => data_port, Word_Length => true);

      LBA48_Mode      : CONSTANT boolean :=
         Sector_Base > 2**28 - 1 OR ELSE Sector_Count > 255;
      Status          : drive_status;

      -- Assumes 512-byte sectors.
      Source_Area     : CONSTANT sectors(1 .. Sector_Count, 1 .. 512 / 2)
      WITH
         Import   => true,
         Address  => Source,
         Annotate => (GNATprove, False_Positive,
                      "object with constraints on bit representation *",
                      "The source area has no fixed representation.");

      Drive_Selection : CONSTANT drive_register :=
        (Block_Number   => Shift_Right(Sector_Base, 24) AND 16#F#,
         Drive_Number   => boolean'pos(Secondary_Drive),
         Reserved_1     => 1,
         LBA_Addressing => true,
         Reserved_2     => 1);
   BEGIN
      LOOP -- Wait for the drive to get ready for commands before we begin.
         PIO_Status(Status, Secondary_Bus);

         IF
            Status.Drive_Error
         THEN
            RETURN;
         END IF;

         EXIT WHEN Status.Drive_Ready;
      END LOOP;

      Set_Bus_Drive(Drive_Selection, Secondary_Bus);

      IF -- Send the higher bytes if the LBA value is greater than 28 bits.
         LBA48_Mode
      THEN
         Set_Sector_Count
           (Shift_Right(Sector_Count, 8) AND 16#FF#, Secondary_Bus);
         Set_Low_LBA
           (Shift_Right(Sector_Base, 24) AND 16#FF#, Secondary_Bus);
         Set_Middle_LBA
           (Shift_Right(Sector_Base, 32) AND 16#FF#, Secondary_Bus);
         Set_High_LBA
           (Shift_Right(Sector_Base, 40) AND 16#FF#, Secondary_Bus);
      END IF;

      Set_Sector_Count
        (Sector_Count                 AND 16#FF#, Secondary_Bus);
      Set_Low_LBA
        (Sector_Base                  AND 16#FF#, Secondary_Bus);
      Set_Middle_LBA
        (Shift_Right(Sector_Base,  8) AND 16#FF#, Secondary_Bus);
      Set_High_LBA
        (Shift_Right(Sector_Base, 16) AND 16#FF#, Secondary_Bus);

      Send_Command((IF LBA48_Mode THEN sector_write_extra
         ELSE sector_write_retry), Secondary_Bus);

      FOR
         Sector IN Source_Area'range(1)
      LOOP
         LOOP -- Wait for the drive to get ready for commands and data.
            PIO_Status(Status, Secondary_Bus);

            IF
               Status.Drive_Error
            THEN
               RETURN;
            END IF;

            EXIT WHEN Status.Drive_Ready AND THEN Status.Transfer_Ready;
         END LOOP;

         FOR
            Word IN Source_Area'range(2)
         LOOP
            Send_Data(Source_Area(Sector, Word), Secondary_Bus);
         END LOOP;
      END LOOP;

      LOOP -- Finally, wait for it to get ready for the cache flush command.
         PIO_Status(Status, Secondary_Bus);

         IF
            Status.Drive_Error
         THEN
            RETURN;
         END IF;

         EXIT WHEN Status.Drive_Ready;
      END LOOP;

      Send_Command((IF LBA48_Mode THEN cache_flush_extra
         ELSE cache_flush), Secondary_Bus);
   END PIO_Write;

END HAVK_ATA_PIO;
