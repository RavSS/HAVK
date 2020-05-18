-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_phase-drive.ads                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

-- This package facilitates a very basic disk driver (ATA PIO) that is to be
-- used until the operating system part of HAVK can load better drivers which
-- use the controllers on the PCI bus instead.
-- READ: https://wiki.osdev.org/ATA_PIO_Mode
-- TODO: Only supports polling, not interrupt-driven operation.
-- TODO: This should really be a user-mode task that utilises system calls and
-- IPC to get and send drive data around, but that can come much later.
-- TODO: This makes a relatively safe assumption that the sector size is 512
-- bytes and not any other value. For newer devices, the sector size can be
-- 4096 bytes. For now, 512 is hardcoded into many places, including the child
-- packages of this package. To resolve it, I would need to interpret the
-- Identify command's 512-byte "IDENTIFY DEVICE data" structure/record.
PACKAGE HAVK_Kernel.Drive
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      Drive_State
      WITH
         External => (Async_Readers, Async_Writers,
                         Effective_Reads, Effective_Writes)
   )
IS
   -- A type for indicating the LBA (logical block addressing) value, which is
   -- a CHS (cylinder-head-sector) tuple. Usually, each sector (the LBA value)
   -- represents 512 bytes, but not always. The current standard is LBA48.
   -- Note that this should not be seen as a memory address, just a number.
   SUBTYPE logical_block_address IS number RANGE 0 .. 2**48 - 1;

   -- Reads values from the LBA/sector to a destination in memory.
   -- A sector count of zero is technically valid, but to avoid confusion,
   -- it is not permitted, as it either means 256 sectors in LBA28 or 65536
   -- sectors in LBA48, of which can vary depending on the base sector.
   PRAGMA Warnings(GNATprove, off, "unused variable ""Destination""",
      Reason => "Only used for overlaying an array on the specific address.");
   PROCEDURE PIO_Read
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Destination     : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   WITH
      Global => (In_Out => (Intrinsics.CPU_Port_State, Drive_State)),
      Pre    => Sector_Count IN 1 .. 2**16 - 1;

   -- Writes values from the memory source to the specified drive sectors.
   -- The same properties of `PIO_Read()` also apply here.
   PRAGMA Warnings(GNATprove, off, "unused variable ""Source""",
      Reason => "Only used for overlaying an array on the specific address.");
   PROCEDURE PIO_Write
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Source          : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   WITH
      Global => (In_Out => (Intrinsics.CPU_Port_State, Drive_State)),
      Pre    => Sector_Count IN 1 .. 2**16 - 1;

PRIVATE
   -- There's two base ports for each bus: one for all I/O operations and the
   -- other for controlling interrupts and selecting drives.
   Disk_IO_Base_1 : CONSTANT := 16#1F0#;
   Control_Base_1 : CONSTANT := 16#3F6#;
   Disk_IO_Base_2 : CONSTANT := 16#170#;
   Control_Base_2 : CONSTANT := 16#376#;

   -- Contains offsets for the two base ports in regards to I/O and control.
   -- Port descriptions are the same for both buses. Each bus has two drives
   -- on it. I'll calculate the offsets elsewhere.
   TYPE ATA_port IS
      ---- Disk I/O ports.
      -- Read and write: Data bytes are read or written to here.
     (data_port,
      -- Read: Used for obtaining error information.
      -- Write: Used for enabling features of the device.
      feature_error_port,
      -- Read and write: The amount of sectors to read or write.
      sector_count_port,
      -- Read and write: The low absolute LBA value.
      LBA_low_port,
      -- Read and write: The middle absolute LBA value.
      LBA_middle_port,
      -- Read and write: The high absolute LBA value.
      LBA_high_port,
      -- Read and write: Selects the drive or the head.
      drive_port,
      -- Read: Used for reading the current status (causes interrupt).
      -- Write: ATA commands are sent here.
      command_status_port,
      ---- Control ports.
      -- Read: Gives the status without an interrupt.
      -- Write: Used for controlling IRQs and resets.
      control_status_port,
      -- Read only. Used for checking drive selection.
      select_port)
   WITH
      Size => 8;
   FOR ATA_port USE
     (data_port           => 0,
      feature_error_port  => 1,
      sector_count_port   => 2,
      LBA_low_port        => 3,
      LBA_middle_port     => 4,
      LBA_high_port       => 5,
      drive_port          => 6,
      command_status_port => 7,
      control_status_port => 8,  -- Not the real offset, which is +0.
      select_port         => 9); -- Not the real offset, which is +1.

   -- There's a large number of ATA commands, but I've only included a few of
   -- them for now. LBA48 extension commands have been suffixed with "extra".
   -- READ: https://wiki.osdev.org/ATA_Command_Matrix
   TYPE ATA_command IS
      -- Does nothing.
     (no_command,
      -- Read sectors and retry if necessary.
      sector_read_retry,
      -- Read sectors but do not retry.
      sector_read_no_retry,
      -- Read sectors (used in LBA48 mode).
      sector_read_extra,
      -- Write sectors and retry if necessary.
      sector_write_retry,
      -- Write sectors but do not retry.
      sector_write_no_retry,
      -- Write sectors (used in LBA48 mode).
      sector_write_extra,
      -- Flushes the cache, which is recommended (on OSDev) after each write.
      cache_flush,
      -- The same as "cache_flush", but for LBA48 mode.
      cache_flush_extra)
   WITH
      Size => 8;
   FOR ATA_command USE
     (no_command            => 16#00#,
      sector_read_retry     => 16#20#,
      sector_read_no_retry  => 16#21#,
      sector_read_extra     => 16#24#,
      sector_write_retry    => 16#30#,
      sector_write_no_retry => 16#31#,
      sector_write_extra    => 16#34#,
      cache_flush           => 16#E7#,
      cache_flush_extra     => 16#EA#);

   -- This is the format read from the error port.
   TYPE error_register IS RECORD
      -- When true, the address mark (or sector identity) was not found.
      Address_Mark_Not_Found : boolean;
      -- When true, the first track was not found.
      Track_Zero_Not_Found   : boolean;
      -- When true, a command was aborted or not recognised.
      Aborted                : boolean;
      -- When true, a disk change request was made i.e. disk ejection.
      Media_Change_Request   : boolean;
      -- When true, a requested address was not found.
      Identity_Not_Found     : boolean;
      -- When true, the disk has been completely changed.
      Media_Changed          : boolean;
      -- When true, there has been a data error which could not be corrected.
      Data_Error             : boolean;
      -- When true, a bad block on the drive was encountered.
      Bad_Block_Detected     : boolean;
   END RECORD;
   FOR error_register USE RECORD
      Address_Mark_Not_Found AT 0 RANGE 0 .. 0;
      Track_Zero_Not_Found   AT 0 RANGE 1 .. 1;
      Aborted                AT 0 RANGE 2 .. 2;
      Media_Change_Request   AT 0 RANGE 3 .. 3;
      Identity_Not_Found     AT 0 RANGE 4 .. 4;
      Media_Changed          AT 0 RANGE 5 .. 5;
      Data_Error             AT 0 RANGE 6 .. 6;
      Bad_Block_Detected     AT 0 RANGE 7 .. 7;
   END RECORD;

   -- This is the format read and written to the drive port. It is mostly for
   -- selecting which drive on the bus you wish to use and how to access it.
   TYPE drive_register IS RECORD
      -- For CHS mode, this is bits 3:0 of the head number, but for LBA mode,
      -- this is bits 27:24 of the block number. We are only focusing on LBA.
      Block_Number   : number RANGE 0 .. 2**4 - 1;
      -- Only two drives are supported in PIO mode.
      Drive_Number   : number RANGE 0 .. 1;
      -- Always set.
      Reserved_1     : number RANGE 1 .. 1;
      -- If true, then the drive uses LBA addressing instead of CHS addressing.
      LBA_Addressing : boolean;
      -- Always set.
      Reserved_2     : number RANGE 1 .. 1;
   END RECORD;
   FOR drive_register USE RECORD
      Block_Number       AT 0 RANGE 0 .. 3;
      Drive_Number       AT 0 RANGE 4 .. 4;
      Reserved_1         AT 0 RANGE 5 .. 5;
      LBA_Addressing     AT 0 RANGE 6 .. 6;
      Reserved_2         AT 0 RANGE 7 .. 7;
   END RECORD;

   -- This is the format read from the status port (both IRQ and non-IRQ).
   TYPE status_register IS RECORD
      -- When true, an error has occurred.
      Error          : boolean;
      -- Always zero.
      Zeroed         : number RANGE 0 .. 0;
      -- Data has been corrected. Apparently always false/zero.
      Corrected_Data : boolean;
      -- When true, the drive is ready to send us data and/or receive our data,
      -- meaning that it is waiting to transfer it.
      Ready          : boolean;
      -- When true, a command is being serviced. The details on this are
      -- somewhat obscure, but I think they seem mostly irrelevant to us.
      Servicing      : boolean;
      -- When true, a fault has occurred with the currently selected drive.
      -- This is separate from the error bit.
      Drive_Faulted  : boolean;
      -- When true, the drive is spinning or powered on etc. This will become
      -- false upon an error.
      Active         : boolean;
      -- When true, the drive is busy transferring data. If the drive is busy,
      -- then the other information here is apparently out of date.
      Busy           : boolean;
   END RECORD;
   FOR status_register USE RECORD
      Error              AT 0 RANGE 0 .. 0;
      Zeroed             AT 0 RANGE 1 .. 1;
      Corrected_Data     AT 0 RANGE 2 .. 2;
      Ready              AT 0 RANGE 3 .. 3;
      Servicing          AT 0 RANGE 4 .. 4;
      Drive_Faulted      AT 0 RANGE 5 .. 5;
      Active             AT 0 RANGE 6 .. 6;
      Busy               AT 0 RANGE 7 .. 7;
   END RECORD;

   -- This is the format written to the control port ("Device Control").
   -- It controls both drive devices on a bus.
   TYPE control_register IS RECORD
      -- Always zeroed.
      Zeroed          : number RANGE 0 .. 0;
      -- When true, the device will not send any interrupts.
      No_Interrupts   : boolean;
      -- To perform a mass reset, this must be set to true, and after several
      -- microseconds, set this to false to re-enable it. This resets all
      -- drives on the bus.
      Reset_Drives    : boolean;
      -- Bits not used for anything.
      Reserved        : number RANGE 0 .. 2**4 - 1;
      -- When true, you can read back the higher bytes when using LBA48 mode.
      -- Otherwise, you will only be able to read the lower bytes (LBA28).
      High_Order_Byte : boolean;
   END RECORD;
   FOR control_register USE RECORD
      Zeroed              AT 0 RANGE 0 .. 0;
      No_Interrupts       AT 0 RANGE 1 .. 1;
      Reset_Drives        AT 0 RANGE 2 .. 2;
      Reserved            AT 0 RANGE 3 .. 6;
      High_Order_Byte     AT 0 RANGE 7 .. 7;
   END RECORD;

   -- This is the format written to the select port ("Drive Address").
   -- It mostly indicates which drive is selected and how so.
   TYPE select_register IS RECORD
      -- When true, drive zero (master) is not selected.
      Drive_0_Deselected   : boolean;
      -- When true, drive one (slave) is not selected.
      Drive_1_Deselected   : boolean;
      -- Contains the selected drive of the selected head in one's complement.
      -- In LBA mode, it contains bits 27:24 of the LBA.
      -- Invert all the bits to get the value normally.
      Drive_Head_Selection : number RANGE 0 .. 2**4 - 1;
      -- When true, the write gate is high, which means there is not anything
      -- being written to the selected drive.
      Not_Writing          : boolean;
      -- Not used, although it apparently indicates a high impedance state.
      Reserved             : number RANGE 0 .. 1;
   END RECORD;
   FOR select_register USE RECORD
      Drive_0_Deselected       AT 0 RANGE 0 .. 0;
      Drive_1_Deselected       AT 0 RANGE 1 .. 1;
      Drive_Head_Selection     AT 0 RANGE 2 .. 5;
      Not_Writing              AT 0 RANGE 6 .. 6;
      Reserved                 AT 0 RANGE 7 .. 7;
   END RECORD;

   -- A short abstraction for the status of the drive.
   TYPE drive_status IS RECORD
      -- The drive is waiting to accept ATA commands.
      Drive_Ready    : boolean := false;
      -- The drive is waiting to send/receive data.
      Transfer_Ready : boolean := false;
      -- The drive is inactive and has halted.
      Drive_Error    : boolean := false;
   END RECORD;

   -- A type that describes an array of sectors. The second dimension will
   -- usually be 256 elements long (for 512 byte sectors).
   TYPE sectors IS ARRAY(number RANGE <>, number RANGE <>)
      OF number RANGE 0 .. 2**16 - 1
   WITH
      Component_Size => 16;

   -- Reads from a ATA PIO port and gets data in a certain record format.
   GENERIC
      TYPE generic_format IS PRIVATE;
      Port          : ATA_port;
      Word_Length   : boolean := false;
   PROCEDURE PIO_Input
     (Data          : OUT generic_format;
      Secondary_Bus : IN boolean := false);

   -- Writes a variable's data to a certain ATA PIO port.
   GENERIC
      TYPE generic_format IS PRIVATE;
      Port          : ATA_port;
      Word_Length   : boolean := false;
   PROCEDURE PIO_Output
     (Data          : IN generic_format;
      Secondary_Bus : IN boolean := false);

   -- Returns true if the device is ready to send or receive data.
   -- There's options on which you can check what is ready. That is either the
   -- disk being ready to transfer and/or the drive being ready to respond.
   PROCEDURE PIO_Status
     (Current_Status : OUT drive_status;
      Secondary_Bus  : IN boolean := false)
   WITH
      Global => (In_Out => Intrinsics.CPU_Port_State, Input => Drive_State);

END HAVK_Kernel.Drive;
