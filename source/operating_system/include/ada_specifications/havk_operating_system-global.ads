-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-global.ads                       --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

-- This package contains child packages which have data formats (type
-- declarations/definitions) in them. It is primarily created for IPC purposes
-- and message interfaces and it contains no body implementation.
PACKAGE HAVK_Operating_System.Global
WITH
   Pure => true
IS
   PACKAGE ATA
   IS
      -- A type for indicating the LBA (logical block addressing) value, which
      -- is a CHS (cylinder-head-sector) tuple. Usually, each sector (the LBA
      -- value) represents 512 bytes, but not always. The current standard is
      -- LBA48. Note that this should not be seen as a memory address, just a
      -- number.
      SUBTYPE logical_block_address IS number RANGE 0 .. 2**48 - 1;

      -- Send this message to the ATA task with any headers and it will respond
      -- with two headerless messages comprising a 512-byte sector. Sending an
      -- LBA value greater than 48 bits is invalid and the task will not
      -- respond back.
      TYPE PIO_request IS RECORD
         Sector_Base     : number;
         Secondary_Bus   : boolean;
         Secondary_Drive : boolean;
         Write_Request   : boolean;
         Padding         : bytes(12 .. 256);
      END RECORD
      WITH
         Size        => 2048,
         Object_Size => 2048;
   END ATA;

   PACKAGE Drive_Manager
   IS
      -- The amount of partitions GPT supports (as of now) is 128.
      SUBTYPE partition_index IS number RANGE 0 .. 127;

      -- The partition name string for GPT can only have 36 UTF-16 code points.
      SUBTYPE partition_name_string IS wide_string(1 .. 36);

      -- A Universally Unique Identifier (UUID) type. This is also called the
      -- Globally Unique Identifier (GUID) by Microsoft. It does what its name
      -- implies and it is a 128-bit value. Used for uniquely identifying file
      -- system types, partitions, or drives. The first three dash-delimited
      -- field's values are stored in little-endian and the second two
      -- dash-delimited field's values are stored in big-endian. The fields
      -- themselves are mostly irrelevant for our purpose and there's many
      -- different versions of UUIDs, but for now, we only care about the
      -- numeric value of the UUID itself for comparison against other
      -- predefined UUIDs.
      -- READ: https://en.wikipedia.org/wiki/Universally_unique_identifier
      TYPE unique_identifier IS RECORD
         -- Lower 32 bits of a time at which the UUID was created.
         -- Little-endian.
         Time_Low              : number RANGE 0 .. 2**32 - 1 := 0;
         -- Middle 16 bits of a time at which the UUID was created.
         -- Little-endian.
         Time_Middle           : number RANGE 0 .. 2**16 - 1 := 0;
         -- Higher 16 bits of a time at which the UUID was created. The most
         -- significant 4 bits indicate the version. Little-endian.
         Time_High_And_Version : number RANGE 0 .. 2**16 - 1 := 0;
         -- The "clock sequence" is usually a random value. The details are not
         -- relevant for us. Big-endian.
         Sequence_And_Variant  : number RANGE 0 .. 2**16 - 1 := 0;
         -- Another random value, apparently this is often a MAC address.
         -- Big-endian.
         Node                  : number RANGE 0 .. 2**48 - 1 := 0;
      END RECORD;
      FOR unique_identifier USE RECORD
         Time_Low                 AT 00 RANGE 0 .. 31;
         Time_Middle              AT 04 RANGE 0 .. 15;
         Time_High_And_Version    AT 06 RANGE 0 .. 15;
         Sequence_And_Variant     AT 08 RANGE 0 .. 15;
         Node                     AT 10 RANGE 0 .. 47;
      END RECORD;

      -- These are a few of the defined UUIDs for partition types.
      Empty_Partition      : CONSTANT unique_identifier :=
        (16#00000000#, 16#0000#, 16#0000#, 16#0000#, 16#000000000000#);
      EFI_Partition        : CONSTANT unique_identifier := -- UEFI FAT boot.
        (16#C12A7328#, 16#F81F#, 16#11D2#, 16#BA4B#, 16#00A0C93EC93B#);
      Basic_Data_Partition : CONSTANT unique_identifier := -- FAT, NTFS, etc.
        (16#EBD0A0A2#, 16#B9E5#, 16#4433#, 16#87C0#, 16#68B6B72699C7#);
      Linux_Data_Partition : CONSTANT unique_identifier := -- EXT4, XFS, etc.
        (16#0FC63DAF#, 16#8483#, 16#4772#, 16#8E79#, 16#3D69D8477DE4#);
      Linux_Root_Partition : CONSTANT unique_identifier := -- x86-64 EXT4, etc.
        (16#4F68BCE3#, 16#E8CD#, 16#4DB1#, 16#96E7#, 16#FBCAF984B709#);

      -- A refactored partition entry format returned by the partition seeker
      -- functions. Main difference is fixing up the UUID's endianness. Also
      -- lets us change the format in the future, separating it from the GPT
      -- standard.
      TYPE partition IS RECORD
         Present         : boolean               := false;
         Index           : partition_index       := 0;
         Name            : partition_name_string :=
           (OTHERS => wide_character'val(0));
         LBA_First       : ATA.logical_block_address :=
            ATA.logical_block_address'first;
         LBA_Last        : ATA.logical_block_address :=
            ATA.logical_block_address'first;
         Drive_UUID      : unique_identifier     := (OTHERS => 0);
         Type_UUID       : unique_identifier     := (OTHERS => 0);
         Partition_UUID  : unique_identifier     := (OTHERS => 0);
         Secondary_Bus   : boolean               := false;
         Secondary_Drive : boolean               := false;
         Valid_GPT       : boolean               := false;
         Supported_GPT   : boolean               := false;
      END RECORD
      WITH
         Dynamic_Predicate => (IF NOT Valid_GPT THEN NOT Supported_GPT);

      TYPE partitioned_PIO_request IS RECORD
         Sector_Base       : number  := 0;
         Secondary_Bus     : boolean := false;
         Secondary_Drive   : boolean := false;
         Write_Request     : boolean := false;
         Partition_Data    : partition;
         Padding           : bytes(181 .. 256);
      END RECORD
      WITH
         Size        => 2048,
         Object_Size => 2048;
   END Drive_Manager;

   PACKAGE FAT
   IS
   END FAT;

END HAVK_Operating_System.Global;