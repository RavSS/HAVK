-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_phase-drive-gpt.adb                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation,
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Drive.GPT
IS
   FUNCTION UTF16_To_ASCII
     (UTF16_String : IN words)
      RETURN string
   IS
      ASCII_String : string(1 .. UTF16_String'length) :=
        (OTHERS => character'val(0));
   BEGIN
      FOR
         I IN UTF16_String'range
      LOOP
         EXIT WHEN UTF16_String(I) = 0; -- Assuming a null-terminated string.
         ASCII_String(positive(I)) :=
            character'val(UTF16_String(I) AND 16#FF#);
      END LOOP;

      RETURN ASCII_String;
   END UTF16_To_ASCII;

   FUNCTION Resolve_UUID
     (UUID : IN unique_identifier)
      RETURN unique_identifier
   IS
     (Time_Low              => UUID.Time_Low,
      Time_Middle           => UUID.Time_Middle,
      Time_High_And_Version => UUID.Time_High_And_Version,
      Sequence_And_Variant  =>
         Shift_Right(Intrinsics.Byte_Swap(UUID.Sequence_And_Variant), 48),
      Node                  =>
         Shift_Right(Intrinsics.Byte_Swap(UUID.Node), 16));

   FUNCTION Image
     (UUID : IN unique_identifier)
      RETURN string
   IS
      Imaged : CONSTANT string :=
         Image(UUID.Time_Low,              Base => 16, Padding => 08) & '-' &
         Image(UUID.Time_Middle,           Base => 16, Padding => 04) & '-' &
         Image(UUID.Time_High_And_Version, Base => 16, Padding => 04) & '-' &
         Image(UUID.Sequence_And_Variant,  Base => 16, Padding => 04) & '-' &
         Image(UUID.Node,                  Base => 16, Padding => 12);
   BEGIN
      RETURN Imaged(partition_name'range);
   END Image;

   PROCEDURE Get_Partition
     (New_Partition   : OUT partition;
      Index           : IN partition_index;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   IS
      -- Temporary types to save stack space.
      TYPE access_partition_table_header IS ACCESS partition_table_header;
      TYPE access_partition_table_sector IS ACCESS partition_table_sector;

      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object =>        partition_table_sector,
         name   => access_partition_table_sector);
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object =>        partition_table_header,
         name   => access_partition_table_header);

      FUNCTION To_Address
        (Table_Header_Access : IN access_partition_table_header)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic,
         Pre        => Table_Header_Access /= NULL,
         Post       => To_Address'result /= 0;

      FUNCTION To_Address
        (Table_Sector_Access : IN access_partition_table_sector)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic,
         Pre        => Table_Sector_Access /= NULL,
         Post       => To_Address'result /= 0;

      -- We need to read the header to get any partitions properly.
      Table_Header : access_partition_table_header :=
         NEW partition_table_header;

      -- To eventually store a 512-byte sector at this object.
      Table_Sector : access_partition_table_sector;
   BEGIN
      New_Partition := (Index => Index, OTHERS => <>);

      PIO_Read(1, 1, To_Address(Table_Header), Secondary_Bus, Secondary_Drive);

      IF -- TODO: Add error-checking here in regards to the CRC32 checksums.
         Table_Header.Signature /= "EFI PART"
      THEN
         New_Partition.Valid_GPT := false;

         Free(Table_Header);
         RETURN;
      ELSIF -- Checking if we can support this GPT layout.
         Table_Header.Revision NOT IN revision_support'range
      THEN
         New_Partition.Valid_GPT     := true;
         New_Partition.Supported_GPT := false;

         Free(Table_Header);
         RETURN;
      END IF;

      New_Partition.Valid_GPT := true;
      Table_Sector            := NEW partition_table_sector;

      PIO_Read(Table_Header.Table_LBA + (Index / 4), 1,
         To_Address(Table_Sector), Secondary_Bus, Secondary_Drive);

      IF
         Resolve_UUID(Table_Sector(Index MOD 4).Type_UUID) /= Empty_Partition
      THEN
         PRAGMA Warnings(GNATprove, off, "unreachable code",
            Reason => "This seems to be a bug; everything below is executed.");
         New_Partition      :=
           (Present         => true,
            Index           => Index,
            Name            => UTF16_To_ASCII
              (Table_Sector(Index MOD 4).Name)(New_Partition.Name'range),
            LBA_First       => Table_Sector(Index MOD 4).LBA_First,
            LBA_Last        => Table_Sector(Index MOD 4).LBA_Last,
            Drive_UUID      => Resolve_UUID
              (Table_Header.Drive_UUID),
            Type_UUID       => Resolve_UUID
              (Table_Sector(Index MOD 4).Type_UUID),
            Partition_UUID  => Resolve_UUID
              (Table_Sector(Index MOD 4).Partition_UUID),
            Secondary_Bus   => Secondary_Bus,
            Secondary_Drive => Secondary_Drive,
            Valid_GPT       => true,
            Supported_GPT   => true);
      END IF;

      Free(Table_Sector);
      Free(Table_Header);
   END Get_Partition;

   PROCEDURE Get_Partition
     (New_Partition   : OUT partition;
      Name            : IN string;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   IS
   BEGIN
      FOR
         Index IN partition_index'range
      LOOP
         Get_Partition(New_Partition, Index, Secondary_Bus, Secondary_Drive);
         EXIT WHEN New_Partition.Present AND THEN
            New_Partition.Name(Name'range) = Name;
      END LOOP;
   END Get_Partition;

END HAVK_Kernel.Drive.GPT;
