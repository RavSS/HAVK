-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_phase-drive-gpt.ads                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   SPARK.Heap;

-- A package for giving understanding of the GPT partition layout, which is
-- what I'll use over the older MBR.
-- READ: UEFI Specification, Version 2.8 - Page 116.
-- READ: https://en.wikipedia.org/wiki/GUID_Partition_Table
-- READ: https://wiki.osdev.org/GPT
PACKAGE HAVK_Kernel.Drive.GPT
WITH
   Preelaborate => true
IS
   -- The amount of partitions GPT supports (as of now) is 128.
   SUBTYPE partition_index IS number RANGE 0 .. 127;

   -- The partition name string for GPT can only have 36 UTF-16 code points.
   -- By doing some conversion, I can turn that into ASCII most of the time.
   SUBTYPE partition_name IS string(1 .. 36);

   -- A Universally Unique Identifier (UUID) type. This is also called the
   -- Globally Unique Identifier (GUID) by Microsoft. It does what its name
   -- implies and it is a 128-bit value. Used for uniquely identifying
   -- file system types, partitions, or drives. The first three dash-delimited
   -- field's values are stored in little-endian and the second two
   -- dash-delimited field's values are stored in big-endian. The fields
   -- themselves are mostly irrelevant for our purpose and there's many
   -- different versions of UUIDs, but for now, we only care about the numeric
   -- value of the UUID itself for comparison against other predefined UUIDs.
   -- READ: https://en.wikipedia.org/wiki/Universally_unique_identifier
   TYPE unique_identifier IS RECORD
      -- Lower 32 bits of a time at which the UUID was created. Little-endian.
      Time_Low              : number RANGE 0 .. 2**32 - 1 := 0;
      -- Middle 16 bits of a time at which the UUID was created. Little-endian.
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
   -- functions. Main difference is attempt at making the partition name into
   -- ASCII and fixing up the UUID's endianness.
   TYPE partition IS RECORD
      Present         : boolean               := false;
      Index           : partition_index       := 0;
      Name            : partition_name        := (OTHERS => character'val(0));
      LBA_First       : logical_block_address := logical_block_address'first;
      LBA_Last        : logical_block_address := logical_block_address'first;
      Drive_UUID      : unique_identifier     := (OTHERS => 0);
      Type_UUID       : unique_identifier     := (OTHERS => 0);
      Partition_UUID  : unique_identifier     := (OTHERS => 0);
      Secondary_Bus   : boolean               := false;
      Secondary_Drive : boolean               := false;
      Valid_GPT       : boolean               := false;
      Supported_GPT   : boolean               := false;
   END RECORD
   WITH
      Dynamic_Predicate => (IF NOT Valid_GPT THEN NOT Supported_GPT),
      Pack              => true; -- Save some space.

   -- Returns information for a particular requested partition via index.
   PROCEDURE Get_Partition
     (New_Partition   : OUT partition;
      Index           : IN partition_index;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   WITH
      Global => (In_Out => (Intrinsics.CPU_Port_State, Drive_State,
                            SPARK.Heap.Dynamic_Memory));

   -- Returns information for a particular requested partition via name.
   -- It will retrieve the first partition that matches the name.
   PROCEDURE Get_Partition
     (New_Partition   : OUT partition;
      Name            : IN string;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   WITH
      Global => (In_Out => (Intrinsics.CPU_Port_State, Drive_State,
                            SPARK.Heap.Dynamic_Memory)),
      Pre    => Name'first = partition_name'first AND THEN
                Name'last <= partition_name'last;

   -- Returns a hexidecimal representation of a UUID with dashes and no
   -- additional padding beyond that of a UUID's maximum values.
   FUNCTION Image
     (UUID : IN unique_identifier)
      RETURN string
   WITH
      Post => Image'result'first = partition_name'first AND THEN
              Image'result'last  = partition_name'last;

PRIVATE
   -- This is the GPT revision/version I currently support. The reasoning is
   -- that I've hardcoded in many values, as Apple for example (in "Secrets of
   -- the GPT") recommends not assuming 512-byte sectors or 128 partition
   -- entries of 128 bytes. It is current as of version 2.8 of the UEFI
   -- specification. UEFI recommends expecting either 512-byte sectors or the
   -- newer 4096-byte sectors. Ultimately, I will need to use the ATA IDENTIFY
   -- command to figure that out.
   SUBTYPE revision_support IS number RANGE 16#00_01_00_00# .. 16#00_01_00_00#;

   -- The record that is found at LBA value 1 after the protective MBR.
   TYPE partition_table_header IS RECORD
      -- The first field is the signature field, which should be "EFI PART".
      Signature         : string(1 .. 8) := (OTHERS => character'val(0));
      -- The revision number of the GPT header.
      Revision          : number RANGE 0 .. 2**32 - 1 := 0;
      -- The size of this very header. Could provide a way to check whether or
      -- not new fields have been added if it doesn't include the blank space
      -- at the end of this header to fill in the entire sector.
      Header_Size       : number RANGE 0 .. 2**32 - 1 := 0;
      -- The CRC32 checksum of this record to verify that it's not corrupt.
      Header_Checksum   : number RANGE 0 .. 2**32 - 1 := 0;
      -- A reserved field.
      Reserved          : number RANGE 0 .. 2**32 - 1 := 0;
      -- The LBA value of where this record was found. This should be 1.
      Header_LBA        : logical_block_address := logical_block_address'first;
      -- An LBA value of the backup partition header and partition table.
      -- This is mirrored, so the header is at the end of the drive and before
      -- it would be the partition table.
      Header_Mirror_LBA : logical_block_address := logical_block_address'first;
      -- The first LBA value that is usable for partitions. Should often come
      -- after the partition table has ended.
      Usable_First_LBA  : logical_block_address := logical_block_address'first;
      -- The last LBA value that is usable for partitions. Usually before the
      -- mirrored partition table.
      Usable_Last_LBA   : logical_block_address := logical_block_address'first;
      -- A unique identifier for the particular drive.
      Drive_UUID        : unique_identifier := (OTHERS => 0);
      -- The base LBA of the partition table, which usually comes after this
      -- record. I believe this is at least 2 for MBR compatibility.
      Table_LBA         : logical_block_address := logical_block_address'first;
      -- The maximum amount of potential entries in the partition table. This
      -- is supposed to be a maximum of 128 as of writing this.
      Table_Entries     : number RANGE 0 .. 2**32 - 1 := 0;
      -- The size of the partition table array. It should fit into 32 sectors
      -- as of writing this.
      Table_Entry_Size  : number RANGE 0 .. 2**32 - 1 := 0;
      -- The CRC32 checksum of the partition table.
      Table_Checksum    : number RANGE 0 .. 2**32 - 1 := 0;
      -- Below here is a large reserved field that zero-fills up to the end of
      -- the block/sector size. 512 bytes is the minimum we can expect.
      Zeroed            : bytes(1 .. 420) := (OTHERS => 0);
   END RECORD;
   FOR partition_table_header USE RECORD
      Signature            AT 00 RANGE 0 .. 0063;
      Revision             AT 08 RANGE 0 .. 0031;
      Header_Size          AT 12 RANGE 0 .. 0031;
      Header_Checksum      AT 16 RANGE 0 .. 0031;
      Reserved             AT 20 RANGE 0 .. 0031;
      Header_LBA           AT 24 RANGE 0 .. 0063;
      Header_Mirror_LBA    AT 32 RANGE 0 .. 0063;
      Usable_First_LBA     AT 40 RANGE 0 .. 0063;
      Usable_Last_LBA      AT 48 RANGE 0 .. 0063;
      Drive_UUID           AT 56 RANGE 0 .. 0127;
      Table_LBA            AT 72 RANGE 0 .. 0063;
      Table_Entries        AT 80 RANGE 0 .. 0031;
      Table_Entry_Size     AT 84 RANGE 0 .. 0031;
      Table_Checksum       AT 88 RANGE 0 .. 0031;
      Zeroed               AT 92 RANGE 0 .. 3359;
   END RECORD;

   -- Each partition (entry) has a number of attribute flags attached to it.
   -- TODO: Fill out the other flag types that aren't standard.
   TYPE partition_table_flags IS RECORD
      -- When true, the system needs this partition to function properly.
      Required        : boolean := false;
      -- When true, the UEFI firmware should not read this partition.
      Ignore          : boolean := false;
      -- When true, it can be booted by the older BIOS.
      BIOS_Bootable   : boolean := false;
      -- Flags that are reserved and will not be set.
      Reserved_Flags  : number RANGE 0 .. 2**45 - 1 := 0;
      -- A whole variety of non-standard flags. Microsoft (Windows NT) has
      -- their own ones and Google (Chrome OS) has their own ones as well.
      Undefined_Flags : number RANGE 0 .. 2**16 - 1 := 0;
   END RECORD;
   FOR partition_table_flags USE RECORD
      Required            AT 0 RANGE 00 .. 00;
      Ignore              AT 0 RANGE 01 .. 01;
      BIOS_Bootable       AT 0 RANGE 02 .. 02;
      Reserved_Flags      AT 0 RANGE 03 .. 47;
      Undefined_Flags     AT 0 RANGE 48 .. 63;
   END RECORD;

   -- This makes up the partition table after the header. There is a maximum
   -- of 128 table entries.
   TYPE partition_table_entry IS RECORD
      -- A 128-bit value that describes the type of partition this entry
      -- belongs to. It's mostly used to distinguish filesystems like e.g. FAT.
      Type_UUID      : unique_identifier := (OTHERS => 0);
      -- A unique 128-bit identifier for the partition itself.
      Partition_UUID : unique_identifier := (OTHERS => 0);
      -- This indicates the first LBA value that belongs to the partition.
      -- The partition itself will refer to it as "LBA 0" due to
      -- relative/logical addressing as opposed to absolute.
      LBA_First      : logical_block_address := logical_block_address'first;
      -- The last LBA value belonging to the partition. This is also
      -- relative/logical like with the partition's first LBA value.
      LBA_Last       : logical_block_address := logical_block_address'first;
      -- A number of attribute flags that indicate the partition's features.
      -- Most of them are not standard.
      Flags          : partition_table_flags;
      -- A UTF-16 (little-endian of course) string that show the partition's
      -- name. Because this is not ASCII or UTF-8, printing this as is will
      -- show any text with spaces in-between due to differing code point size.
      -- This is actually only 36 UTF-16 code points. By ignoring all the odd
      -- index bytes, you can most likely print the name without the above
      -- issue. See how I did it in HAVK's UEFI bootloader.
      Name           : words
        (number(partition_name'first) .. number(partition_name'last)) :=
        (OTHERS => 0);
   END RECORD;
   FOR partition_table_entry USE RECORD
      Type_UUID      AT 00 RANGE 0 .. 127;
      Partition_UUID AT 16 RANGE 0 .. 127;
      LBA_First      AT 32 RANGE 0 .. 063;
      LBA_Last       AT 40 RANGE 0 .. 063;
      Flags          AT 48 RANGE 0 .. 063;
      Name           AT 56 RANGE 0 .. 575;
   END RECORD;

   -- Each sector can essentially fit 4 table entries in it.
   TYPE partition_table_sector IS ARRAY(number RANGE 0 .. 3)
      OF partition_table_entry
   WITH
      Component_Size => 1024, -- Entries are 128 bytes minimum as of now.
      Size           => 4096; -- 512 bytes minimum (most common sector size).

   -- Attempts to shorten the UTF-16 code point to only a single byte in the
   -- hopes that it will become an ASCII compatible string. The passed word
   -- array is presumed to be null-terminated.
   FUNCTION UTF16_To_ASCII
     (UTF16_String : IN words)
      RETURN string
   WITH
      Pre => UTF16_String'first = number(positive'first) AND THEN
             UTF16_String'last <= number(positive'last);

   -- The order of the UUID when read off the drives is not correct, as they're
   -- in mixed endian. This corrects that by taking in the raw value and
   -- returning the appropriate one. Only the sequence/variant and node fields
   -- need to be byte swapped into little-endian (which is x86's native
   -- endianness). The rest are unmodified.
   FUNCTION Resolve_UUID
     (UUID : IN unique_identifier)
      RETURN unique_identifier
   WITH
      Inline => true;

END HAVK_Kernel.Drive.GPT;
