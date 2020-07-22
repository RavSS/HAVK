-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-drive-fat.ads                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Drive.GPT;

-- A package that details the FAT file system (mostly FAT16 for early boot
-- purposes). For now, it merely attempts to read the file system, but it
-- cannot create/make a new FAT partition or write new files.
-- READ: https://en.wikipedia.org/wiki/Design_of_the_FAT_file_system
-- READ: https://wiki.osdev.org/FAT
-- TODO: Has no write support as of now.
-- TODO: Like with the parent package, this is only to be here temporarily, as
-- it is a driver running in ring 0. Chances of it failing are minimal and
-- mostly depend on hardware, but it will soon have user space interaction.
-- TODO: Adding FAT32 support doesn't seem like too much work once FAT16
-- support has been added in, so it may be okay to implement support for it.
-- TODO: Lacks a lot of error checking and the contracts provided for
-- verification aren't fully adequate, although runtime errors in the Ada code
-- itself are unlikely to happen.
-- TODO: Make a virtual file system to abstract from FAT's specific details.
PACKAGE HAVK_Kernel.Drive.FAT
IS
   -- A context/state type. What's inside should be hidden from the rest of the
   -- operating system to avoid modifications by mistake, as it's not relevant
   -- to them.
   TYPE file_system IS PRIVATE;

   -- The four major versions of the FAT file system. Note that "VFAT" is
   -- essentially just an extension to do with long file names.
   TYPE version IS
     (unknown,
      FAT12, -- TODO: Unsupported. Just needs file allocation table parsing.
      FAT16, -- TODO: Reading is supported, but writing is not yet supported.
      FAT32, -- TODO: Unsupported. Requires new header logic and calculations.
      exFAT) -- TODO: Unsupported. Requires new header logic and calculations.
   WITH
      Default_Value => unknown;

   -- A restructured version of the DOS 8.3 file format for convenience.
   TYPE file IS RECORD
      -- The DOS 8.3 file name (also including the extension dot).
      Name : string(1 .. 12);
      -- The size of the entire file.
      Size : number;
      -- Add more fields here as necessary.
   END RECORD;

   -- Returns the FAT version from a FAT context.
   FUNCTION Get_FAT_Version
     (FAT_Context : IN file_system)
      RETURN version;

   -- Attempts to scan a valid partition for a FAT file system and returns it
   -- in the first parameter. If no file system was found, the error will be
   -- recorded in the "file_system" type itself.
   PROCEDURE Get_File_System
     (New_FAT_Context : OUT file_system;
      FAT_Partition   : IN GPT.partition)
   WITH
      Pre => FAT_Partition.Present;

   -- Checks for the existence of a file or dictionary.
   PROCEDURE Check_File
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Error_Status : OUT error;
      Found_File   : OUT file)
   WITH
      Pre  => Get_FAT_Version(FAT_Context) = FAT16 AND THEN
              Path_Name'first = 1                  AND THEN
              Path_Name'last IN Path_Name'first .. 255,
      Post => Error_Status IN no_error | path_error;

   -- Reads a file from the FAT file system and puts it in a specific
   -- destination. The maximum path name of a file is 255 characters. If
   -- the base byte and/or byte size parameters do not correspond to a file
   -- properly, then the procedure will silently return. Returns "path_error"
   -- if the path does not resolve to the path name or "size_error" if the
   -- byte offsets are invalid.
   PROCEDURE Read_File
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Destination  : IN address;
      Error_Status : OUT error;
      Base_Byte    : IN number := 1;
      Byte_Size    : IN number := 0)
   WITH
      Pre  => Get_FAT_Version(FAT_Context) = FAT16 AND THEN
              Destination /= 0                     AND THEN
              Base_Byte /= 0                       AND THEN
              Path_Name'first = 1                  AND THEN
              Path_Name'last IN Path_Name'first .. 255,
      Post => Error_Status IN no_error | path_error | size_error;

   -- The path separator I've arbitrarily chosen.
   Separator : CONSTANT character := '>';

PRIVATE
   FAT_Tag   : CONSTANT string := "FAT";

   -- This details the BIOS parameter block (BPB). It is found in sector zero.
   -- All references to sectors are logical.
   TYPE BIOS_parameter_block IS RECORD
      -- This field contains instructions which jump over the BPB and EBPB.
      -- The instructions are `JMP 0x3E` and a NOP after it, which should
      -- decompile down to 0xEB3C90 (0x903CEB in little-endian, which we will
      -- see).
      Jump_Code           : number RANGE 0 .. 2**24 - 1 := 0;
      -- Identifies the version of DOS being used or the software that created
      -- the current file system. Apparently useless, although some drivers may
      -- check it for "MSWIN4.1" and other values. Microsoft recommends the
      -- previous Microsoft Windows 4.1 identifier.
      OEM_Identifier      : string(1 .. 8) := (OTHERS => character'val(0));
      -- The amount of bytes that make up a sector. This should usually be 512
      -- from what I know.
      Bytes_Per_Sector    : number RANGE 0 .. 2**16 - 1 := 512;
      -- How many logical sectors there are in one cluster.
      Sectors_Per_Cluster : number RANGE 0 .. 2**08 - 1 := 1;
      -- The amount of reserved sectors (including the one this is found in).
      -- The first FAT comes right after the reserved sectors at the beginning.
      Reserved_Sectors    : number RANGE 0 .. 2**16 - 1 := 2;
      -- The maximum amount of file allocation tables. Apparently this is
      -- usually 2.
      FAT_Count           : number RANGE 0 .. 2**08 - 1 := 2;
      -- The (maximum) amount of root directory entries. For FAT32, this will
      -- always be zero, as they're instead stored in the regular clusters.
      Directory_Entries   : number RANGE 0 .. 2**16 - 1 := 0;
      -- The total number of logical sectors on the drive. If the total number
      -- is too large to fit into this field, then it goes at the end of the
      -- BPB in the "Large_Total_Sectors" field and this will be set to zero.
      Total_Sectors       : number RANGE 0 .. 2**16 - 1 := 0;
      -- Contains a lot of antique descriptions for disk devices. For devices
      -- which are not described, this should be either 0xF0 for removable
      -- drives or 0xF8 for fixed drives.
      Media_Descriptor    : number RANGE 0 .. 2**08 - 1 := 0;
      -- The amount of logical sectors in a file allocation table. FAT32 sets
      -- this to zero and has another value for it in the FAT32 EBPB.
      Sectors_Per_FAT     : number RANGE 0 .. 2**16 - 1 := 0;
      -- This is unused if CHS is obsolete for a drive. That is true if a
      -- value of zero or one is stored here, indicating it's reserved.
      Sectors_Per_Track   : number RANGE 0 .. 2**16 - 1 := 0;
      -- This is unused if CHS is obsolete for a drive. That is true if a
      -- value of zero or one is stored here, indicating it's reserved.
      Head_Count          : number RANGE 0 .. 2**16 - 1 := 0;
      -- The number of logical sectors that are hidden from the FAT volume.
      -- If it's zero, then the drive has not been partitioned yet.
      Hidden_Sector_Count : number RANGE 0 .. 2**32 - 1 := 0;
      -- The total number of logical sectors on the drive if the previously
      -- smaller total sector count field could not hold the values. This
      -- should be used if the previous field is zero.
      Large_Total_Sectors : number RANGE 0 .. 2**32 - 1 := 0;
   END RECORD;
   FOR BIOS_parameter_block USE RECORD
      Jump_Code              AT 00 RANGE 0 .. 23;
      OEM_Identifier         AT 03 RANGE 0 .. 63;
      Bytes_Per_Sector       AT 11 RANGE 0 .. 15;
      Sectors_Per_Cluster    AT 13 RANGE 0 .. 07;
      Reserved_Sectors       AT 14 RANGE 0 .. 15;
      FAT_Count              AT 16 RANGE 0 .. 07;
      Directory_Entries      AT 17 RANGE 0 .. 15;
      Total_Sectors          AT 19 RANGE 0 .. 15;
      Media_Descriptor       AT 21 RANGE 0 .. 07;
      Sectors_Per_FAT        AT 22 RANGE 0 .. 15;
      Sectors_Per_Track      AT 24 RANGE 0 .. 15;
      Head_Count             AT 26 RANGE 0 .. 15;
      Hidden_Sector_Count    AT 28 RANGE 0 .. 31;
      Large_Total_Sectors    AT 32 RANGE 0 .. 31;
   END RECORD;

   -- This is the Extended BIOS Parameter Block (EBPB) and this one in
   -- particular is for FAT12 and FAT16, but not for FAT32.
   TYPE extended_BIOS_parameter_block_16 IS RECORD
      -- The physical drive number. If the value goes from 0x00 to 0x7E, then
      -- it is a removable device; otherwise, if the value goes from 0x80 to
      -- 0xFF, then it is a fixed device. We can ignore this for the most part.
      Drive_Number     : number RANGE 0 .. 2**08 - 1 := 0;
      -- A set of flags that is supposed to be reserved, but Windows NT uses it
      -- for various things, like CHKDSK status. We'll ignore this too.
      Windows_NT_Flags : number RANGE 0 .. 2**08 - 1 := 0;
      -- This is either 0x29 to indicate that there's three other fields after
      -- this field, or it's 0x28 to indicate that it's some older version of
      -- the EBPB. It should be 0x29 these days.
      Signature        : number RANGE 0 .. 2**08 - 1 := 0;
      -- The serial number for the partition. The way this is generated is
      -- often by combining the creation time and date.
      Serial_Number    : number RANGE 0 .. 2**32 - 1 := 0;
      -- The volume label. This is not available if the signature is set to
      -- 0x28, it is only here if it's 0x29. Padded with spaces.
      Label            : string(1 .. 11) := (OTHERS => character'val(0));
      -- A string representation of the FAT version the partition is supposed
      -- to be; however, this is apparently not reliable and it should not be
      -- used to determine the version. Padded with spaces.
      Version_Name     : string(1 .. 08) := (OTHERS => character'val(0));
   END RECORD;
   FOR extended_BIOS_parameter_block_16 USE RECORD
      Drive_Number        AT 00 RANGE 0 .. 07;
      Windows_NT_Flags    AT 01 RANGE 0 .. 07;
      Signature           AT 02 RANGE 0 .. 07;
      Serial_Number       AT 03 RANGE 0 .. 31;
      Label               AT 07 RANGE 0 .. 87;
      Version_Name        AT 18 RANGE 0 .. 63;
   END RECORD;

   -- Describes the boot record sector of a FAT file system.
   -- TODO: When adding FAT32 support, use "Unchecked_Union" to add "EBPB_32".
   TYPE boot_record IS RECORD
      BPB            : BIOS_parameter_block;
      EBPB_16        : extended_BIOS_parameter_block_16;
      Boot_Code_16   : bytes(1 .. 448) := (OTHERS => 0);
      Boot_Signature : number RANGE 0 .. 2**16 - 1 := 0;
   END RECORD;
   FOR boot_record USE RECORD
      BPB              AT 000 RANGE 0 .. 0287;
      EBPB_16          AT 036 RANGE 0 .. 0207;
      Boot_Code_16     AT 062 RANGE 0 .. 3583;
      Boot_Signature   AT 510 RANGE 0 .. 0015;
   END RECORD;

   -- A set of attributes an 8.3 file entry can have.
   TYPE standard_file_attributes IS
     (no_file_attribute,
      read_only_file_attribute,
      hidden_file_attribute,
      system_file_attribute,
      volume_identity_attribute,
      long_file_name_attribute,
      directory_attribute,
      archive_attribute)
   WITH
      Default_Value => no_file_attribute,
      Size          => 8;
   FOR standard_file_attributes USE
     (no_file_attribute         => 16#00#,
      read_only_file_attribute  => 16#01#,
      hidden_file_attribute     => 16#02#,
      system_file_attribute     => 16#04#,
      volume_identity_attribute => 16#08#,
      long_file_name_attribute  => 16#0F#,
      directory_attribute       => 16#10#,
      archive_attribute         => 16#20#);

   -- The creation or modification time record for standard file entries.
   TYPE standard_file_time IS RECORD
      -- The hour the file was created or modified.
      Hour          : number RANGE 0 .. 2**5 - 1 := 0;
      -- The minute the file was created or modified.
      Minute        : number RANGE 0 .. 2**6 - 1 := 0;
      -- The second the file was created or modified. Note that this value must
      -- be doubled in order to get the actual seconds value.
      Second_Halved : number RANGE 0 .. 2**5 - 1 := 0;
   END RECORD;
   FOR standard_file_time USE RECORD
      Hour              AT 0 RANGE 00 .. 04;
      Minute            AT 0 RANGE 05 .. 10;
      Second_Halved     AT 0 RANGE 11 .. 15;
   END RECORD;

   -- The creation or modification date record for standard file entries.
   TYPE standard_file_date IS RECORD
      -- The year the file was created or modified.
      Year  : number RANGE 0 .. 2**7 - 1 := 0;
      -- The month the file was created or modified.
      Month : number RANGE 0 .. 2**4 - 1 := 0;
      -- The day the file was created or modified.
      Day   : number RANGE 0 .. 2**5 - 1 := 0;
   END RECORD;
   FOR standard_file_date USE RECORD
      Year      AT 0 RANGE 00 .. 06;
      Month     AT 0 RANGE 07 .. 10;
      Day       AT 0 RANGE 11 .. 15;
   END RECORD;

   -- The standard 8.3 format for FAT/file entries. The name "8.3" comes from
   -- the fact that FAT (originally) only supported names of 8 character length
   -- with a 3 character-long extension name.
   TYPE standard_file_format IS RECORD
      -- The name of the file itself. It is limited to 8 characters if you do
      -- not use long file names, which is a VFAT extension. This is padded
      -- with spaces, not null characters. If the first byte is in a cluster,
      -- then there are no more entries. If it's 0xE5, then it's unused and you
      -- can skip the entry.
      File_Name         : string(1 .. 8) := (OTHERS => character'val(0));
      -- The extension given to the file. Does not include the full stop.
      Extension         : string(1 .. 3) := (OTHERS => character'val(0));
      -- A number of file attribute flags that describe the file entry itself.
      Attributes        : standard_file_attributes;
      -- A reserved byte for Windows NT. Not sure what it does.
      Reserved_1        : number RANGE 0 .. 2**08 - 1 := 0;
      -- This is apparently some sort of field used for storing the creation
      -- time in seconds; however, I cannot find much information on it. Some
      -- documents list it as reserved while OSDev Wiki says Windows NT stores
      -- a value from 0 to 199 (inclusive) in here for the former reason.
      -- I'll just ignore it for now.
      Reserved_2        : number RANGE 0 .. 2**08 - 1 := 0;
      -- The time at which the file entry was created.
      Creation_Time     : standard_file_time;
      -- The date at which the file entry was created.
      Creation_Date     : standard_file_date;
      -- The last date at which the file entry was accessed.
      Access_Date       : standard_file_date;
      -- The higher 16 bits of the first cluster number. For FAT12 and FAT16,
      -- this will obviously always be zero. It's for FAT32 only.
      Cluster_High      : number RANGE 0 .. 2**16 - 1 := 0;
      -- The last time at which the file entry was modified.
      Modification_Time : standard_file_time;
      -- The last date at which the file entry was modified.
      Modification_Date : standard_file_date;
      -- The entry's first cluster number. This's relevant to all FAT versions.
      -- If this is zero (along with the higher two bytes), then this links
      -- back to the root directory.
      Cluster_Low       : number RANGE 0 .. 2**16 - 1 := 0;
      -- The size of the file in bytes. FAT is unfortunately limited to 4 GiB
      -- files.
      File_Size         : number RANGE 0 .. 2**32 - 1 := 0;
   END RECORD;
   FOR standard_file_format USE RECORD
      File_Name            AT 00 RANGE 0 .. 63;
      Extension            AT 08 RANGE 0 .. 23;
      Attributes           AT 11 RANGE 0 .. 07;
      Reserved_1           AT 12 RANGE 0 .. 07;
      Reserved_2           AT 13 RANGE 0 .. 07;
      Creation_Time        AT 14 RANGE 0 .. 15;
      Creation_Date        AT 16 RANGE 0 .. 15;
      Access_Date          AT 18 RANGE 0 .. 15;
      Cluster_High         AT 20 RANGE 0 .. 15;
      Modification_Time    AT 22 RANGE 0 .. 15;
      Modification_Date    AT 24 RANGE 0 .. 15;
      Cluster_Low          AT 26 RANGE 0 .. 15;
      File_Size            AT 28 RANGE 0 .. 31;
   END RECORD;

   -- A short abstraction for an entry in a path.
   TYPE path_entry IS RECORD
      -- Not present by default.
      Present    : boolean         := false;
      -- Everything in a path is a dictionary unless specified otherwise.
      Dictionary : boolean         := true;
      -- An empty name is just padded with spaces everywhere.
      Name       : string(1 .. 12) := (OTHERS => ' ');
   END RECORD;

   -- A type that is an array which describes a path direction. Each element
   -- is an 8.3 entry's name (including the dot extension). A maximum length
   -- for this would be around 128 components, which is hardcoded as the length
   -- due to a secondary stack not being available.
   -- TODO: Support long file names.
   TYPE path IS ARRAY(number RANGE 1 .. 128) OF path_entry
   WITH
      Pack => true;

   -- This is for browsing the file entries in a sector. Simply divide the
   -- sector size with the size of the standard file format (32 bytes) to get
   -- the maximum limit of file entries in a sector. The first dimension is
   -- for the indicated sector.
   TYPE file_entries IS ARRAY(number RANGE <>, number RANGE <>)
      OF standard_file_format
   WITH
      Pack => true;
   TYPE access_file_entries IS ACCESS file_entries;

   -- The below range types indicate where the invalid cluster values belong.
   -- The least significant byte indicates why it's invalid. For all intents
   -- and purposes, you can (must) skip any clusters here.
   SUBTYPE invalid_cluster_12 IS number RANGE 16#00000FF0# .. 16#00000FFF#;
   SUBTYPE invalid_cluster_16 IS number RANGE 16#0000FFF0# .. 16#0000FFFF#;
   SUBTYPE invalid_cluster_32 IS number RANGE 16#0FFFFFF0# .. 16#0FFFFFFF#;

   -- A cluster that is reserved. There are other byte markers that indicate
   -- a cluster is reserved as well, but this tends to be the most compatible
   -- one.
   Reserved_Cluster : CONSTANT := 16#F6#;
   -- A cluster with a bad sector in it, leaving it unsuitable for data
   -- storage. This one is always 0xF7.
   Bad_Cluster      : CONSTANT := 16#F7#;
   -- Indicates the end of the linked list of clusters. This also comes in the
   -- form of 0xF0 and 0xFF, so be sure to stop at them as well.
   End_Cluster      : CONSTANT := 16#F8#;
   -- This is a custom marker I created, it is not official and is only used
   -- this kernel, it is never written to the drive. It's just for indicating
   -- that there was no file to match against and get the cluster value of.
   No_File_Match    : CONSTANT invalid_cluster_32 := invalid_cluster_32'last;

   -- Describes the state for a FAT file system. Used by other packages to
   -- read and write files. I've avoided making this into a class/tagged type
   -- for now, but I may change that in the future.
   -- TODO: When adding FAT32 support, use "Unchecked_Union" to add "EBPB_32"
   -- if this record is not tagged.
   TYPE file_system IS RECORD
      -- The FAT version. This is set to "unknown" if the state of the file
      -- system is not supported, unrecognised, or corrupted.
      FAT_Version                 : version;
      -- A copy of the partition information for where the FAT file system was
      -- found etc. Required to get the correct drive and calculate the correct
      -- relative LBAs as opposed to absolute LBAs.
      Drive_Partition             : GPT.partition;
      -- A copy of the BIOS Parameter Block, as it contains much important
      -- information to do with traversing the file system.
      BPB                         : BIOS_parameter_block;
      -- Another copy, but this time of the FAT12/FAT16 Extended BIOS Parameter
      -- Block, which is less useful, but it's small enough to include.
      EBPB_16                     : extended_BIOS_parameter_block_16;
      -- The total amount of data sectors in the file system.
      Data_Sectors                : number := 0;
      -- The total amount of root directory sectors in the file system.
      Root_Directory_Sectors      : number := 0;
      -- The first data sector. Pre-calculated so getting clusters is easier.
      First_Data_Sector           : logical_block_address := 0;
      -- The first root directory sector. Pre-calculated so getting clusters is
      -- easier for versions below FAT32.
      First_Root_Directory_Sector : logical_block_address := 0;
   END RECORD
   WITH
      Dynamic_Predicate =>
      (  -- See the OSDev Wiki article for the calculations below as usual.
         CASE
            FAT_Version
         IS -- These checks are for supported versions only.
            WHEN FAT16 => -- TODO: Only 512-byte sectors are supported.
               BPB.Bytes_Per_Sector = 512 AND THEN
               BPB.Sectors_Per_Cluster >= 1 AND THEN
               BPB.Reserved_Sectors >= 1 AND THEN
               BPB.FAT_Count >= 1 AND THEN
               Root_Directory_Sectors IN 1 .. ((BPB.Directory_Entries * 32) +
                 (BPB.Bytes_Per_Sector - 1)) / BPB.Bytes_Per_Sector AND THEN
               BPB.Total_Sectors >= ((BPB.Reserved_Sectors + (BPB.FAT_Count *
                  BPB.Sectors_Per_FAT)) + Root_Directory_Sectors) AND THEN
               Data_Sectors IN 1 .. BPB.Total_Sectors -
                 ((BPB.Reserved_Sectors + (BPB.FAT_Count *
                  BPB.Sectors_Per_FAT)) + Root_Directory_Sectors) AND THEN
               Root_Directory_Sectors + Data_Sectors <=
                  BPB.Total_Sectors AND THEN
               First_Root_Directory_Sector <= BPB.Reserved_Sectors +
                 (BPB.FAT_Count * BPB.Sectors_Per_FAT) AND THEN
               First_Data_Sector <= First_Root_Directory_Sector +
                  Root_Directory_Sectors AND THEN
               Drive_Partition.LBA_First + First_Root_Directory_Sector IN
                  Drive_Partition.LBA_First .. Drive_Partition.LBA_Last
                  AND THEN
               Drive_Partition.LBA_First + First_Data_Sector IN
                  Drive_Partition.LBA_First .. Drive_Partition.LBA_Last,
            WHEN OTHERS =>
               true
      );

   -- Does some light abstractions for reading sectors off the FAT partition.
   -- Essentially the same as using `PIO_Read()`.
   PRAGMA Warnings(GNATprove, off, """Object_Location"" is not modified, *",
      Reason => "The location is modified, but not through the variable.");
   GENERIC
      TYPE object(<>) IS PRIVATE;
      TYPE access_object IS ACCESS object;
   PROCEDURE FAT_Read
     (FAT_Context     : IN file_system;
      Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Object_Location : IN access_object)
   WITH
      Inline => true,
      Pre    => FAT_Context.FAT_Version = FAT16 AND THEN
                Sector_Count IN 1 .. 2**16 - 1  AND THEN
                FAT_Context.Drive_Partition.LBA_First + Sector_Base IN
                   FAT_Context.Drive_Partition.LBA_First ..
                      FAT_Context.Drive_Partition.LBA_Last;

   -- Does some light abstractions for writing sectors to the FAT partition.
   -- Essentially the same as using `PIO_Write()`.
   GENERIC
      TYPE object(<>) IS PRIVATE;
      TYPE access_object IS ACCESS object;
   PROCEDURE FAT_Write
     (FAT_Context     : IN file_system;
      Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Object_Location : IN access_object)
   WITH
      Inline => true,
      Pre    => FAT_Context.FAT_Version = FAT16 AND THEN
                Sector_Count IN 1 .. 2**16 - 1  AND THEN
                FAT_Context.Drive_Partition.LBA_First + Sector_Base IN
                   FAT_Context.Drive_Partition.LBA_First ..
                      FAT_Context.Drive_Partition.LBA_Last;

   -- As of now, this simply turns a single string indicating a path into an
   -- array of 128 12-character strings, with each element being the name of a
   -- file/directory entry. A completely blank (space padded) string terminates
   -- the path.
   FUNCTION Tokenize_Path
     (Path_Name      : IN string;
      Dictionary_End : IN boolean := false)
      RETURN path
   WITH
      Pre  => Path_Name'first = 1 AND THEN
              Path_Name'last IN Path_Name'first .. 255;

   -- If a file (or directory, if specified) matches the entry name passed,
   -- then this will return the first cluster number. If nothing matches, then
   -- it will return a FAT32 end-of-cluster-link value.
   -- TODO: This is limited to 8.3 file names.
   FUNCTION Match_Entry
     (Entries    : NOT NULL ACCESS CONSTANT file_entries;
      Entry_Name : IN string;
      Directory  : IN boolean := false)
      RETURN standard_file_format
   WITH
      Pre => Entry_Name'first = 1 AND THEN
             Entry_Name'last IN Entry_Name'first .. 12;

   -- Searches for an entry inside a dictionary. If the sixth parameter is
   -- true, then it just searches for other dictionaries as opposed to files
   -- only. The "Next_Cluster" output is just the file entry's cluster start
   -- that has the high and low words combined/OR'd.
   PROCEDURE Search_For_Entry
     (FAT_Context   : IN file_system;
      Entry_Name    : IN string;
      First_Cluster : IN number;
      Next_Cluster  : OUT number;
      File_Entry    : OUT standard_file_format;
      Dictionary    : IN boolean := false)
   WITH
      Pre  => FAT_Context.FAT_Version = FAT16 AND THEN
              First_Cluster <= 2**32 - 1      AND THEN
              Entry_Name'first = 1            AND THEN
              Entry_Name'last IN Entry_Name'first .. 12,
      Post => Next_Cluster <= 2**32 - 1;

   -- Goes through a file entry's linked (cluster) list.
   -- TODO: For now, this only supports FAT16, not FAT12 or FAT32 etc. as the
   -- calculations done are only for 16-bit entries in the FAT itself.
   PROCEDURE Get_Next_Cluster
     (FAT_Context   : IN file_system;
      First_Cluster : IN number;
      Next_Cluster  : OUT number)
   WITH
      Pre  => FAT_Context.FAT_Version = FAT16 AND THEN
              First_Cluster <= 2**32 - 1,
      Post =>  Next_Cluster <= 2**32 - 1;

   -- This is the private version of the `Check_File()` procedure. It also
   -- returns the unrevised file entry itself.
   PROCEDURE Check_File
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Error_Status : OUT error;
      File_Entry   : OUT standard_file_format)
   WITH
      Pre  => Get_FAT_Version(FAT_Context) = FAT16 AND THEN
              Path_Name'first = 1                  AND THEN
              Path_Name'last IN Path_Name'first .. 255,
      Post => Error_Status IN no_error | path_error;

   -- Puts the actual file into memory, as opposed to just reading the entry.
   -- The destination will contain the file's first byte. There is also no
   -- padding, as this is done in the simplest manner possible via a
   -- byte-by-byte transfer and it contains no (algorithmic) optimisations.
   -- The base byte and byte size parameters control which parts of the file
   -- to read into the memory destination. A byte size of zero means the range
   -- will go from the base byte to the end of the file. By default, this
   -- copies the entire file to the memory destination.
   PROCEDURE File_To_Memory
     (FAT_Context  : IN file_system;
      File_Entry   : IN standard_file_format;
      Destination  : IN address;
      Error_Status : OUT error;
      Base_Byte    : IN number := 1;
      Byte_Size    : IN number := 0)
   WITH
      Pre  => FAT_Context.FAT_Version = FAT16                     AND THEN
              Destination /= 0                                    AND THEN
              File_Entry.File_Size /= 0                           AND THEN
              Base_Byte IN 1 .. File_Entry.File_Size              AND THEN
              Byte_Size <= (File_Entry.File_Size - Base_Byte) + 1 AND THEN
             (Shift_Left(File_Entry.Cluster_High, 16) OR
                File_Entry.Cluster_Low) < invalid_cluster_16'first,
      Post => Error_Status IN no_error; -- TODO: Add more error checking.

END HAVK_Kernel.Drive.FAT;
