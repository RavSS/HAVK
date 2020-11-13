-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA PIO Driver                   --
-- Filename        -- havk_ata_pio-fat.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation,
   System.Case_Util;

PACKAGE BODY HAVK_ATA_PIO.FAT
IS
   FUNCTION Get_FAT_Version
     (FAT_Context : IN file_system)
      RETURN version
   IS
     (FAT_Context.FAT_Version);

   PROCEDURE Get_File_System
     (New_FAT_Context : OUT file_system;
      FAT_Partition   : IN GPT.partition)
   IS
      -- Temporary type to save stack space.
      TYPE access_boot_record IS ACCESS boot_record;

      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object => boot_record, name => access_boot_record);

      FUNCTION To_Address
        (Boot_Record_Access : IN access_boot_record)
         RETURN address
      WITH
         Import => true,
         Convention => Intrinsic,
         Pre        => Boot_Record_Access /= NULL,
         Post       => To_Address'result /= 0;

      FAT_Boot_Record             : access_boot_record := NEW boot_record;
      FAT_Version                 : version;
      Root_Directory_Sectors      : number;
      Data_Sectors                : number;
      First_Root_Directory_Sector : logical_block_address;
      First_Data_Sector           : logical_block_address;
   BEGIN
      PIO_Read(FAT_Partition.LBA_First, 1, To_Address(FAT_Boot_Record),
         FAT_Partition.Secondary_Bus, FAT_Partition.Secondary_Drive);
      New_FAT_Context := (FAT_Version => unknown, OTHERS => <>);

      IF -- The first byte will always be 0xEB. Second one may be different.
        (FAT_Boot_Record.BPB.Jump_Code AND 16#FF#) /= 16#EB#
      THEN
         Log("FAT boot record jump code is incorrect (0x" &
            Image(FAT_Boot_Record.BPB.Jump_Code, Base => 16, Padding => 6) &
            "). Will not proceed.", Tag => FAT_Tag, Warn => true);

         Free(FAT_Boot_Record);
         RETURN;
      ELSIF
         FAT_Boot_Record.BPB.Bytes_Per_Sector /= 512
      THEN
         Log("The FAT file system does not use a sector size of 512.",
           Tag => FAT_Tag, Warn => true);
         Log("Cannot continue as of this time.", Tag => FAT_Tag, Warn => true);

         Free(FAT_Boot_Record);
         RETURN;
      END IF;

      -- Times the amount of (root) directory entries with the size of the
      -- 8.3 file format.
      Root_Directory_Sectors := ((FAT_Boot_Record.BPB.Directory_Entries * 32) +
        (FAT_Boot_Record.BPB.Bytes_Per_Sector - 1)) /
         FAT_Boot_Record.BPB.Bytes_Per_Sector;

      IF
         Root_Directory_Sectors = 0
      THEN
         Log("File system on EFI drive is not FAT12 or FAT16 (likely FAT32)," &
            " cannot proceed as of now.", Tag => FAT_Tag, Warn => true);

         Free(FAT_Boot_Record);
         RETURN;
      END IF;

      IF -- This is not to be depended upon as an error-check method.
         FAT_Boot_Record.EBPB_16.Version_Name IN "FAT12   " | "FAT16   "
      THEN
         Log("FAT file system found.", Tag => FAT_Tag);
      ELSE
         Log("FAT file system found, but it could be corrupt.", Tag => FAT_Tag,
            Warn => true);
      END IF;

      Data_Sectors := FAT_Boot_Record.BPB.Total_Sectors -
        ((FAT_Boot_Record.BPB.Reserved_Sectors +
        (FAT_Boot_Record.BPB.FAT_Count *
         FAT_Boot_Record.BPB.Sectors_Per_FAT)) + Root_Directory_Sectors);

      -- This is the only determinant way to see which FAT version the file
      -- system is.
      CASE -- I took the cluster ranges from Wikipedia and also OSDev Wiki.
         Data_Sectors / FAT_Boot_Record.BPB.Sectors_Per_Cluster
      IS
         WHEN 000000000000 .. 00000004084 => FAT_Version := FAT12;
         WHEN 000000004085 .. 00000065524 => FAT_Version := FAT16;
         WHEN 000000065525 .. 00268435444 => FAT_Version := FAT32;
         WHEN 000268435445 .. number'last => FAT_Version := exFAT;
      END CASE;

      Log("File system verified to be " & version'image(FAT_Version) & '.',
         Tag => FAT_Tag);

      IF
         FAT_Version /= FAT16
      THEN
         Log("Can only parse FAT16. Cannot proceed.", Tag => FAT_Tag,
            Warn => true);

         Free(FAT_Boot_Record);
         RETURN;
      END IF;

      -- Jump over the FAT(s) and the reserved sectors to get to the root
      -- directory of the FAT file system.
      First_Root_Directory_Sector := FAT_Boot_Record.BPB.Reserved_Sectors +
        (FAT_Boot_Record.BPB.FAT_Count * FAT_Boot_Record.BPB.Sectors_Per_FAT);

      -- Now skip the entire root directory region to get to the data sectors.
      First_Data_Sector := First_Root_Directory_Sector +
         Root_Directory_Sectors;

      New_FAT_Context :=
        (FAT_Version                 => FAT_Version,
         Drive_Partition             => FAT_Partition,
         BPB                         => FAT_Boot_Record.BPB,
         EBPB_16                     => FAT_Boot_Record.EBPB_16,
         Data_Sectors                => Data_Sectors,
         Root_Directory_Sectors      => Root_Directory_Sectors,
         First_Data_Sector           => First_Data_Sector,
         First_Root_Directory_Sector => First_Root_Directory_Sector);

      Free(FAT_Boot_Record);
   END Get_File_System;

   PROCEDURE FAT_Read
     (FAT_Context     : IN file_system;
      Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Object_Location : IN access_object)
   IS
      FUNCTION To_Address
        (Object_Access : IN access_object)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;
   BEGIN
      PIO_Read
        (FAT_Context.Drive_Partition.LBA_First + Sector_Base,
         Sector_Count,
         To_Address(Object_Location),
         Secondary_Bus   => FAT_Context.Drive_Partition.Secondary_Bus,
         Secondary_Drive => FAT_Context.Drive_Partition.Secondary_Drive);
   END FAT_Read;

   PROCEDURE FAT_Write
     (FAT_Context     : IN file_system;
      Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Object_Location : IN access_object)
   IS
      FUNCTION To_Address
        (Object_Access : IN access_object)
         RETURN address
      WITH
         Import     => true,
         Convention => Intrinsic;
   BEGIN
      PIO_Write
        (FAT_Context.Drive_Partition.LBA_First + Sector_Base,
         Sector_Count,
         To_Address(Object_Location),
         Secondary_Bus   => FAT_Context.Drive_Partition.Secondary_Bus,
         Secondary_Drive => FAT_Context.Drive_Partition.Secondary_Drive);
   END FAT_Write;

   PROCEDURE Get_Next_Cluster
     (FAT_Context   : IN file_system;
      First_Cluster : IN number;
      Next_Cluster  : OUT number)
   IS
      -- Save stack space as usual.
      TYPE access_words IS ACCESS words;

      -- The namesake of the file system comes from its File Allocation Table,
      -- which is a region in the file system that acts as a cluster map. Each
      -- cluster has the same size of several kibibytes, and each FAT entry
      -- depends on the entry size within the table i.e. for FAT12 and FAT16,
      -- entries will occupy 12 bits and 16 bits respectively, while for FAT32,
      -- they're 32 bits.
      PROCEDURE Get_File_Allocation_Table IS NEW FAT_Read
        (object        =>        words,
         access_object => access_words);
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object        =>        words,
         name          => access_words);

      -- Instead of reading the entire file allocation table into our memory
      -- and then getting the next cluster value, we calculate a sector of the
      -- FAT where our next cluster value is and only load that to save time.
      Table_Sector  : CONSTANT logical_block_address :=
         FAT_Context.BPB.Reserved_Sectors + ((First_Cluster * 2) /
         FAT_Context.BPB.Bytes_Per_Sector);

      -- This is for FAT16, so the sector size is divided by 2 to indicate
      -- e.g. for a 512-byte sector, there will be 256 16-bit FAT entries
      -- in this particular part (sector) of the file allocation table.
      Table_Entries : access_words := NEW words
        (1 .. FAT_Context.BPB.Bytes_Per_Sector / 2);

      -- Index calculation for the above array. Presumes one-based array.
      Table_Index   : CONSTANT number := (((First_Cluster * 2) MOD
         FAT_Context.BPB.Bytes_Per_Sector) / 2) + 1;
   BEGIN
      IF -- Check if the table sector is in the FAT partition itself.
         FAT_Context.Drive_Partition.LBA_First + Table_Sector NOT IN
            FAT_Context.Drive_Partition.LBA_First ..
               FAT_Context.Drive_Partition.LBA_Last
      THEN
         Next_Cluster := invalid_cluster_16'last;
      ELSIF -- Don't read an invalid cluster or else we'll cause a drive error.
         First_Cluster < invalid_cluster_16'first
      THEN
         Get_File_Allocation_Table(FAT_Context, Table_Sector, 1,
            Table_Entries);

         IF -- Check must be placed here, as the access type can't be constant.
            Table_Index IN Table_Entries'range
         THEN
            Next_Cluster := number(Table_Entries(Table_Index));
         ELSE
            Next_Cluster := invalid_cluster_16'last;
         END IF;
      ELSE
         Next_Cluster := invalid_cluster_16'last;
      END IF;

      Free(Table_Entries);
   END Get_Next_Cluster;

   FUNCTION Tokenize_Path
     (Path_Name      : IN string;
      Dictionary_End : IN boolean := false)
      RETURN path
   IS
      Entries : number := 0; -- Counts the amount of entries in the path.
      Index   : positive; -- Stores the current character index for a name.
   BEGIN
      FOR
         Letter OF Path_Name
      LOOP
         EXIT WHEN Letter = NUL;

         IF
            Letter = Separator AND THEN
            Entries /= number'last -- For `gnatprove` (overflow check).
         THEN
            Entries := Entries + 1;
         END IF;
      END LOOP;

      IF -- TODO: Not sure if the below range check is the correct length.
         Path_Name'length = 0                    OR ELSE
         Path_Name(Path_Name'first) /= Separator OR ELSE
         Entries NOT IN 1 .. 128
      THEN
         RETURN (OTHERS => <>);
      END IF;

      RETURN -- Remember that 8.3 path names are padded with spaces.
         Parsed_Path : path
      DO
         Entries := 1; -- Reused as an entry index variable.
         Index   := Parsed_Path(Parsed_Path'first).Name'first;

         FOR -- Skip the first separator, as we now know it's there.
            Letter OF Path_Name(Path_Name'first + 1 .. Path_Name'last)
         LOOP
            PRAGMA Loop_Invariant
              (Index IN Parsed_Path(Parsed_Path'first).Name'range AND THEN
               Entries IN 1 .. 128);
            EXIT WHEN Letter = NUL;

            IF
               Letter /= Separator AND THEN
               Entries IN Parsed_Path'range -- Just for `gnatprove`.
            THEN -- Add the character to the entry's name.
               Parsed_Path(Entries).Name(Index) := Letter;
               Index := (IF Index + 1 IN Parsed_Path(Entries).Name'range
                  THEN Index + 1 ELSE Parsed_Path(Entries).Name'last);
            ELSE -- Move onto the next entry if the separator was encountered.
               Entries := (IF Entries /= number'last AND THEN
                  Entries + 1 IN Parsed_Path'range THEN
                  Entries + 1 ELSE Parsed_Path'last);
               Parsed_Path(Entries).Present := true;
               Index := Parsed_Path(Entries).Name'first;
            END IF;
         END LOOP;

         Parsed_Path(Entries).Dictionary := Dictionary_End;
      END RETURN;
   END Tokenize_Path;

   FUNCTION Match_Entry
     (Entries    : NOT NULL ACCESS CONSTANT file_entries;
      Entry_Name : IN string;
      Directory  : IN boolean := false)
      RETURN standard_file_format
   IS
      -- Returns the index of the character before the padding of spaces shows
      -- up. That does not include any spaces which have characters after it.
      FUNCTION Last_Space
        (Name : IN string)
         RETURN positive
      WITH
         Pre  => Name'first = 1 AND THEN
                 Name'last IN Name'first .. 12,
         Post => Last_Space'result IN Name'first .. Name'last;

      FUNCTION Last_Space
        (Name : IN string)
         RETURN positive
      IS
      BEGIN
         FOR
            Index IN Name'range
         LOOP
            IF
               Name(Index) = ' '
            THEN
               IF
                 (FOR ALL Check IN Index .. Name'last => Name(Check) = ' ')
               THEN
                  RETURN (IF Index /= Name'first THEN Index - 1 ELSE Index);
               END IF;
            END IF;
         END LOOP;

         RETURN Name'last;
      END Last_Space;

      Uppercase_Entry_Name : -- The 8.3 format stores names in all uppercase.
         string(Entry_Name'first .. Last_Space(Entry_Name));
   BEGIN
      FOR
         Character_Index IN Uppercase_Entry_Name'range
      LOOP
         Uppercase_Entry_Name(Character_Index) :=
            Case_Util.To_Upper(Entry_Name(Character_Index));
      END LOOP;

      FOR
         File OF Entries.ALL
      LOOP
         -- Entries with a null first byte mark the end of a directory.
         EXIT WHEN File.File_Name(File.File_Name'first) = character'val(0);

         PRAGMA Warnings(GNATprove, off,
            "attribute Valid is assumed to return True",
            Reason => "The entry is ignored if it's false.");
         IF -- Check for unused entries and if it's a dictionary or not..
            File.File_Name(File.File_Name'first) /= character'val(16#E5#)
               AND THEN
            File.Attributes'valid AND THEN
           (IF Directory THEN File.Attributes = directory_attribute ELSE
               File.Attributes /= directory_attribute)
         THEN
            IF
               File.Extension = (File.Extension'range => ' ')
            THEN -- Ignore the extension and compare it directly.
               IF
                  File.File_Name
                    (File.File_Name'first .. Last_Space(File.File_Name)) =
                     Uppercase_Entry_Name
               THEN
                  RETURN File;
               END IF;
            ELSE -- Merge the file name and extension, then compare it.
               IF
                  File.File_Name
                    (File.File_Name'first .. Last_Space(File.File_Name)) &
                     '.' & File.Extension
                       (File.Extension'first .. Last_Space(File.Extension)) =
                     Uppercase_Entry_Name
               THEN
                  RETURN File;
               END IF;
            END IF;
         END IF;
      END LOOP;

      RETURN
        (Cluster_Low  => No_File_Match AND 16#FFFF#,
         Cluster_High => Shift_Right(No_File_Match, 16),
         OTHERS       => <>);
   END Match_Entry;

   PROCEDURE Search_For_Entry
     (FAT_Context   : IN file_system;
      Entry_Name    : IN string;
      First_Cluster : IN number;
      Next_Cluster  : OUT number;
      File_Entry    : OUT standard_file_format;
      Dictionary    : IN boolean := false)
   IS
      PROCEDURE Get_File_Entries IS NEW FAT_Read
        (object        =>        file_entries,
         access_object => access_file_entries);
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object        =>        file_entries,
         name          => access_file_entries);

      -- The second dimension end range's denominator is
      -- (standard_file_format'size / 8), which `gnatprove` needs help with.
      Cluster : access_file_entries := NEW file_entries
        (1 .. FAT_Context.BPB.Sectors_Per_Cluster,
         1 .. FAT_Context.BPB.Bytes_Per_Sector / 32);

      Temporary_Cluster : number := First_Cluster;
      Temporary_Entry   : standard_file_format;
   BEGIN
      WHILE
         Temporary_Cluster < invalid_cluster_16'first
      LOOP
         PRAGMA Loop_Invariant(Cluster /= NULL);

         EXIT WHEN Temporary_Cluster < 2; -- Overflow check for the below exit.
         EXIT WHEN FAT_Context.Drive_Partition.LBA_First +
           (FAT_Context.First_Data_Sector + ((Temporary_Cluster - 2) *
            FAT_Context.BPB.Sectors_Per_Cluster)) NOT IN
               FAT_Context.Drive_Partition.LBA_First ..
                  FAT_Context.Drive_Partition.LBA_Last;

         Get_File_Entries(FAT_Context,
            FAT_Context.First_Data_Sector + ((Temporary_Cluster - 2) *
               FAT_Context.BPB.Sectors_Per_Cluster),
            FAT_Context.BPB.Sectors_Per_Cluster, Cluster);

         Temporary_Entry := Match_Entry(Cluster, Entry_Name, Dictionary);
         Temporary_Cluster := Shift_Left(Temporary_Entry.Cluster_High, 16) OR
            Temporary_Entry.Cluster_Low;

         IF
            Temporary_Cluster = No_File_Match
         THEN -- Try again. If it's the end, then it'll return the error value.
            Get_Next_Cluster(FAT_Context, Temporary_Cluster,
               Temporary_Cluster);
         ELSE -- Found the requested entry's first cluster.
            Next_Cluster := Temporary_Cluster;
            File_Entry   := Temporary_Entry;
            Free(Cluster);
            RETURN;
         END IF;
      END LOOP;

      Next_Cluster := No_File_Match;
      File_Entry   := (OTHERS => <>);
      Free(Cluster);
   END Search_For_Entry;

   PROCEDURE File_To_Memory -- TODO: Heavily unoptimised for max simplicity.
     (FAT_Context  : IN file_system;
      File_Entry   : IN standard_file_format;
      Destination  : IN address;
      Error_Status : OUT error;
      Base_Byte    : IN number := 1;
      Byte_Size    : IN number := 0)
   IS
      -- I'll be writing raw bytes to memory and will need a buffer.
      TYPE access_bytes IS ACCESS bytes;

      PROCEDURE Get_File_Data IS NEW FAT_Read
        (object        =>        bytes,
         access_object => access_bytes);
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object        =>        bytes,
         name          => access_bytes);

      -- TODO: In the future, it would be better to try avoiding this buffer
      -- and telling the drive to directly write to the memory up above. That
      -- won't remove all chances of having to use a buffer, but if the file
      -- is big enough, then it can be avoided until the very last few bytes
      -- occupying a portion of a cluster. I don't think the performance gains
      -- would be noticeable enough to warrant it and I don't care about speed
      -- when considering we're using PIO and not DMA, so I've left it out.
      -- I'm not sure how guaranteed the cluster's empty bytes are to be null
      -- values.
      Cluster       : access_bytes := NEW bytes
        (1 .. FAT_Context.BPB.Sectors_Per_Cluster *
              FAT_Context.BPB.Bytes_Per_Sector);

      -- The area to move the data to. A byte size of zero is ignored and the
      -- maximum length is presumed (file size).
      Memory_Area   : bytes(Base_Byte .. Base_Byte +
        (IF Byte_Size /= 0 THEN Byte_Size - 1 ELSE File_Entry.File_Size))
      WITH
         Import   => true,
         Address  => Destination,
         Annotate => (GNATprove, False_Positive,
                      "object with constraints on bit representation *",
                      "This is safe regardless of what data gets written.");

      -- The current byte we're on. If the byte index is past the memory area's
      -- last index, then we'll return early to save a little time at least.
      Byte_Index    : number RANGE 1 .. Memory_Area'last := 1;

      -- The current cluster we're on.
      Data_Cluster  : number := File_Entry.Cluster_Low;
   BEGIN
      -- TODO: When the PIO operation procedures start returning error
      -- enumerations, then this must also do so.
      Error_Status := no_error;

      Transfer_Bytes : WHILE
         Data_Cluster < invalid_cluster_16'first AND THEN
         Byte_Index /= File_Entry.File_Size
      LOOP
         PRAGMA Loop_Invariant(Cluster /= NULL);

         EXIT WHEN Data_Cluster < 2; -- Overflow check for the below exit.
         EXIT WHEN FAT_Context.Drive_Partition.LBA_First +
           (FAT_Context.First_Data_Sector + ((Data_Cluster - 2) *
            FAT_Context.BPB.Sectors_Per_Cluster)) NOT IN
               FAT_Context.Drive_Partition.LBA_First ..
                  FAT_Context.Drive_Partition.LBA_Last;

         Get_File_Data(FAT_Context,
            FAT_Context.First_Data_Sector + ((Data_Cluster - 2) *
               FAT_Context.BPB.Sectors_Per_Cluster),
            FAT_Context.BPB.Sectors_Per_Cluster, Cluster);

         FOR
            Data_Byte OF Cluster.ALL
         LOOP
            IF
               Byte_Index IN Memory_Area'range
            THEN
               Memory_Area(Byte_Index) := Data_Byte;
               PRAGMA Assert(Memory_Area(Byte_Index) = Data_Byte);
            END IF;

            EXIT Transfer_Bytes WHEN Byte_Index = Memory_Area'last;
            Byte_Index := Byte_Index + 1;
         END LOOP;

         Get_Next_Cluster(FAT_Context, Data_Cluster, Data_Cluster);
      END LOOP Transfer_Bytes;

      Free(Cluster);
   END File_To_Memory;

   PROCEDURE Check_File
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Error_Status : OUT error;
      File_Entry   : OUT standard_file_format)
   IS
      PROCEDURE Get_File_Entries IS NEW FAT_Read
        (object        =>        file_entries,
         access_object => access_file_entries);
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object        =>        file_entries,
         name          => access_file_entries);

      Parsed_Path    : CONSTANT path := Tokenize_Path(Path_Name);

      -- The second dimension end range's denominator is
      -- (standard_file_format'size / 8), which `gnatprove` needs help with.
      Root_Directory : access_file_entries := NEW file_entries
        (1 .. FAT_Context.Root_Directory_Sectors,
         1 .. FAT_Context.BPB.Bytes_Per_Sector / 32);

      Old_Cluster, Next_Cluster : number RANGE 0 .. 2**32 - 1;
   BEGIN
      Get_File_Entries(FAT_Context, FAT_Context.First_Root_Directory_Sector,
         FAT_Context.Root_Directory_Sectors, Root_Directory);

      File_Entry := Match_Entry(Root_Directory,
         Parsed_Path(Parsed_Path'first).Name,
         Directory => Parsed_Path(Parsed_Path'first).Dictionary);

      Next_Cluster := Shift_Left(File_Entry.Cluster_High, 16) OR
         File_Entry.Cluster_Low;

      Free(Root_Directory);

      IF
         Next_Cluster = No_File_Match
      THEN
         File_Entry   := (OTHERS => <>);
         Error_Status := path_error;
         RETURN;
      END IF;

      FOR -- Skip over all the subdictionaries in the middle.
         Path_Index IN Parsed_Path'first + 1 .. Parsed_Path'last
      LOOP
         EXIT WHEN NOT Parsed_Path(Path_Index).Present;

         Old_Cluster := Next_Cluster;

         -- Directories can be bigger than just a 512-byte cluster, which would
         -- limit a directory to 16 file entries; thus, we need to check every
         -- cluster of the directory for obvious reasons.
         Directory_Search : LOOP
            Search_For_Entry(FAT_Context, Parsed_Path(Path_Index).Name,
               Next_Cluster, Next_Cluster, File_Entry,
               Dictionary => Parsed_Path(Path_Index).Dictionary);

            EXIT Directory_Search WHEN Next_Cluster /= No_File_Match;

            Get_Next_Cluster(FAT_Context, Old_Cluster, Next_Cluster);

            IF -- Incorrect path if true.
               Next_Cluster >= invalid_cluster_16'first
            THEN
               Error_Status := path_error;
               RETURN;
            END IF;
         END LOOP Directory_Search;
      END LOOP;

      Error_Status := no_error;
   END Check_File;

   PROCEDURE Check_File -- Does not expose the original FAT file entry.
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Error_Status : OUT error;
      Found_File   : OUT file)
   IS
      DOS_File_Entry : standard_file_format;
   BEGIN
      Check_File(FAT_Context, Path_Name, Error_Status, DOS_File_Entry);
      Found_File.Name :=
         DOS_File_Entry.File_Name & '.' & DOS_File_Entry.Extension;
      Found_File.Size := DOS_File_Entry.File_Size;
   END Check_File;

   PROCEDURE Read_File
     (FAT_Context  : IN file_system;
      Path_Name    : IN string;
      Destination  : IN address;
      Error_Status : OUT error;
      Base_Byte    : IN number := 1;
      Byte_Size    : IN number := 0)
   IS
      File_Entry   : standard_file_format;
   BEGIN
      Check_File(FAT_Context, Path_Name, Error_Status, File_Entry);

      IF
         Error_Status = path_error
      THEN
         RETURN;
      ELSIF
        (Shift_Left(File_Entry.Cluster_High, 16) OR File_Entry.Cluster_Low) <
            invalid_cluster_16'first AND THEN
         File_Entry.File_Size /= 0 AND THEN
         Base_Byte <= File_Entry.File_Size AND THEN
         Byte_Size <= (File_Entry.File_Size - Base_Byte) + 1
      THEN
         File_To_Memory(FAT_Context, File_Entry, Destination, Error_Status,
            Base_Byte, Byte_Size);
      ELSE
         Error_Status := size_error;
      END IF;
   END Read_File;

END HAVK_ATA_PIO.FAT;
