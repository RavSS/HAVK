-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-elf.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory,
   HAVK_Kernel.Paging;

PACKAGE BODY HAVK_Kernel.Tasking.ELF
IS
   PRAGMA Warnings(GNATprove, off, "attribute Valid is assumed to return True",
      Reason => "It's just a part of the greater manual validity checks.");

   FUNCTION Valid_File_Header
     (Header   : NOT NULL ACCESS CONSTANT file_header;
      ELF_File : IN Drive.FAT.file)
      RETURN boolean
   IS -- TODO: I am not sure if these are all the necessary checks.
   (
      -- Verify the magic numbers.
      Header.File_Identity.Magic_Byte = 16#7F#              AND THEN
      Header.File_Identity.Magic_Name = "ELF"               AND THEN
      -- Check for the correct bit size and endianness.
      Header.File_Identity.Class'valid                      AND THEN
      Header.File_Identity.Class = long_mode                AND THEN
      Header.File_Identity.Endianness'valid                 AND THEN
      Header.File_Identity.Endianness = little_endian       AND THEN
      -- Make sure the version numbers are expected. Ignore the ABI version.
      Header.File_Identity.Version = 1                      AND THEN
      Header.File_Identity.ABI = 0                          AND THEN
      -- Now verify if the ELF object itself is an executable ELF.
      Header.File_Type'valid                                AND THEN
      Header.File_Type = executable_object_file             AND THEN
      -- Check if it's for x86-64/AMD64/IA-32e and check the version.
      Header.Instruction_Set = 16#3E#                       AND THEN
      Header.Version = 1                                    AND THEN
      -- The entry address must match the one the tasking code expects.
      Header.Entry_Address = Virtual_Entry                  AND THEN
      -- Make sure the offsets for the subheaders are sane.
      Header.Program_Header_Offset < address(ELF_File.Size) AND THEN
      Header.Section_Header_Offset < address(ELF_File.Size) AND THEN
      -- The header size must be 64 for ELF64 files.
      Header.Header_Size = 64                               AND THEN
      -- Now further check the subheader sizes and lengths so they don't go
      -- over the actual file size itself.
      Header.Program_Header_Offset +
         address(Header.Program_Header_Entry_Count *
         Header.Program_Header_Entry_Size) <=
         address(ELF_File.Size)                             AND THEN
      Header.Section_Header_Offset +
         address(Header.Section_Header_Entry_Count *
         Header.Section_Header_Entry_Size) <=
         address(ELF_File.Size)                             AND THEN
      -- The section index for section string names index must be under the
      -- one-based count of all section headers.
      Header.Section_Header_Names_Index < Header.Section_Header_Entry_Count
   );

   FUNCTION Valid_Program_Header_Entry
     (Header : NOT NULL ACCESS CONSTANT program_header_entry)
      RETURN boolean
   IS
   (
      -- Make sure the segment type enumeration is indeed valid.
      Header.Segment_Type'valid                                       AND THEN
      -- Check the memory access/permission/protection flags. I've disallowed a
      -- few of them on purpose, as we must only respect W^X or read-only
      -- permissions.
      Header.Permission_Flag'valid                                    AND THEN
      Header.Permission_Flag IN
         readable_segment .. readable_and_writeable_segment           AND THEN
      -- The virtual address and physical address seem to match up for the
      -- small code model, so I'll enforce the check.
      Header.Virtual_Address = Header.Physical_Address                AND THEN
      -- Now check if the alignment is the proper value. It must be a power of
      -- two and the virtual address must be aligned.
     (IF Header.Alignment >= 2 THEN -- A value below two means no alignment.
        (Header.Alignment AND (Header.Alignment - 1)) = 0 AND THEN
         Header.Virtual_Address MOD address(Header.Alignment) = 0)
   );

   PROCEDURE Load
     (FAT_Context  : IN Drive.FAT.file_system;
      File_Path    : IN string;
      Task_Name    : IN string;
      Error_Status : OUT error)
   IS
      Error_Check        : error;
      ELF_File           : Drive.FAT.file;
      ELF_Header         : access_file_header;
      ELF_Segment        : access_program_header_entry;
      ELF_Segment_Offset : address;
      -- Usually the virtual base is specified by the first "LOAD" segment
      -- section, but I'll check it just in case.
      ELF_Virtual_Base   : address := address'last;
      ELF_Memory_Size    : number := 0;
      Task_Index         : Tasking.task_limit;
   BEGIN
      Log("Attempting to load """ & File_Path & """.", Tag => ELF_Tag);
      Drive.FAT.Check_File(FAT_Context, File_Path, Error_Check, ELF_File);

      IF
         Error_Check /= no_error
      THEN
         Log("ELF file not found.", Tag => ELF_Tag, Warn => true);
         Error_Status := Error_Check;
         RETURN;
      END IF;

      ELF_Header := NEW file_header;

      -- The byte size below is just the byte size of the file header, as
      -- `gnatprove` needs help with it.
      Drive.FAT.Read_File(FAT_Context, File_Path, To_Address(ELF_Header),
         Error_Check, Byte_Size => 64);

      IF
         Error_Check /= no_error
      THEN
         Log("Could not load ELF file.", Tag => ELF_Tag, Warn => true);
         Error_Status := Error_Check;
         Free(ELF_Header);
         RETURN;
      ELSIF
         NOT Valid_File_Header(ELF_Header, ELF_File)
      THEN
         Log("The ELF file has a corrupt file header.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := format_error;
         Free(ELF_Header);
         RETURN;
      END IF;

      ELF_Segment := NEW program_header_entry;

      FOR
         Index IN 0 .. ELF_Header.Program_Header_Entry_Count - 1
      LOOP
         -- The `Read_File()` function's base offset begins from one, not zero.
         ELF_Segment_Offset := (ELF_Header.Program_Header_Offset + 1) +
            address(ELF_Header.Program_Header_Entry_Size * Index);

         Drive.FAT.Read_File(FAT_Context, File_Path, To_Address(ELF_Segment),
            Error_Check,
            Base_Byte => number(ELF_Segment_Offset),
            Byte_Size => program_header_entry'size / 8);

         IF
            Error_Check /= no_error
         THEN
            Log("Could not read the ELF file.", Tag => ELF_Tag,
               Warn => true);
            Error_Status := Error_Check;
            Free(ELF_Segment);
            Free(ELF_Header);
            RETURN;
         ELSIF
            NOT Valid_Program_Header_Entry(ELF_Segment)
         THEN
            Log("The ELF file has a corrupt program header entry.",
               Tag => ELF_Tag, Warn => true);
            Error_Status := format_error;
            Free(ELF_Segment);
            Free(ELF_Header);
            RETURN;
         ELSIF
            ELF_Segment.Segment_Type = loadable_segment
         THEN
            ELF_Memory_Size := ELF_Memory_Size + ELF_Segment.Memory_Size;

            IF
               ELF_Segment.Virtual_Address < ELF_Virtual_Base
            THEN
               ELF_Virtual_Base := ELF_Segment.Virtual_Address;
            END IF;
         END IF;
      END LOOP;

      ELF_Memory_Size := Memory.Align(ELF_Memory_Size, Paging.Page,
         Round_Up => true);

      Tasking.Create(Task_Name, Paging.Size_To_Pages(ELF_Memory_Size),
         Error_Check);

      IF
         Error_Check /= no_error
      THEN
         Log("Task creation error while loading ELF.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := Error_Check;
         Free(ELF_Segment);
         Free(ELF_Header);
         RETURN;
      END IF;

      Tasking.Get_Task_Index(Task_Name, Task_Index, Error_Check);

      IF
         Error_Check /= no_error
      THEN
         Log("Task was created, but its index could not be found.",
            Tag => ELF_Tag, Warn => true);
         Error_Status := Error_Check;
         Free(ELF_Segment);
         Free(ELF_Header);
         RETURN;
      END IF;

      Paging.Kernel_Map_Address_Range
        (Tasks(Task_Index).Physical_Base,
         Tasks(Task_Index).Physical_Base,
         ELF_Memory_Size,
         Write_Access => true);

      FOR
         Index IN 0 .. ELF_Header.Program_Header_Entry_Count - 1
      LOOP
         -- The `Read_File()` function's base offset begins from one, not zero.
         ELF_Segment_Offset := (ELF_Header.Program_Header_Offset + 1) +
            address(ELF_Header.Program_Header_Entry_Size * Index);

         Drive.FAT.Read_File(FAT_Context, File_Path, To_Address(ELF_Segment),
            Error_Check,
            Base_Byte => number(ELF_Segment_Offset),
            Byte_Size => program_header_entry'size / 8);

         -- We've already validated the program header, but when concurrency is
         -- more privalent in the kernel, then it would be wise to check it
         -- again or use some sort of lock.
         IF
            Error_Check /= no_error
         THEN
            Log("Failed to prepare ELF segment for loading.", Tag => ELF_Tag,
               Warn => true);

            -- Remove the mapping as well on an error.
            Paging.Kernel_Map_Address_Range
              (Tasks(Task_Index).Physical_Base,
               Tasks(Task_Index).Physical_Base,
               ELF_Memory_Size,
               Present => false);

            Error_Status := Error_Check;
            Free(ELF_Segment);
            Free(ELF_Header);
            RETURN;
         ELSIF
            ELF_Segment.Segment_Type = loadable_segment
         THEN
            -- TODO: This check is for a segment only containing the BSS.
            -- It seems to have a file offset specified that goes over the
            -- file itself; however, that makes some sense, as it's a "NOBITS"
            -- section. Fix this in a more eloquent manner later.
            IF
               number(ELF_Segment.File_Offset) + ELF_Segment.File_Size <=
                  ELF_File.Size
            THEN -- Need to read section-specific information.
               Drive.FAT.Read_File(FAT_Context, File_Path,
                  Tasks(Task_Index).Physical_Base +
                    (ELF_Segment.Virtual_Address - ELF_Virtual_Base),
                  Error_Check,
                  Base_Byte => number(ELF_Segment.File_Offset + 1),
                  Byte_Size => ELF_Segment.File_Size);
            END IF;

            IF
               Error_Check /= no_error
            THEN
               Log("Failed to load ELF file into task's memory.",
                  Tag => ELF_Tag, Warn => true);

               -- Remove the mapping as well on an error.
               Paging.Kernel_Map_Address_Range
                 (Tasks(Task_Index).Physical_Base,
                  Tasks(Task_Index).Physical_Base,
                  ELF_Memory_Size,
                  Present => false);

               Error_Status := Error_Check;
               Free(ELF_Segment);
               Free(ELF_Header);
               RETURN;
            END IF;

            Paging.Map_Address_Range
              (Tasks(Task_Index).Virtual_Space,
               ELF_Segment.Virtual_Address,
               Tasks(Task_Index).Physical_Base +
                 (ELF_Segment.Virtual_Address - ELF_Virtual_Base),
               ELF_Memory_Size,
               User_Access  => true,
               Write_Access => (ELF_Segment.Permission_Flag =
                  readable_and_writeable_segment),
               No_Execution => (ELF_Segment.Permission_Flag /=
                  readable_and_executable_segment));
         END IF;
      END LOOP;

      -- Now we remove the mapping from the kernel.
      Paging.Kernel_Map_Address_Range
        (Tasks(Task_Index).Physical_Base,
         Tasks(Task_Index).Physical_Base,
         ELF_Memory_Size,
         Present => false);

      Tasks(Task_Index).Alive := true;
      Tasks(Task_Index).Threads(Tasks(Task_Index).Active_Thread).Alive := true;

      Error_Status := no_error;
      Free(ELF_Segment);
      Free(ELF_Header);
      Log("ELF loaded successfully.", Tag => ELF_Tag);
   END Load;

END HAVK_Kernel.Tasking.ELF;
