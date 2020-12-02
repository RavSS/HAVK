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
     (Header    : IN file_header;
      File_Size : IN number)
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
      Header.Program_Header_Offset < address(File_Size)     AND THEN
      Header.Section_Header_Offset < address(File_Size)     AND THEN
      -- The header size must be 64 for ELF64 files.
      Header.Header_Size = 64                               AND THEN
      -- Should have at least a single program header entry.
      Header.Program_Header_Entry_Count > 0                 AND THEN
      -- Now further check the subheader sizes and lengths so they don't go
      -- over the actual file size itself.
      Header.Program_Header_Offset +
         address(Header.Program_Header_Entry_Count *
         Header.Program_Header_Entry_Size) <=
         address(File_Size)                                 AND THEN
      Header.Section_Header_Offset +
         address(Header.Section_Header_Entry_Count *
         Header.Section_Header_Entry_Size) <=
         address(File_Size)                                 AND THEN
      -- The section index for section string names index must be under the
      -- one-based count of all section headers.
      Header.Section_Header_Names_Index < Header.Section_Header_Entry_Count
   );

   FUNCTION Valid_Program_Header_Entry
     (Header       : IN program_header_entry;
      File_Address : IN Memory.canonical_address;
      File_Size    : IN number)
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
         Header.Virtual_Address MOD address(Header.Alignment) = 0)    AND THEN
      -- A check to see if the segment is inside the file (the buffer) itself.
     (IF Header.Segment_Type = loadable_segment THEN
         Header.File_Size < number(Memory.invalid_address'first) AND THEN
         Header.File_Size < File_Size                            AND THEN
         File_Address + Header.File_Offset IN
            File_Address .. File_Address + address(File_Size)    AND THEN
         File_Address + (Header.File_Offset + address(Header.File_Size)) IN
            File_Address .. File_Address + address(File_Size))   AND THEN
         File_Address + Header.File_Offset NOT IN
            Memory.invalid_address'range                         AND THEN
         File_Address + (Header.File_Offset + address(Header.File_Size)) NOT IN
            Memory.invalid_address'range
   );

   PROCEDURE Load
     (File_Address : IN Memory.canonical_address;
      File_Size    : IN number;
      Task_Name    : IN string;
      Error_Status : OUT error)
   IS
      ELF_Header          : CONSTANT file_header
      WITH
         Import   => true,
         Address  => File_Address,
         Annotate => (GNATprove, False_Positive,
                      "object with constraints on bit representation *",
                      "It will be checked for validity later.");

      ELF_Segment_Address : address;

      -- Usually the virtual base is specified by the first "LOAD" segment
      -- section, but I'll check it just in case.
      ELF_Virtual_Base    : address := address'last;
      ELF_Memory_Size     : number := 0;

      Error_Check         : error;
      Task_Index          : number;
   BEGIN
      IF
         File_Address = 0
      THEN
         Log("The ELF file's address is null.", Tag => ELF_Tag, Warn => true);
         Error_Status := memory_error;
         RETURN;
      ELSIF
         File_Size <= 64 -- This is the same as `file_header'size / 8`.
      THEN
         Log("The ELF file is too small.", Tag => ELF_Tag, Warn => true);
         Error_Status := size_error;
         RETURN;
      ELSIF
         File_Address + address(File_Size) IN Memory.invalid_address'range
      THEN
         Log("The ELF file is not stored in canonical memory.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := memory_error;
         RETURN;
      ELSIF
         Task_Name'length > task_name_string'length
      THEN
         Log("The task name is too long.", Tag => ELF_Tag, Warn => true);
         Error_Status := size_error;
         RETURN;
      ELSIF
         Task_Name = (Task_Name'range => NUL)
      THEN
         Log("The task name is null.", Tag => ELF_Tag, Warn => true);
         Error_Status := format_error;
         RETURN;
      ELSIF
         NOT Valid_File_Header(ELF_Header, File_Size)
      THEN
         Log("The ELF file has a corrupt file header.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := format_error;
         RETURN;
      END IF;

      FOR
         Index IN 0 .. ELF_Header.Program_Header_Entry_Count - 1
      LOOP
         ELF_Segment_Address := (File_Address +
            ELF_Header.Program_Header_Offset) +
            address(ELF_Header.Program_Header_Entry_Size * Index);

         IF
            ELF_Segment_Address NOT IN
               File_Address .. File_Address + address(File_Size) OR ELSE
            ELF_Segment_Address IN Memory.invalid_address'range
         THEN
            Log("The ELF file has out-of-range program segments.",
               Tag => ELF_Tag, Warn => true);
            Error_Status := format_error;
            RETURN;
         END IF;

         DECLARE
            ELF_Segment : CONSTANT program_header_entry
            WITH
               Import   => true,
               Address  => ELF_Segment_Address,
               Annotate => (GNATprove, False_Positive,
                            "object with constraints on bit representation *",
                            "It will be checked for validity manually.");
         BEGIN
            IF
               NOT Valid_Program_Header_Entry
                 (ELF_Segment, File_Address, File_Size)
            THEN
               Log("The ELF file has a corrupt program header entry.",
                  Tag => ELF_Tag, Warn => true);
               Error_Status := format_error;
               RETURN;
            ELSIF
               ELF_Segment.Segment_Type = loadable_segment
            THEN
               IF -- Just a wrap-around check.
                  quadword(ELF_Memory_Size) +
                     quadword(ELF_Segment.Memory_Size) >
                     quadword(ELF_Memory_Size)
               THEN
                  ELF_Memory_Size := ELF_Memory_Size + ELF_Segment.Memory_Size;
               ELSE
                  Log("The ELF file exceeds the 64-bit memory space.");
                  Error_Status := size_error;
                  RETURN;
               END IF;

               IF
                  ELF_Segment.Virtual_Address < ELF_Virtual_Base
               THEN
                  ELF_Virtual_Base := ELF_Segment.Virtual_Address;
               END IF;
            END IF;
         END;
      END LOOP;

      ELF_Memory_Size := Memory.Align(ELF_Memory_Size, Paging.Page,
         Round_Up => true);

      IF
         Paging.Size_To_Pages(ELF_Memory_Size) NOT IN
            1 .. (2 * GiB) / Paging.Page -- Current limit for task code spaces.
      THEN
         Log("The ELF file contains no executable code.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := format_error;
         RETURN;
      END IF;

      Create(Task_Name, Paging.Size_To_Pages(ELF_Memory_Size), Error_Check);

      IF
         Error_Check /= no_error
      THEN
         Log("Task creation error while loading ELF.", Tag => ELF_Tag,
            Warn => true);
         Error_Status := Error_Check;
         RETURN;
      END IF;

      Get_Task_Index(Task_Name, Task_Index, Error_Check);

      IF -- TODO: This should not be necessary. Rework the "Tasking" package.
         Error_Check /= no_error            OR ELSE
         Task_Index NOT IN task_limit'range OR ELSE
         Tasks(Task_Index) = NULL
      THEN
         Log("A task was created, but its index could not be found.",
            Tag => ELF_Tag, Warn => true);
         Error_Status := Error_Check;
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
         PRAGMA Loop_Invariant(Tasks(Task_Index) /= NULL);
         DECLARE
            ELF_Segment : CONSTANT program_header_entry
            WITH
               Import   => true,
               Address  => (File_Address + ELF_Header.Program_Header_Offset) +
                            address(ELF_Header.Program_Header_Entry_Size *
                            Index), -- This calculation has been checked.
               Annotate => (GNATprove, False_Positive,
                            "object with constraints on bit representation *",
                            "It will be checked for validity manually.");
         BEGIN
            IF
               ELF_Segment.Segment_Type = loadable_segment
            THEN
               IF -- Make sure we calculate the addresses correctly.
                  -- This first check is a repeat of the validity function to
                  -- let `gnatprove` know that the file calculations are fine.
                  -- When performance matters, this can be redone.
                  NOT Valid_Program_Header_Entry
                    (ELF_Segment, File_Address, File_Size)             OR ELSE
                  -- Canonical address checks.
                  Tasks(Task_Index).Physical_Base +
                    (ELF_Segment.Virtual_Address - ELF_Virtual_Base) IN
                        Memory.invalid_address'range                   OR ELSE
                  Tasks(Task_Index).Physical_Base +
                    (address(ELF_Segment.File_Size) +
                       (ELF_Segment.Virtual_Address - ELF_Virtual_Base)) IN
                           Memory.invalid_address'range                OR ELSE
                  -- Now check if the segment fits inside the task's space.
                  Tasks(Task_Index).Physical_Base +
                    (ELF_Segment.Virtual_Address - ELF_Virtual_Base) NOT IN
                        Tasks(Task_Index).Physical_Base ..
                           Tasks(Task_Index).Physical_Base +
                              address(ELF_Memory_Size)                 OR ELSE
                  Tasks(Task_Index).Physical_Base +
                    (address(ELF_Segment.File_Size) +
                       (ELF_Segment.Virtual_Address - ELF_Virtual_Base)) NOT IN
                           Tasks(Task_Index).Physical_Base ..
                              Tasks(Task_Index).Physical_Base +
                                 address(ELF_Memory_Size)
               THEN
                  Log("The ELF file contains bad segment addresses.",
                     Tag => ELF_Tag, Warn => true);
                  Remove(Task_Index, Error_Check);

                  IF
                     Error_Check /= no_error
                  THEN
                     RAISE Panic
                     WITH
                        Source_Location &
                        " - Failed to remove a dormant task.";
                     PRAGMA Annotate(GNATprove, Intentional,
                        "exception might be raised",
                        "Can't leave behind a permanent zombie task.");
                  END IF;

                  Error_Status := memory_error;
                  RETURN;
               END IF;

               -- TODO: This check is for a segment only containing the BSS. It
               -- seems to have a file offset specified that goes over the file
               -- itself; however, that makes some sense, as it's a "NOBITS"
               -- section. Fix this in a more eloquent manner later. If the
               -- entry has a file size of zero, then don't accidentally
               -- read/load the whole file, as that will cause a page fault.
               IF -- Need to read section-specific information for the above.
                  ELF_Segment.File_Size /= 0 AND THEN
                  quadword(ELF_Segment.File_Offset) + -- Wraparound check.
                     quadword(ELF_Segment.File_Size) <= quadword(File_Size)
               THEN
                  Memory.Copy -- Hopefully this does not explode.
                    (Tasks(Task_Index).Physical_Base +
                       (ELF_Segment.Virtual_Address - ELF_Virtual_Base),
                     File_Address + ELF_Segment.File_Offset,
                     ELF_Segment.File_Size);
               END IF;

               Paging.Map_Address_Range
                 (Tasks(Task_Index).Virtual_Space,
                  ELF_Segment.Virtual_Address,
                  Tasks(Task_Index).Physical_Base +
                    (ELF_Segment.Virtual_Address - ELF_Virtual_Base),
                  ELF_Segment.Memory_Size,
                  User_Access  => true,
                  Write_Access => (ELF_Segment.Permission_Flag =
                     readable_and_writeable_segment),
                  No_Execution => (ELF_Segment.Permission_Flag /=
                     readable_and_executable_segment));
            END IF;
         END;
      END LOOP;

      -- Now we remove the mapping from the kernel.
      Paging.Kernel_Map_Address_Range
        (Tasks(Task_Index).Physical_Base,
         Tasks(Task_Index).Physical_Base,
         ELF_Memory_Size,
         Present => false);

      -- TODO: This check is for `gnatprove` and it isn't actually necessary.
      -- Improve the contracts in the tasking package and remove this.
      IF -- Pretend this is an assumption. The predicate should hold.
         Tasks(Task_Index).Initial_Frames IN 1 .. (2 * GiB) / Paging.Page
            AND THEN
         Tasks(Task_Index).Name /= (Tasks(Task_Index).Name'range => NUL)
            AND THEN
         Tasks(Task_Index).Exit_Code = 0
      THEN
         Tasks(Task_Index).Alive := true;
      ELSE
         Log("The task was not finalised properly.", Tag => ELF_Tag,
            Warn => true);
         Remove(Task_Index, Error_Check);

         IF
            Error_Check /= no_error
         THEN
            RAISE Panic
            WITH
               Source_Location &
               " - Failed to remove a dormant task.";
            PRAGMA Annotate(GNATprove, Intentional,
               "exception might be raised",
               "Can't leave behind a permanent zombie task.");
         END IF;

         Error_Status := attempt_error;
         RETURN;
      END IF;

      Error_Status := no_error;
      Log("ELF loaded successfully as task """ & Task_Name & """.",
         Tag => ELF_Tag);
   END Load;

END HAVK_Kernel.Tasking.ELF;
