-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA PIO Driver                   --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   System,
   HAVK_Operating_System,
   HAVK_ATA_PIO,
   HAVK_ATA_PIO.GPT,
   HAVK_ATA_PIO.FAT;
USE
   System,
   HAVK_Operating_System,
   HAVK_ATA_PIO;

-- TODO: For now, I'm just loading an example ELF off the drive. This needs to
-- be reworked so it can be a proper "server" task.
PROCEDURE Main
IS
   USE TYPE
      FAT.version;

   -- Cache the ELF file in task memory. Avoid multiple file lookups. Gives a
   -- chance to more strenuously test out heap memory as well.
   TYPE access_bytes IS ACCESS bytes;

   PROCEDURE Copy -- Not bothered doing it the way it's done in-kernel.
     (Destination : IN address;
      Source      : IN address;
      Byte_Size   : IN number)
   WITH
      Import        => true,
      Convention    => C,
      External_Name => "memcpy";

   Main_Tag           : CONSTANT string := "MAIN";
   Error_Check        : error;

   EFI_Boot_Partition : GPT.partition;
   EFI_File_System    : FAT.file_system;

   -- This can be any executable.
   Task_Name          : CONSTANT string := "Framebuffer Tester";
   Task_Executable    : CONSTANT string := ">HAVK>system>FRAMEB~1.ELF";

   ELF                : access_bytes := NULL;
   ELF_Offset         : number := 1;
   ELF_File           : FAT.file;
   ELF_Buffer         : arguments;
   ELF_Data           : ALIASED XMM_registers;

   Padded_Task_Name   : XMM_string := (OTHERS => NUL);
   Packed_Task_Name   : XMM_registers;
BEGIN
   -- TODO: Does not do drive error checking or drive resets. Last time I
   -- checked, this will freeze up the driver if a non-existent drive is
   -- accessed.
   FOR -- Check both buses and both drives for the (U)EFI boot partition.
      I IN 1 .. 4
   LOOP
      GPT.Get_Partition(EFI_Boot_Partition, "EFI",
         Secondary_Bus => (I IN 3 | 4), Secondary_Drive => (I IN 2 | 4));
      EXIT WHEN EFI_Boot_Partition.Present;
   END LOOP;

   IF
      NOT EFI_Boot_Partition.Present
   THEN
      RAISE Program_Error
      WITH
         "No EFI System Partition (ESP) detected.";
   END IF;

   FAT.Get_File_System(EFI_File_System, EFI_Boot_Partition);

   IF
      FAT.Get_FAT_Version(EFI_File_System) /= FAT.FAT16
   THEN
      RAISE Program_Error
      WITH
         "No FAT16 partition detected.";
   END IF;

   FAT.Check_File(EFI_File_System, Task_Executable, Error_Check, ELF_File);

   IF
      Error_Check /= no_error
   THEN
      RAISE Program_Error
      WITH
         "Could not find the framebuffer tester.";
   END IF;

   ELF_Buffer := (buffer_operation, 1, general_register(ELF_File.Size),
      OTHERS => <>);

   IF
      System_Call(ELF_Buffer) /= no_error
   THEN
      RAISE Program_Error
      WITH
         "Failed to create the kernel buffer.";
   END IF;

   ELF := NEW bytes(ELF_Offset .. ELF_File.Size);
   FAT.Read_File(EFI_File_System, Task_Executable, ELF.ALL'address,
      Error_Check);

   IF
      Error_Check /= no_error
   THEN
      RAISE Program_Error
      WITH
         "Failed to create the file cache.";
   END IF;

   WHILE
      ELF_Offset IN ELF'range
   LOOP
      ELF_Buffer := (buffer_operation, 3, general_register(ELF_Offset),
         OTHERS => <>);

      Copy(ELF_Data'address, ELF.ALL(ELF_Offset)'address,
         ELF_Data'size / 8);

      IF
         System_Call(ELF_Buffer, ELF_Data) /= no_error
      THEN
         RAISE Program_Error
         WITH
            "Failed to write to our task's kernel buffer.";
      ELSE
         ELF_Offset := ELF_Offset + (ELF_Data'size / 8);
      END IF;
   END LOOP;

   Padded_Task_Name(Task_Name'range) := XMM_string(Task_Name);
   Packed_Task_Name := To_XMM_Registers(Padded_Task_Name);
   ELF_Buffer := (load_elf_operation, OTHERS => <>); -- Reuse the name.

   IF
      System_Call(ELF_Buffer, Packed_Task_Name) /= no_error
   THEN
      RAISE Program_Error
      WITH
         "Failed to create the new task.";
   END IF;
END Main;