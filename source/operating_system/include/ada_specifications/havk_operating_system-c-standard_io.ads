-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-standard_io.ads                --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System.Call;
USE
   HAVK_Operating_System.Call;

-- An Ada implementation of the C standard library's "stdio.h". The package
-- does not do any actual I/O operations itself. It merely acts as an interface
-- to a task that does implement filesystems etc. The logic in this package is
-- kept as dull and simple as fully possible. Nothing is built for the sake of
-- efficiency in here, as seen by how data travels between tasks and how the
-- state of a file is being excessively communicated.
PACKAGE HAVK_Operating_System.C.Standard_IO
WITH
   Preelaborate => true
IS
   -- The task that receives (and sends back) file data. These can be changed
   -- during runtime.
   -- TODO: Implement some sort of task dictionary or something instead of
   -- hardcoding these in.
   Storage_Task : CONSTANT general_register := 4;

   -- Every field inside this record is implementation-defined. The way this
   -- works is that a file state is sent to the storage task and then the
   -- storage task decides to send back file data.
   -- TODO: I can pack this further by picking more sensible types.
   TYPE FILE IS RECORD
      -- Represents the file path of the file itself.
      File_Path     : ALIASED char_array(0 .. 127) := (OTHERS => 0);
      -- What mode the file is open in.
      File_Mode     : ALIASED char_array(0 .. 002) := (OTHERS => 0);
      -- The size of the file's content. A size of zero indicates that the file
      -- has not yet been opened to an I/O task.
      File_Size     : size_t := 0;
      -- The current byte offset/position of the file. This is a zero-based
      -- index.
      File_Offset   : size_t := 0;
      -- If false, then this is doing a read request.
      Write_Request : bool := false;
      -- This controls the amount of incoming/outgoing data during a read or
      -- write over a series of messages.
      Buffer_Size   : size_t := 0;
      Padding       : bytes(169 .. 256);
   END RECORD
   WITH
      Size              => 2048,
      Object_Size       => 2048,
      Convention        => C,
      Dynamic_Predicate => (IF File_Size /= 0 THEN File_Offset < File_Size);

   TYPE FILE_pointer IS ACCESS ALL FILE
   WITH
      Convention => C;

   FUNCTION System_Call IS NEW System_Call_Generic_Data_Function
     (generic_data => FILE);
   PROCEDURE System_Call IS NEW System_Call_Generic_Data_Procedure
     (generic_data => FILE);

   SEEK_SET : CONSTANT := 0;
   SEEK_CUR : CONSTANT := 1;
   SEEK_END : CONSTANT := 2;

   -- TODO: The file mode string is not yet actually used according to its
   -- contents. For now, just reading is supported, so "r" and "rb" are only
   -- valid.
   FUNCTION File_Open
     (File_Path : ALIASED IN char_array;
      File_Mode : ALIASED IN char_array)
      RETURN FILE_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "fopen",
      Pre           => File_Path'address /= Null_Address AND THEN
                       File_Mode'address /= Null_Address;

   -- For the following functions, I cannot figure out if `FILE *` is allowed
   -- to be null when passed or if it's undefined behaviour. I've gone with the
   -- route of adding pre-conditions, just in case.

   FUNCTION File_Close
     (Open_File : IN FILE_pointer)
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "fclose",
      Pre           => Open_File /= NULL;

   FUNCTION File_Read
     (Data_Buffer   : IN void_pointer;
      Element_Size  : IN size_t;
      Element_Count : IN size_t;
      Open_File     : IN FILE_pointer)
      RETURN size_t
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "fread",
      Pre           => Open_File /= NULL AND THEN
                       Data_Buffer /= NULL;

   FUNCTION File_Seek
     (Open_File   : IN FILE_pointer;
      File_Offset : IN long;
      Origin      : IN int)
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "fseek",
      Pre           => Open_File /= NULL AND THEN
                       File_Offset >= 0  AND THEN
                       Origin IN SEEK_SET | SEEK_CUR | SEEK_END;

   FUNCTION File_Tell
     (Open_File : IN FILE_pointer)
      RETURN long
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "ftell",
      Pre           => Open_File /= NULL;

END HAVK_Operating_System.C.Standard_IO;
