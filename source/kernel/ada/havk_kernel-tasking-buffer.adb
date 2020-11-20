-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-buffer.adb                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Deallocation,
   HAVK_Kernel.Memory.Allocator;

PACKAGE BODY HAVK_Kernel.Tasking.Buffer
WITH
   Refined_State => (Buffer_State => Buffers)
IS
   PROCEDURE Create
     (Task_Identity : IN number;
      Buffer_Size   : IN number;
      Error_Status  : OUT error)
   WITH
      Refined_Post => (IF Error_Status = no_error THEN
                          Buffers(Task_Identity) /= NULL AND THEN
                          Buffers(Task_Identity).Buffer_Size = Buffer_Size
                             AND THEN
                          Buffers(Task_Identity).Owners(Task_Identity))
   IS
      PROCEDURE Allocate IS NEW Memory.Allocator.Discriminant_Record
        (generic_discriminant  => buffer_size_limit,
         generic_record        => buffer,
         access_generic_record => access_buffer);
   BEGIN
      IF -- Check if it's a valid task identity.
         Task_Identity NOT IN task_limit'range
      THEN
         Error_Status := index_error;
      ELSIF -- The buffer should not be active.
         Buffers(Task_Identity) /= NULL
      THEN
         Error_Status := attempt_error;
      ELSIF -- The buffer size has to be in a sensible range.
         Buffer_Size NOT IN buffer_size_limit'range
      THEN
         Error_Status := size_error;
      ELSE -- Try allocating it.
         Allocate(Buffers(Task_Identity), Buffer_Size);

         -- The memory manager could occasionally return a null pointer/access
         -- due to allocation failures.
         IF
            Buffers(Task_Identity) /= NULL
         THEN
            PRAGMA Assume(Buffers(Task_Identity).Buffer_Size = Buffer_Size,
               "See the assembly implementation. This should hold here.");
            Buffers(Task_Identity).Owners(Task_Identity) := true;
            Buffers(Task_Identity).Data := (OTHERS => 0); -- Zero it out.
            Error_Status := no_error;
         ELSE
            Error_Status := memory_error;
         END IF;
      END IF;
   END Create;

   PROCEDURE Read
     (Task_Identity : IN number;
      Buffer_Offset : IN number;
      Buffer_Data   : OUT Intrinsics.XMM_registers;
      Error_Status  : OUT error)
   WITH
      SPARK_Mode => off -- The buffer-to-XMM loop crashes `gnatprove`.
   IS
      Offset_Count : number := Buffer_Offset;
   BEGIN
      IF -- Check if it's a valid task identity.
         Task_Identity NOT IN task_limit'range
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF -- The buffer should be active.
         Buffers(Task_Identity) = NULL
      THEN
         Error_Status := attempt_error;
         RETURN;
      ELSIF
         Buffer_Offset NOT IN Buffers(Task_Identity).Data'range
      THEN
         Error_Status := size_error; -- Technically an index error...
         RETURN;
      END IF;

      Buffer_Data := (OTHERS => <>); -- Clear it out first.

      -- TODO: The below crashes `gnatprove`. It does compile correctly.
      Buffer_Copy : FOR
         Register_Block OF Buffer_Data
      LOOP
         FOR
            Byte_Index IN Register_Block.XMM_Bytes'range
         LOOP
            Register_Block.XMM_Bytes(Byte_Index) :=
               Buffers(Task_Identity).Data(Offset_Count);
            Offset_Count := Offset_Count + 1;
            EXIT Buffer_Copy WHEN
               Offset_Count NOT IN Buffers(Task_Identity).Data'range;
         END LOOP;
      END LOOP Buffer_Copy;

      Error_Status := no_error;
   END Read;

   PROCEDURE Write
     (Task_Identity : IN number;
      Buffer_Offset : IN number;
      Buffer_Data   : IN Intrinsics.XMM_registers;
      Error_Status  : OUT error)
   WITH
      SPARK_Mode => off -- The XMM-to-buffer loop crashes `gnatprove`.
   IS
      Offset_Count  : number := Buffer_Offset;
   BEGIN
      IF -- Check if it's a valid task identity.
         Task_Identity NOT IN task_limit'range
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF -- The buffer should be active.
         Buffers(Task_Identity) = NULL
      THEN
         Error_Status := attempt_error;
         RETURN;
      ELSIF
         Buffer_Offset NOT IN Buffers(Task_Identity).Data'range
      THEN
         Error_Status := size_error; -- Technically an index error...
         RETURN;
      END IF;

      -- TODO: The below crashes `gnatprove`. It does compile correctly.
      Block_Copy : FOR
         Register_Block OF Buffer_Data
      LOOP
         FOR
            Byte_Part OF Register_Block.XMM_Bytes
         LOOP
            Buffers(Task_Identity).Data(Offset_Count) := Byte_Part;
            Offset_Count := Offset_Count + 1;
            EXIT Block_Copy WHEN
               Offset_Count NOT IN Buffers(Task_Identity).Data'range;
         END LOOP;
      END LOOP Block_Copy;

      Error_Status := no_error;
   END Write;

   PROCEDURE Delete
     (Task_Identity : IN number)
   WITH
      Refined_Post => (IF Task_Identity IN task_limit'range THEN
                          Buffers(Task_Identity) = NULL)
   IS
      PROCEDURE Free IS NEW Ada.Unchecked_Deallocation
        (object => buffer, name => access_buffer);
   BEGIN
      IF
         Task_Identity IN task_limit'range AND THEN
         Buffers(Task_Identity) /= NULL
      THEN
         Buffers(Task_Identity).Data := (OTHERS => 0); -- Zero it out.
         Free(Buffers(Task_Identity));
      END IF;
   END Delete;

   PROCEDURE Buffer_Base
     (Task_Identity  : IN number;
      Buffer_Address : OUT Memory.canonical_address;
      Buffer_Size    : OUT number;
      Error_Status   : OUT error)
   WITH
      SPARK_Mode   => off, -- "Address" attribute is used to return a pointer.
      Refined_Post => (IF Error_Status = no_error THEN
                          Buffer_Size IN buffer_size_limit'range)
   IS
   BEGIN
      IF -- Check if it's a valid task identity.
         Task_Identity NOT IN task_limit'range
      THEN
         Buffer_Address := address'first;
         Buffer_Size := 0;
         Error_Status := index_error;
         RETURN;
      ELSIF -- The buffer should be active.
         Buffers(Task_Identity) = NULL
      THEN
         Buffer_Address := address'first;
         Buffer_Size := 0;
         Error_Status := attempt_error;
         RETURN;
      ELSE
         Buffer_Address := Buffers(Task_Identity).Data'address;
         Buffer_Size := Buffers(Task_Identity).Buffer_Size;
         Error_Status := no_error;
      END IF;
   END Buffer_Base;

END HAVK_Kernel.Tasking.Buffer;