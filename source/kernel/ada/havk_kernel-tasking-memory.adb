-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-memory.adb                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Memory.Frames,
   HAVK_Kernel.Paging;

PACKAGE BODY HAVK_Kernel.Tasking.Memory
IS
   PROCEDURE Increase
     (Task_Index   : IN number;
      End_Address  : OUT HAVK_Kernel.Memory.canonical_address;
      Error_Status : OUT error)
   IS
      New_Frame    : HAVK_Kernel.Memory.page_address;
   BEGIN
      IF -- Check if the task index is valid and if the task exists.
         Task_Index NOT IN Tasks'range OR ELSE
         NOT Tasks(Task_Index).Present
      THEN
         End_Address  := 0;
         Error_Status := index_error;
         RETURN;
      ELSIF -- Initialise the heap end address if it hasn't already been done.
         Tasks(Task_Index).Heap_End NOT IN user_memory_address'range
      THEN
         Tasks(Task_Index).Heap_End := user_memory_address'first;
      ELSIF -- This shouldn't be reached on systems, but the check is needed.
         Tasks(Task_Index).Heap_End >=
            user_memory_address'last - (Paging.page_size'enum_rep - 1)
      THEN
         End_Address  := user_memory_address'last -
           (Paging.page_size'enum_rep - 1);
         Error_Status := attempt_error;
         RETURN;
      END IF;

      HAVK_Kernel.Memory.Frames.Allocate(New_Frame, Task_Index,
         Frame_Count => 1);

      IF
         New_Frame = HAVK_Kernel.Memory.Frames.Null_Frame_Address
      THEN
         End_Address  := 0;
         Error_Status := memory_error;
         RETURN;
      END IF;

      Paging.Map_Address -- Make the old heap end now writable.
        (Tasks(Task_Index).Virtual_Space,
         Virtual_Address  => Tasks(Task_Index).Heap_End,
         Physical_Address => New_Frame,
         Write_Access     => true,
         User_Access      => true);

      Tasks(Task_Index).Heap_End := -- Now extend the heap end address.
         Tasks(Task_Index).Heap_End + Paging.page_size'enum_rep;

      End_Address  := Tasks(Task_Index).Heap_End;
      Error_Status := no_error;
   END Increase;

END HAVK_Kernel.Tasking.Memory;
