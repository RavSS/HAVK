-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-buffer.ads                         --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

-- This package exists as a workaround for shared memory resources and kernel
-- access to them. I've "designed" the kernel to not touch task memory no
-- matter what once the task starts and I do that by passing memory/data purely
-- through registers. It's a fun challenge, but it's silly, so this has to
-- exist. Another way would be for task's to replace their program image with
-- another one, sort of like how the `exec*()` functions work, but that's not
-- bizarre enough for my liking and that is only more feasible if the
-- filesystem is running in the kernel. Data still needs to be touchable by
-- HAVK's kernel somehow.
-- TODO: Support reading and writing other tasks' buffers.
PACKAGE HAVK_Kernel.Tasking.Buffer
WITH
   Abstract_State => Buffer_State
IS
   -- Creates a kernel buffer of a specific size. By default, the only owner of
   -- the buffer is the task itself.
   PROCEDURE Create
     (Task_Identity : IN number;
      Buffer_Size   : IN number;
      Error_Status  : OUT error);

   -- Reads data from a kernel buffer and returns it in a record that can fit
   -- inside the XMM registers. Buffer offset is one-based.
   -- TODO: Does not yet support interacting with another task's buffer on
   -- behalf of a different task.
   PROCEDURE Read
     (Task_Identity : IN number;
      Buffer_Offset : IN number;
      Buffer_Data   : OUT Intrinsics.XMM_registers;
      Error_Status  : OUT error);

   -- Writes data to a kernel buffer from a record that can fit inside the XMM
   -- registers. Buffer offset is one-based.
   -- TODO: Does not yet support interacting with another task's buffer on
   -- behalf of a different task.
   PROCEDURE Write
     (Task_Identity : IN number;
      Buffer_Offset : IN number;
      Buffer_Data   : IN Intrinsics.XMM_registers;
      Error_Status  : OUT error);

   -- Deletes the task's kernel buffer.
   PROCEDURE Delete
     (Task_Identity : IN number);

   -- For internal kernel usage. This is not useful to tasks themselves, as
   -- they do not have direct memory access to them. That may change in the
   -- future in case I decide to implement (actual) shared memory
   -- spaces/mappings between tasks, but not for the time being.
   PROCEDURE Buffer_Base
     (Task_Identity  : IN number;
      Buffer_Address : OUT Memory.canonical_address;
      Buffer_Size    : OUT number;
      Error_Status   : OUT error);

PRIVATE
   Buffer_Tag : CONSTANT string := "BUFFER";

   -- Bitset of tasks which own the respective buffer.
   TYPE buffer_owners IS ARRAY(task_limit'range) OF boolean
   WITH
      Component_Size => 1;

   -- TODO: This should be adjustable for different tasks. It's only here for
   -- the sake of a starting point.
   SUBTYPE buffer_size_limit IS number RANGE 256 .. 8 * MiB;

   TYPE buffer_bytes IS ARRAY(number RANGE <>) OF ALIASED byte
   WITH
      Component_Size => byte_length'enum_rep;

   -- The buffer itself. The data goes at the end, as it's dynamically sized.
   TYPE buffer(Buffer_Size : buffer_size_limit) IS LIMITED RECORD
      -- The index is tied to a task's identity. If the element for that task
      -- is true, then that task can modify this as it seems fit.
      Owners : buffer_owners := (OTHERS => false);
      -- The data itself.
      Data   : ALIASED bytes(1 .. Buffer_Size);
   END RECORD
   WITH
      Pack              => true,
      Dynamic_Predicate => Buffer_Size = buffer.Buffer_Size;
   FOR buffer USE RECORD
      Buffer_Size AT 0 RANGE 0 .. 63;
   END RECORD;

   -- The buffers are dynamically allocated.
   TYPE access_buffer IS ACCESS buffer;

   -- The index is the buffer identity.
   Buffers : ARRAY(task_limit'range) OF access_buffer
   WITH
      Part_Of => Buffer_State;

END HAVK_Kernel.Tasking.Buffer;
