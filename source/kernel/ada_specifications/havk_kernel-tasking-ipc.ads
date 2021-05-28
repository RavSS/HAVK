-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-ipc.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

-- Handles simple message passing as of now. Currently, each message can hold
-- the amount of byte space inside all of the 16 XMM registers; hence, only SSE
-- is supported. AVX support (YMM and ZMM registers) is avoided for now, since
-- the gain is not worth the compatibility probing.
-- TODO: This is a multiple copy method of IPC, which isn't efficient and
-- requires a system call. Later on, implement IPC via virtual memory or page
-- sharing... or don't, as the point here by using these registers is to
-- replicate Intel's SMEP+SMAP, which is only available on more modern CPUs.
PACKAGE HAVK_Kernel.Tasking.IPC
WITH
   Abstract_State => IPC_State
IS
   TYPE message_data IS RECORD
      XMM : Intrinsics.XMM_registers;
   END RECORD;
   FOR message_data USE RECORD
      XMM AT 0 RANGE 0 .. 2047;
   END RECORD;

   -- Place a message in a specific task's message box.
   PROCEDURE Send_Message
     (Sender       : IN number;
      Receiver     : IN number;
      Header       : IN number;
      Subheader    : IN number;
      Data         : IN message_data;
      Error_Status : OUT error)
   WITH
      Post => Error_Status IN no_error | index_error | attempt_error;

   -- Retrieve a message from a specific sender. If the sender index is zero,
   -- then the first used message box of the receiver will be selected.
   PROCEDURE Receive_Message
     (Sender       : IN OUT number;
      Receiver     : IN number;
      Header       : OUT number;
      Subheader    : OUT number;
      Data         : OUT message_data;
      Error_Status : OUT error)
   WITH
      Post => Error_Status IN no_error | index_error | attempt_error;

PRIVATE
   TYPE message_box IS RECORD
      -- Indicates whether or not the message spot is free i.e. has been
      -- received by the receiver. When this is false, the contents of the
      -- components below it should be completely ignored and be ready for
      -- overwriting.
      Used      : boolean := false;
      -- A 64-bit header field. Potentially useful for a port number etc.
      Header    : number := 0;
      -- A 64-bit subheader field. Could be used for data length etc.
      Subheader : number := 0;
      -- The message data that is type-defined elsewhere.
      Data      : message_data;
   END RECORD;

   -- The first index is the index of the receiving task and the second index
   -- is the index of which the receiving task wishes to receive a message
   -- from. Notice that there is no allocation here and it grows exponentially
   -- in size with the task limit. This means that IPC is guaranteed to work,
   -- as allocation can never fail. This might change in the future if I make
   -- the data buffers bigger, but for now, it's acceptable.
   Messages : ARRAY(task_limit, task_limit) OF message_box
   WITH
      Part_Of => IPC_State;

END HAVK_Kernel.Tasking.IPC;
