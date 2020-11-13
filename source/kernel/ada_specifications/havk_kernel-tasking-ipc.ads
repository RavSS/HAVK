-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-ipc.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

-- Handles simple message passing as of now. Currently, each message can hold
-- the amount of byte space inside all of the 16 XMM registers; hence, only SSE
-- is supported.
-- TODO: Switch to AVX, as the YMM registers are bigger (double the total
-- space) and most systems after i.e. Sandy Bridge should have AVX support.
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
      XMM AT 0 RANGE 0 .. (128 * Intrinsics.XMM_registers'length) - 1;
   END RECORD;

   -- Place a message in the message buffer.
   PROCEDURE Send_Message
     (Sender       : IN number;
      Receiver     : IN number;
      Port         : IN number;
      Data         : IN message_data;
      Length       : IN number;
      Error_Status : OUT error);

   -- Get the oldest message in the message buffer. Note that you cannot
   -- specify the oldest message by a particular sender for the purpose of
   -- forcing users to obtain the actual oldest message first.
   PROCEDURE Receive_Message
     (Sender       : OUT number;
      Receiver     : IN number;
      Port         : OUT number;
      Data         : OUT message_data;
      Length       : OUT number;
      Error_Status : OUT error);

PRIVATE
   -- The limit for the amount of messages that can be stored.
   SUBTYPE message_limit IS number RANGE 1 .. 1024;

   Message_Data_Max_Length : CONSTANT number :=
     (message_data'size AND number'last)
   WITH
      Annotate => (GNATprove, False_Positive, "range check might fail",
                   "The bit size can't be outside the range.");

   TYPE message IS RECORD
      -- Indicates whether or not the message spot is free.
      Used     : boolean := false;
      -- The task index of the sending task.
      Sender   : task_limit := task_limit'first;
      -- The task index of the receiving task.
      Receiver : task_limit := task_limit'first;
      -- The port the message will be delivered to.
      -- TODO: I have not yet defined the semantics around this very well.
      Port     : number := 0;
      -- The message data that is type-defined elsewhere.
      Data     : message_data;
      -- The bit length of the message.
      Length   : number RANGE 0 .. Message_Data_Max_Length := 0;
   END RECORD
   WITH
      Alignment => 16;

   -- The buffer for where message records are stored. The oldest messages are
   -- closest to the first index and the newest messages are appended. I've yet
   -- again avoided a pointer based structure, as I couldn't manage to do it
   -- due to move permissions. Yes, pointers (or no copying at all) would be
   -- the sensible thing to do here, but I don't want to turn SPARK mode off.
   Messages : ARRAY(message_limit) OF message
   WITH
      Part_Of   => IPC_State,
      Alignment => 16;

   -- Removes the message at the specified index and always makes sure that
   -- there's at least one space left.
   PROCEDURE Drop_Message
     (Message_Index : IN message_limit)
   WITH
      Post => NOT Messages(Messages'last).Used;

END HAVK_Kernel.Tasking.IPC;
