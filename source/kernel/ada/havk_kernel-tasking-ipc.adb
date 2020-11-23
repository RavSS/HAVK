-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-ipc.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Tasking.IPC
WITH
   Refined_State => (IPC_State => Messages)
IS
   PROCEDURE Send_Message
     (Sender       : IN number;
      Receiver     : IN number;
      Header       : IN number;
      Subheader    : IN number;
      Data         : IN message_data;
      Error_Status : OUT error)
   IS
   BEGIN
      IF
         Sender NOT IN task_limit'range   OR ELSE
         Receiver NOT IN task_limit'range OR ELSE
         Tasks(Sender) = NULL             OR ELSE
         Tasks(Receiver) = NULL
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF
         Messages(Receiver, Sender).Used
      THEN
         Error_Status := attempt_error;
         RETURN;
      END IF;

      Messages(Receiver, Sender) :=
        (Used      => true,
         Header    => Header,
         Subheader => Subheader,
         Data      => Data);

      Error_Status := no_error;
   END Send_Message;

   PROCEDURE Receive_Message
     (Sender       : IN number;
      Receiver     : IN number;
      Header       : OUT number;
      Subheader    : OUT number;
      Data         : OUT message_data;
      Error_Status : OUT error)
   IS
   BEGIN
      IF
         Sender NOT IN task_limit'range   OR ELSE
         Receiver NOT IN task_limit'range OR ELSE
         Tasks(Sender) = NULL             OR ELSE
         Tasks(Receiver) = NULL
      THEN
         Header    := number'first;
         Subheader := number'first;
         Data      := (OTHERS => <>);

         Error_Status := index_error;
         RETURN;
      ELSIF
         NOT Messages(Receiver, Sender).Used
      THEN
         Header    := number'first;
         Subheader := number'first;
         Data      := (OTHERS => <>);

         Error_Status := attempt_error;
         RETURN;
      END IF;

      Header    := Messages(Receiver, Sender).Header;
      Subheader := Messages(Receiver, Sender).Subheader;
      Data      := Messages(Receiver, Sender).Data;

      Messages(Receiver, Sender).Used := false;
      Error_Status := no_error;
   END Receive_Message;

END HAVK_Kernel.Tasking.IPC;
