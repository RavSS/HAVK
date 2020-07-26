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
      Port         : IN number;
      Data         : IN message_data;
      Length       : IN number;
      Error_Status : OUT error)
   IS
   BEGIN
      IF
         Sender NOT IN Tasks'range   OR ELSE
         Receiver NOT IN Tasks'range OR ELSE
         Tasks(Sender) = NULL        OR ELSE
         Tasks(Receiver) = NULL
      THEN
         Error_Status := index_error;
         RETURN;
      ELSIF
         Length > Message_Data_Max_Length
      THEN
         Error_Status := size_error;
         RETURN;
      END IF;

      FOR
         Message_Index IN Messages'range
      LOOP
         IF
            NOT Messages(Message_Index).Used
         THEN
            Messages(Message_Index) :=
              (Used     => true,
               Sender   => Sender,
               Receiver => Receiver,
               Port     => Port,
               Data     => Data,
               Length   => Length);
            Error_Status := no_error;
            RETURN;
         END IF;
      END LOOP;

      Drop_Message(Messages'first);
      Messages(Messages'last) :=
        (Used     => true,
         Sender   => Sender,
         Receiver => Receiver,
         Port     => Port,
         Data     => Data,
         Length   => Length);
      Error_Status := no_error;
   END Send_Message;

   PROCEDURE Receive_Message
     (Sender       : OUT number;
      Receiver     : IN number;
      Port         : OUT number;
      Data         : OUT message_data;
      Length       : OUT number;
      Error_Status : OUT error)
   IS
   BEGIN
      IF
         Receiver NOT IN Tasks'range OR ELSE
         Tasks(Receiver) = NULL
      THEN
         Error_Status := index_error;
      ELSE
         FOR
            Message_Index IN Messages'range
         LOOP
            IF
               Messages(Message_Index).Used AND THEN
               Messages(Message_Index).Receiver = Receiver
            THEN
               Sender := Messages(Message_Index).Sender;
               Port   := Messages(Message_Index).Port;
               Data   := Messages(Message_Index).Data;
               Length := Messages(Message_Index).Length;
               Error_Status := no_error;

               Drop_Message(Message_Index);
               RETURN;
            END IF;
         END LOOP;

         Error_Status := attempt_error;
      END IF;

      Sender := 0;
      Port   := 0;
      Data   := (OTHERS => <>);
      Length := 0;
   END Receive_Message;

   PROCEDURE Drop_Message
     (Message_Index : IN message_limit)
   IS
   BEGIN
      IF
         Message_Index /= Messages'last
      THEN
         Messages(Message_Index .. Messages'last - 1) :=
            Messages(Message_Index + 1 .. Messages'last);
      END IF;

      Messages(Messages'last) := (OTHERS => <>);
   END Drop_Message;

END HAVK_Kernel.Tasking.IPC;
