WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel IS
   -- TODO: Is there a way I can avoid these wrappers for the debug procedures?
   PROCEDURE Debug_Initialise
   IS
   BEGIN
      PRAGMA Debug(Debug.Initialise);
   END Debug_Initialise;

   PROCEDURE Debug_Message(
      Message : IN string)
   IS
   BEGIN
      PRAGMA Debug(Debug.Message(Message));
   END Debug_Message;
END HAVK_Kernel;