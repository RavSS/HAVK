WITH
   HAVK_Kernel.Debug;

PACKAGE BODY HAVK_Kernel IS
   -- TODO: Is there a way I can avoid these wrappers for the debug procedures?
   PROCEDURE Debug_Initialize
   IS
   BEGIN
      Debug.Initialize;
   END Debug_Initialize;

   PROCEDURE Debug_Message(
      Message : IN string)
   IS
   BEGIN
      Debug.Message(Message);
   END Debug_Message;
END HAVK_Kernel;