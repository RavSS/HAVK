-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-exceptions.adb                     --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;

PACKAGE BODY HAVK_Kernel.Tasking.Exceptions
IS
   PROCEDURE Null_Task_Check
   IS
   BEGIN
      -- Let the active task killer handle the panic when the active task is
      -- neither dead or alive.
      WHILE
         Tasks(Active_Task) = NULL
      LOOP
         Tasking.Kill_Active_Task(1);
      END LOOP;
   END Null_Task_Check;

   PROCEDURE Page_Fault_Handler
     (Error_Location : IN address;
      Error_Code     : IN number)
   IS
      FUNCTION Read_CR2
         RETURN address
      WITH
         Global            => (Input => Paging.MMU_State),
         Volatile_Function => true,
         Import            => true,
         Convention        => Assembler,
         External_Name     => "assembly__get_page_fault_address";

      -- The fault address is always in the CR2 register, which we presume is
      -- loaded already, as this should be called from ISR 14's handler.
      Fault_Address : CONSTANT address := Read_CR2;

      -- The below conditionals describe why the page fault was raised.
      -- READ: https://wiki.osdev.org/Exceptions#Page_Fault

      Present_Field : CONSTANT string :=
      (
         IF
            Intrinsics.Bit_Test(Error_Code, 0)
         THEN
            "Page-protection violation, "
         ELSE
            "Page not present, "
      );

      Write_Field   : CONSTANT string :=
      (
         IF
            Intrinsics.Bit_Test(Error_Code, 1)
         THEN
            "occurred during write."
         ELSE
            "occurred during read."
      );

      -- TODO: Add more to describe the page fault. I've only covered 2 fields.
   BEGIN
      Null_Task_Check;

      Log("Task """ & Tasks(Active_Task).Name & """ " &
         "page fault - Error code: 0x" & Image(Error_Code, Base => 16) &
         " - Fault address: 0x" & Image(Fault_Address) &
         " - Instruction address: 0x" & Image(Error_Location) & " - " &
         Present_Field & Write_Field, Tag => Tasking_Exception_Tag,
         Warn => true);

      Tasking.Kill_Active_Task(1);
   END Page_Fault_Handler;

   PROCEDURE Zero_Division_Handler
     (Error_Location : IN address)
   IS
   BEGIN
      Null_Task_Check;

      Log("Task """ & Tasks(Active_Task).Name & """ " &
         "division by zero - Instruction address: 0x" &
         Image(Error_Location) & '.', Tag => Tasking_Exception_Tag,
         Warn => true);

      Tasking.Kill_Active_Task(1);
   END Zero_Division_Handler;

END HAVK_Kernel.Tasking.Exceptions;
