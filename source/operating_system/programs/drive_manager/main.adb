-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Drive Manager                    --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Global,
   HAVK_Drive_Manager,
   HAVK_Drive_Manager.GPT;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Global,
   HAVK_Drive_Manager;

PROCEDURE Main
WITH
   No_Return => true
IS
   USE
      Global.Drive_Manager;

   FUNCTION System_Call IS NEW System_Call_Generic_Data_Function
     (generic_data => partitioned_PIO_request);

   EFI_System_Partition : partition;
   Call_Arguments       : arguments;
   Request_Data         : ALIASED partitioned_PIO_request;
   Sector_Part          : ARRAY(1 .. 2) OF ALIASED XMM_registers
   WITH
      Size => 2048 * 2; -- Effectively packed together.
BEGIN
   FOR -- Check both buses and both drives for the (U)EFI boot partition.
      I IN 1 .. 4
   LOOP
      GPT.Get_Partition(EFI_System_Partition, "EFI",
         Secondary_Bus => I > 2, Secondary_Drive => I IN 2 | 4);
      EXIT WHEN EFI_System_Partition.Present;
   END LOOP;

   IF
      NOT EFI_System_Partition.Present
   THEN
      RAISE Program_Error
      WITH
         "Could not obtain EFI System Partition (ESP) from the ATA driver.";
   END IF;

   LOOP
      FOR
         Task_Index IN general_register RANGE 1 .. 256
      LOOP
         Call_Arguments :=
           (receive_message_operation, Task_Index, OTHERS => <>);

         IF
            System_Call(Call_Arguments, Request_Data'access) = no_error
         THEN
            IF -- TODO: Add in some form of authentication or something.
               NOT Request_Data.Partition_Data.Present
            THEN
               Request_Data :=
                 (Partition_Data => EFI_System_Partition, OTHERS => <>);

               LOOP
                  Call_Arguments :=
                    (send_message_operation, Task_Index, OTHERS => <>);

                  EXIT WHEN System_Call
                    (Call_Arguments, Request_Data'access) = no_error;

                  Call_Arguments := (yield_operation, OTHERS => <>);
                  System_Call(Call_Arguments);
               END LOOP;
            ELSIF
               EFI_System_Partition = Request_Data.Partition_Data AND THEN
               Request_Data.Sector_Base IN
                  EFI_System_Partition.LBA_First ..
                     EFI_System_Partition.LBA_Last
            THEN
               IF -- TODO: Only read support for now.
                  NOT Request_Data.Write_Request
               THEN
                  PIO_Read(Request_Data.Sector_Base, 1, Sector_Part(1)'address,
                     Request_Data.Secondary_Bus, Request_Data.Secondary_Drive);

                  LOOP
                     Call_Arguments.Operation_Call := send_message_operation;
                     EXIT WHEN
                        System_Call(Call_Arguments, Sector_Part(1)) = no_error;
                     Call_Arguments.Operation_Call := yield_operation;
                     System_Call(Call_Arguments);
                  END LOOP;

                  LOOP
                     Call_Arguments.Operation_Call := send_message_operation;
                     EXIT WHEN
                        System_Call(Call_Arguments, Sector_Part(2)) = no_error;
                     Call_Arguments.Operation_Call := yield_operation;
                     System_Call(Call_Arguments);
                  END LOOP;
               END IF;
            END IF;
         END IF;
      END LOOP;
   END LOOP;
END Main;