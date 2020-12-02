-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System ATA Driver                       --
-- Filename        -- main.adb                                               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_Operating_System.Global,
   HAVK_ATA.PIO;
USE
   HAVK_Operating_System,
   HAVK_Operating_System.Call,
   HAVK_ATA.PIO;

PROCEDURE Main
WITH
   No_Return => true
IS
   USE
      Global.ATA;

   FUNCTION System_Call IS NEW System_Call_Generic_Data_Function
     (generic_data => PIO_request);

   Call_Arguments : arguments;
   Request_Data   : ALIASED PIO_request;
   Sector_Part    : ARRAY(1 .. 2) OF ALIASED XMM_registers
   WITH
      Size => 2048 * 2; -- Effectively packed together.
BEGIN
   LOOP
      FOR
         Task_Index IN general_register RANGE 1 .. 256
      LOOP
         Call_Arguments.Operation_Call := receive_message_operation;
         Call_Arguments.Argument_1 := Task_Index;

         IF
            System_Call(Call_Arguments, Request_Data'access) = no_error
         THEN
            IF
               Request_Data.Sector_Base IN logical_block_address'range
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