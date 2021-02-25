-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Drive Manager                    --
-- Filename        -- havk_drive_manager.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Operating_System.Call;
USE
   HAVK_Operating_System.Call;

PACKAGE BODY HAVK_Drive_Manager
WITH
   Refined_State => (Drive_State => NULL)
IS
   PROCEDURE PIO_Read
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Destination     : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   IS
      FUNCTION System_Call IS NEW Call.System_Call_Generic_Data_Function
        (generic_data => PIO_request);

      TYPE sector_parts IS ARRAY(number RANGE <>, number RANGE <>) OF
         XMM_registers
      WITH
         Component_Size => 2048,
         Pack           => true;

      ATA_Task            : CONSTANT general_register := 2;
      Call_Arguments      : arguments;
      Request_Data        : ALIASED PIO_request;
      Sector_Data         : XMM_registers;
      Sectors_Destination : sector_parts(1 .. Sector_Count, 1 .. 2)
      WITH
         Import  => true,
         Address => Destination;
   BEGIN
      FOR
         Sector_Index IN 1 .. Sector_Count
      LOOP
         Request_Data :=
           (Sector_Base     => Sector_Base + (Sector_Index - 1),
            Secondary_Bus   => Secondary_Bus,
            Secondary_Drive => Secondary_Drive,
            Write_Request   => false,
            OTHERS          => <>);

         Call_Arguments := (send_message_operation, ATA_Task, OTHERS => <>);

         LOOP
            EXIT WHEN
               System_Call(Call_Arguments, Request_Data'access) = no_error;
         END LOOP;

         Call_Arguments := (receive_message_operation, ATA_Task, OTHERS => <>);

         FOR
            Sector_Part_Index IN number RANGE 1 .. 2
         LOOP
            LOOP
               EXIT WHEN System_Call(Call_Arguments, Sector_Data) = no_error;
            END LOOP;

            Sectors_Destination(Sector_Index, Sector_Part_Index) :=
               Sector_Data;
         END LOOP;
      END LOOP;
   END PIO_Read;

END HAVK_Drive_Manager;
