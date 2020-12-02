-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System Drive Manager                    --
-- Filename        -- havk_drive_manager.ads                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   System,
   HAVK_Operating_System,
   HAVK_Operating_System.Global,
   HAVK_Operating_System.Utility;
USE
   System,
   HAVK_Operating_System,
   HAVK_Operating_System.Global,
   HAVK_Operating_System.Utility;

-- A namespace for storage drive management. Everything in this package so far
-- has to do with the ATA driver's IPC interface, meaning that it is currently
-- geared for ATA PIO operations.
PACKAGE HAVK_Drive_Manager
WITH
   Preelaborate   => true,
   Abstract_State =>
   (
      Drive_State
      WITH
         External => (Async_Readers, Async_Writers,
                         Effective_Reads, Effective_Writes)
   )
IS
   USE
      Global.ATA;

   -- Contacts the ATA task to get sectors.
   PROCEDURE PIO_Read
     (Sector_Base     : IN logical_block_address;
      Sector_Count    : IN number;
      Destination     : IN address;
      Secondary_Bus   : IN boolean := false;
      Secondary_Drive : IN boolean := false)
   WITH
      Global => (In_Out => (CPU_Port_State, Drive_State)),
      Pre    => Sector_Count IN 1 .. 2**16 - 1;

   -- TODO: Implement sector-write support.

END HAVK_Drive_Manager;