-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call-tasking.ads                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Operating_System.Call.Tasking
WITH
   Preelaborate => true
IS
   -- See the "HAVK_Kernel.Tasking" package for details about the tasking data
   -- structures.
   SUBTYPE task_name_string IS string(1 .. 64);
   TYPE task_status IS RECORD
      Index   : number := 0;
      Alive   : boolean := false;
      Name    : task_name_string := (OTHERS => NUL);
   END RECORD;

   FOR task_status USE RECORD
      Index   AT 00 RANGE 0 .. 63;
      Alive   AT 08 RANGE 0 .. 07;
      Name    AT 09 RANGE 0 .. (8 * task_name_string'length) - 1;
   END RECORD;

   TYPE padded_task_status IS RECORD
      Data   : task_status;
      Zeroed : bytes(1 .. 176);
   END RECORD
   WITH
      Size        => 2048,
      Object_Size => 2048;

   FUNCTION To_Status IS NEW Ada.Unchecked_Conversion
     (source => XMM_string, target => padded_task_status);

   -- Finds the specified task if it can and returns information about it if it
   -- can. If it can't, then the returned index will be zero.
   PROCEDURE Task_Finder
     (Task_Name : IN string;
      Status    : OUT task_status);

   PROCEDURE Exit_Task
     (Return_Code : IN number)
   WITH
      No_Return => true;

END HAVK_Operating_System.Call.Tasking;
