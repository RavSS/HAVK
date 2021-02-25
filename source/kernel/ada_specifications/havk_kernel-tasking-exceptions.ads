-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-exceptions.ads                     --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

-- This package contains exception handling for tasks.
-- TODO: Many handlers are missing e.g. a general protection fault handler.
PACKAGE HAVK_Kernel.Tasking.Exceptions
IS
   -- Handles page faults caused by tasks.
   PROCEDURE Page_Fault_Handler
     (Error_Location : IN address;
      Error_Code     : IN number); -- The error code should be 32 bits.

   -- Handles a divide-by-zero error.
   PROCEDURE Zero_Division_Handler
     (Error_Location : IN address);

PRIVATE
   Tasking_Exception_Tag : CONSTANT string := "TASKEXCP";

   PROCEDURE Null_Task_Check
   WITH
      Inline => true,
      Post   => Tasks(Active_Task).Present;

END HAVK_Kernel.Tasking.Exceptions;
