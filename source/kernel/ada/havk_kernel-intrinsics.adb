-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-intrinsics.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.Intrinsics
WITH
   Refined_State => (CPU_Port_State => NULL, CPU_MSR_State => NULL)
IS
   FUNCTION Bit_Test
     (Value : IN number;
      Bit   : IN number)
      RETURN boolean
   IS
     (boolean'val(Shift_Right(Value, natural(Bit)) AND 1));

END HAVK_Kernel.Intrinsics;
