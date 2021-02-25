-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call-interrupt.ads               --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

-- This package deals with user space interrupts. It's essentially empty at the
-- moment.
PACKAGE HAVK_Operating_System.Call.Interrupt
WITH
   Preelaborate => true
IS
   IRQ_Base : CONSTANT := 32;

END HAVK_Operating_System.Call.Interrupt;
