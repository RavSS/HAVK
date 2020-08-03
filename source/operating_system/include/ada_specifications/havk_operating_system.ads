-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system.ads                              --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

-- A package with some simple types for general style unity between programs.
-- TODO: Either add this to the runtime or let programs use it regularly.
PACKAGE HAVK_Operating_System
WITH
   Pure => true
IS
   TYPE number IS MOD 2**64;

END HAVK_Operating_System;
