-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-call-logging.ads                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Operating_System.Call.Logging
WITH
   Preelaborate => true
IS
   PROCEDURE Log
     (Information : IN string;
      Tag         : IN string  := "N/A";
      Warn        : IN boolean := false;
      Critical    : IN boolean := false)
   WITH
      Pre => Information'length <= XMM_string'length - 10 AND THEN
             Tag'first = positive'first                   AND THEN
             Tag'last IN positive'first .. 8;

END HAVK_Operating_System.Call.Logging;
