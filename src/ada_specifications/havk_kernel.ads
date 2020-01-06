-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel.ads                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   System,
   System.Address_Image,
   Ada.Unchecked_Conversion;
USE
   System;

-- This is the main package spec for the kernel itself, it just contains
-- some types for usage everywhere else, along with some shortcuts.
PACKAGE HAVK_Kernel
IS
   -- Because HAVK is a 64-bit kernel, I'll make the "default" types 64-bit.

   -- Natural number, assuming you include zero. This is to be used everywhere,
   -- including for memory addresses that you do not expect to overlay or
   -- import into variables/objects.
   TYPE number  IS MOD 2**64
   WITH
      Size           => 64;
   TYPE numbers IS ARRAY(number RANGE <>) OF number
   WITH
      Component_Size => 64;

   -- GNAT provides its own intrinsics, which can be optimised slightly so
   -- better than the ones defined by me via inline assembly.
   PRAGMA Provide_Shift_Operators(number);

   -- A type which indicates that a variable's data serves no direct purpose
   -- and only the address of the variable itself is the important information.
   -- This type's limit and size is irrelevant, as it's just a placeholder.
   TYPE void IS MOD 1;

   -- This is for converting "address" to "number" and vice versa.
   FUNCTION Address_Value IS NEW Ada.Unchecked_Conversion
     (Source => address, Target =>  number);
   FUNCTION Address_Value IS NEW Ada.Unchecked_Conversion
     (Source =>  number, Target => address);

   -- Helper variables for memory units (non-SI).
   KiB : CONSTANT number := 1024;
   MiB : CONSTANT number := KiB**2;
   GiB : CONSTANT number := KiB**3;
   TiB : CONSTANT number := KiB**4;

   -- Avoids a "WITH" for "System.Address_Image" everywhere.
   FUNCTION Hex_Image
     (Value : IN address)
      RETURN string
   RENAMES System.Address_Image;

   -- A type that describes certain levels of urgency. This is mainly used
   -- for logging purposes as of now.
   TYPE urgency IS
     (trivial, -- A log that does not really matter at all in the long run.
      nominal, -- Of little urgency. Should be used for meaningful logs.
      warning, -- Self-explanatory. All warnings should be issued with this.
      fatal);  -- The most serious level. Only use for actual errors.

   -- Stores a log in the main kernel log collection variable.
   -- Default log priority is "trivial".
   PROCEDURE Log
     (Information : IN string;
      Priority    : IN urgency := trivial);

PRIVATE
   -- This controls the maximum amount of log entries. Note that this is
   -- static, it does not use heap allocation. This reserves around 30 KiB as
   -- of now for logs.
   TYPE log_entry_limit IS RANGE 1 .. 200;

   -- A record of a log containing its specific details.
   TYPE log_information IS RECORD
      -- String description of the log's meaning. 150 characters max as of now.
      Information : string(1 .. 150) := (OTHERS => character'val(0));
      -- A field hinting at the context of the log.
      Priority    : urgency          := trivial;
   END RECORD;

   -- An array type that indicates how logs are indexed.
   TYPE log_entries IS ARRAY(log_entry_limit) OF log_information;

   -- The main log structure.
   TYPE log_collection IS RECORD
      -- A list of the log entries.
      Log_List : log_entries;
      -- A variable that indicates what the current log index is.
      Last_Log : log_entry_limit := log_entry_limit'first;
   END RECORD;

   -- This is where the logs are stored. I've kept it to only one collection
   -- as of now to avoid complicating things.
   Logs : log_collection;
END HAVK_Kernel;
