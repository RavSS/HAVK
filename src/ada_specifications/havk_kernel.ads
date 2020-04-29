-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel.ads                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   System;
USE
   System;

-- This is the main package spec for the kernel itself, it just contains
-- some types for usage everywhere else, along with some shortcuts.
PACKAGE HAVK_Kernel
WITH
   Preelaborate => true
IS
   -- Because HAVK is a 64-bit kernel, I'll make the "default" types 64-bit.
   -- Natural number, assuming you include zero. This is to be used everywhere
   -- except for memory addresses.
   TYPE number IS MOD 2**64
   WITH
      Size => 64;

   -- GNAT provides its own intrinsics, which can be optimised slightly so
   -- better than the ones defined by me via inline assembly.
   PRAGMA Provide_Shift_Operators(number);

   -- A type which indicates that a variable's data serves no direct purpose
   -- and only the address of the variable itself is the important information.
   -- This type's limit and size is irrelevant, as it's just a placeholder.
   TYPE void IS MOD 1;

   -- Types used to express memory/data without any specific purpose. It's best
   -- to derive from these types and then provide more information about why
   -- they're going to be used; thus, just pretend they are anonymous arrays,
   -- but with each element having a guaranteed/known size.
   TYPE bits         IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**01 - 1
   WITH
      Component_Size          => 01,
      Default_Component_Value => 00;
   TYPE bytes        IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**08 - 1
   WITH
      Component_Size          => 08,
      Default_Component_Value => 00;
   TYPE words        IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**16 - 1
   WITH
      Component_Size          => 16,
      Default_Component_Value => 00;
   TYPE double_words IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**32 - 1
   WITH
      Component_Size          => 32,
      Default_Component_Value => 00;
   TYPE quad_words   IS ARRAY(number RANGE <>) OF number RANGE 0 .. 2**64 - 1
   WITH
      Component_Size          => 64,
      Default_Component_Value => 00;

   -- A broad exception identity to be used for simple kernel panics anywhere.
   -- There's little point in declaring numerous exception identities, as we're
   -- not actually allowed to handle them in SPARK 2014 (to my understanding).
   Panic : EXCEPTION;

   -- Returns the current file and current line number of where this procedure
   -- was called in a "file:line" string format. Use with the panic exception.
   FUNCTION Source_Location
      RETURN string
   WITH
      Import     => true,
      Convention => Intrinsic;

   -- Helper variables for memory units (non-SI).
   KiB : CONSTANT := 1024;
   MiB : CONSTANT := KiB**2;
   GiB : CONSTANT := KiB**3;
   TiB : CONSTANT := KiB**4;

   -- Imaging function for numeric values. An improvement over the image
   -- attribute is the ability to set a base and that this is also not reliant
   -- on the runtime image functions which the attribute makes a call to.
   -- TODO: Add binary and integer/negative number imaging.
   FUNCTION Image
     (Value   : IN number;
      Base    : IN number := 10;
      Padding : IN number := 0)
      RETURN string
   WITH
      Pre  => Base IN 10 | 16 AND THEN Padding < 64,
      Post => Image'result'first = 1 AND THEN
             (IF Padding = 0 THEN Image'result'last IN 1 .. 64
                 ELSE Image'result'last IN positive(Padding) .. 64);

   -- By default, image addresses in base 16.
   FUNCTION Image
     (Value   : IN address;
      Base    : IN number := 16;
      Padding : IN number := 0)
      RETURN string
   WITH
      Pre  => Base IN 10 | 16 AND THEN Padding < 64,
      Post => Image'result'first = 1 AND THEN
             (IF Padding = 0 THEN Image'result'last IN 1 .. 64
                 ELSE Image'result'last IN positive(Padding) .. 64);

   -- A type that describes certain levels of urgency. This is mainly used
   -- for logging purposes as of now.
   TYPE urgency IS
     (trivial, -- A log that does not really matter at all in the long run.
      nominal, -- Of little urgency. Should be used for meaningful logs.
      warning, -- Self-explanatory. All warnings should be issued with this.
      fatal)   -- The most serious level. Only use for actual errors.
   WITH
      Convention => C;

   -- Stores a log in the main kernel log collection variable.
   -- Default log priority is "trivial".
   PROCEDURE Log
     (Information : IN string;
      Priority    : IN urgency := trivial);

PRIVATE
   -- This controls the maximum amount of log entries. Note that this is
   -- static, it does not use heap allocation. This reserves around 30 KiB as
   -- of now for logs.
   SUBTYPE log_entry_limit IS number RANGE 1 .. 200;

   -- The maximum length of a log. 150 characters max as of now.
   SUBTYPE log_string_limit IS number RANGE 1 .. 150;

   -- A record of a log containing its specific details.
   TYPE log_information IS RECORD
      -- String description of the log's meaning.
      Information : string(positive(log_string_limit'first) ..
         positive(log_string_limit'last)) := (OTHERS => character'val(0));
      -- A field hinting at the context of the log.
      Priority    : urgency := trivial;
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

   -- A wrapper for `Log()` that does not take in an unconstrained array.
   -- Instead, it takes in a null-terminated string. This is for external use.
   PROCEDURE External_Log
     (Pointer  : IN address;
      Priority : IN urgency)
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "ada__log";

   -- This is where the logs are stored. I've kept it to only one collection
   -- as of now to avoid complicating things.
   Logs : log_collection;
END HAVK_Kernel;
