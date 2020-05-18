-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
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
   Preelaborate   => true,
   Abstract_State => Logging_State
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

   -- A general error type for returning errors from procedures and functions.
   -- Expand this as we go. It's a rough equivalent of Linux or Windows error
   -- codes. The exact meaning of these should be defined by what returns them
   -- and anything that outputs them should have a postcondition membership
   -- check as well so a subprogram only returns intended error values.
   TYPE error IS
     (no_error,         -- The operation succeeded.
      attempt_error,    -- The respective attempt failed; try again.
      memory_error,     -- An error to do with memory i.e. allocation.
      permission_error, -- Invalid permissions to do something.
      size_error,       -- An incorrect size or length was specified.
      format_error,     -- The format of a piece of data is incorrect.
      hardware_error,   -- Used for when there's any hardware issues.
      path_error)       -- A path is invalid or the file/directory is missing.
   WITH
      Default_Value => no_error,
      Convention    => C;

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

   -- Stores a log in the main kernel log collection variable.
   PROCEDURE Log
     (Information : IN string;
      Tag         : IN string  := "N/A";
      Warn        : IN boolean := false;
      Critical    : IN boolean := false)
   WITH
      Pre => Tag'first = positive'first AND THEN
             Tag'last IN positive'first .. 8; -- Check the tag statically.

PRIVATE
   -- This controls the maximum amount of log entries. Note that this is
   -- static, it does not use heap allocation.
   SUBTYPE log_entry_limit IS positive RANGE 1 .. 150;

   -- A limit for a tag so you know where the log roughly came from without
   -- needing to search for the entire log string in the kernel source files.
   -- Make sure this matches the precondition on `Log()`.
   SUBTYPE log_tag_limit IS positive RANGE 1 .. 8;

   -- The maximum length of a log.
   SUBTYPE log_string_limit IS positive RANGE 1 .. 300;

   -- A record of a log containing its specific details.
   TYPE log_information IS RECORD
      -- String description of the log's meaning.
      Information : string(log_string_limit) := (OTHERS => character'val(0));
      -- The tag of the log.
      Tag         : string(log_tag_limit) := (OTHERS => character'val(0));
      -- When true, the log is a warning.
      Warned      : boolean := false;
      -- When true, the log is critical information.
      Critical    : boolean := false;
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
   -- Instead, it takes in null-terminated strings. This is for external use.
   PROCEDURE External_Log
     (Log_Pointer : IN address;
      Tag_Pointer : IN address;
      Warn        : IN character;
      Critical    : IN character)
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "ada__log";

   -- This is where the logs are stored. I've kept it to only one collection
   -- as of now to avoid complicating things.
   Logs : log_collection
   WITH
      Part_Of => Logging_State;
END HAVK_Kernel;
