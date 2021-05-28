-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel.ads                                        --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2021                --
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
      Size     => 64,
      Annotate => (GNATprove, No_Wrap_Around);

   -- GNAT provides its own intrinsics, which can be optimised slightly so
   -- better than the ones defined by me via inline assembly.
   PRAGMA Provide_Shift_Operators(number);

   -- A type which indicates that a variable's data serves no direct purpose
   -- and only the address of the variable itself is the important information.
   -- This type's limit and size is irrelevant, as it's just a placeholder.
   TYPE void IS MOD 1;

   -- The addressable bit lengths of memory for x86-64 (only including the
   -- general integer registers).
   TYPE addressable_length IS
     (byte_length,
      word_length,
      doubleword_length,
      quadword_length);
   FOR addressable_length USE
     (byte_length       => 08,
      word_length       => 16,
      doubleword_length => 32,
      quadword_length   => 64);

   -- Unsigned types used to express memory/data without any specific purpose.
   TYPE bit        IS MOD 2**1
   WITH
      Size        => 1,
      Object_Size => byte_length'enum_rep;
   TYPE byte       IS MOD 2**byte_length'enum_rep
   WITH
      Size        => byte_length'enum_rep,
      Object_Size => byte_length'enum_rep;
   TYPE word       IS MOD 2**word_length'enum_rep
   WITH
      Size        => word_length'enum_rep,
      Object_Size => word_length'enum_rep;
   TYPE doubleword IS MOD 2**doubleword_length'enum_rep
   WITH
      Size        => doubleword_length'enum_rep,
      Object_Size => doubleword_length'enum_rep;
   TYPE quadword   IS MOD 2**quadword_length'enum_rep
   WITH
      Size        => quadword_length'enum_rep,
      Object_Size => quadword_length'enum_rep;

   -- The arrays of the above types. It's best to derive from these types and
   -- then provide more information about why they're going to be used; thus,
   -- just pretend they are anonymous arrays, but with each element having a
   -- guaranteed/known size.
   TYPE bits        IS ARRAY(number RANGE <>) OF bit
   WITH
      Component_Size          => 1,
      Default_Component_Value => bit'first;
   TYPE bytes       IS ARRAY(number RANGE <>) OF ALIASED byte
   WITH
      Component_Size          => byte_length'enum_rep,
      Default_Component_Value => byte'first;
   TYPE words       IS ARRAY(number RANGE <>) OF ALIASED word
   WITH
      Component_Size          => word_length'enum_rep,
      Default_Component_Value => word'first;
   TYPE doublewords IS ARRAY(number RANGE <>) OF ALIASED doubleword
   WITH
      Component_Size          => doubleword_length'enum_rep,
      Default_Component_Value => doubleword'first;
   TYPE quadwords   IS ARRAY(number RANGE <>) OF ALIASED quadword
   WITH
      Component_Size          => quadword_length'enum_rep,
      Default_Component_Value => quadword'first;
   TYPE addresses   IS ARRAY(number RANGE <>) OF ALIASED address
   WITH
      Component_Size          => address'size,
      Default_Component_Value => address'first;

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
      index_error,      -- A wrong index was specified.
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

   -- I don't want to use the old ASCII package or drag in the Latin-1 package,
   -- so I've just included the most relevant characters to save time typing.
   NUL : CONSTANT character := character'val(00);
   HT  : CONSTANT character := character'val(09);
   LF  : CONSTANT character := character'val(10);
   CR  : CONSTANT character := character'val(13);

   -- Since there's no secondary stack, the string returned is padded with null
   -- characters which should be ignored by I/O utilities.
   SUBTYPE image_string IS string(1 .. 64);

   -- Imaging function for numeric values. An improvement over the image
   -- attribute is the ability to set a base and that this is also not reliant
   -- on the runtime image functions which the attribute makes a call to. There
   -- is also no secondary stack involved. Note that the "Padding" parameter
   -- indicates a minimum number of (visible) characters in the return string.
   -- TODO: Add binary and integer/negative number imaging.
   FUNCTION Image
     (Value   : IN number;
      Base    : IN number   := 10;
      Padding : IN positive := 01)
      RETURN image_string
   WITH
      Pre => Base IN 10 | 16 AND THEN
             Padding <= image_string'last;

   -- By default, image addresses in base 16.
   FUNCTION Image
     (Value   : IN address;
      Base    : IN number   := 16;
      Padding : IN positive := 01)
      RETURN image_string
   WITH
      Pre => Base IN 10 | 16 AND THEN
             Padding <= image_string'last;

   -- This scans a string and returns the first unsigned numeric value it can
   -- find. It returns the maximum value possible if no ASCII numbers were
   -- found or if the size indicated is simply too large to fit inside 64 bits.
   -- TODO: Support different bases if needed, along with prefixes i.e. "0x",
   -- "0c", "0b", and "-".
   FUNCTION Scan
     (Imaged : IN string)
      RETURN number
   WITH
      Pre => Imaged'length > 0;

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
   SUBTYPE log_string_limit IS positive RANGE 1 .. 1000;

   -- A record of a log containing its specific details.
   TYPE log_information IS RECORD
      -- String description of the log's meaning.
      Information : string(log_string_limit) := (OTHERS => NUL);
      -- The tag of the log.
      Tag         : string(log_tag_limit) := (OTHERS => NUL);
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

   -- A wrapper for `Log()` that does not take in an Ada array.
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
