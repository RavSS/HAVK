-- This is the main package spec for the kernel itself, it just contains
-- some types for usage everywhere else, along with some shortcuts.
WITH
   System,
   Ada.Unchecked_Conversion;

PACKAGE HAVK_Kernel
IS
   -- Because HAVK is a 64-bit kernel, I'll make the "default" types 64-bit.

   -- Natural number, assuming you include zero. This is to be used everywhere,
   -- including for memory addresses that you do not expect to overlay or
   -- import into variables/objects.
   TYPE num  IS MOD 2 ** 64;
   TYPE nums IS ARRAY(num RANGE <>) OF num;
   FOR  num'size USE 64;
   PRAGMA Provide_Shift_Operators(num); -- GNAT shift intrinsics are provided.

   -- Simple 64-bit signed integer. Prefer using unsigned types instead.
   TYPE int  IS RANGE -(2 ** 63) .. +(2 ** 63 - 1);
   TYPE ints IS ARRAY(num RANGE <>) OF int;
   FOR  int'size USE 64;
   PRAGMA Provide_Shift_Operators(int);

   -- This is for converting "System.Address" to "num."
   FUNCTION Address_To_num IS NEW Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => num);

   -- A type that describes certain levels of urgency. This is mainly used
   -- for logging purposes as of now.
   TYPE urgency IS(
      trivial, -- A log that does not really matter at all in the long run.
      nominal, -- Of little urgency. Should be used for meaningful logs.
      warning, -- Self-explanatory. All warnings should be issued with this.
      fatal);  -- The most serious level. Only use for actual errors.

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

   -- Stores a log in the main kernel log collection variable.
   -- Default log priority is "trivial".
   PROCEDURE Log(
      Information : IN string;
      Priority    : IN urgency := trivial)
   WITH
      Global => (In_Out => Logs),
      Pre    => Information'last <= Logs.Log_List(1).Information'last;

END HAVK_Kernel;
