-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-string.ads                     --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

-- An Ada implementation of the C standard library's "string.h".
PACKAGE HAVK_Operating_System.C.String
WITH
   Preelaborate => true
IS
   FUNCTION Memory_Copy
     (Destination : IN void_pointer;
      Source      : IN void_pointer;
      Byte_Size   : IN size_t)
      RETURN void_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "memcpy";

   FUNCTION Memory_Set
     (Destination : IN void_pointer;
      Value       : IN int;
      Byte_Size   : IN size_t)
      RETURN void_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "memset",
      Post          => Memory_Set'result = Destination;

   FUNCTION String_Length
     (Passed_String : ALIASED IN char_array)
      RETURN size_t
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strlen";

   FUNCTION String_Length
     (Passed_String : ALIASED IN char_array;
      Length_Limit  : IN size_t)
      RETURN size_t
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strnlen",
      Post          => String_Length'result <= Length_Limit;

   FUNCTION String_Compare
     (String_1 : ALIASED IN char_array;
      String_2 : ALIASED IN char_array)
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strcmp";

   FUNCTION String_Compare
     (String_1     : ALIASED IN char_array;
      String_2     : ALIASED IN char_array;
      Length_Limit : IN size_t)
      RETURN int
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strncmp";

   FUNCTION String_Copy
     (Destination : ALIASED OUT char_array;
      Source      : ALIASED IN char_array)
      RETURN char_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strcpy";

   FUNCTION String_Copy
     (Destination  : ALIASED OUT char_array;
      Source       : ALIASED IN char_array;
      Length_Limit : IN size_t)
      RETURN char_pointer
   WITH
      Export        => true,
      Convention    => C,
      External_Name => "strncpy";

END HAVK_Operating_System.C.String;
