-------------------------------------------------------------------------------
-- Program         -- HAVK Operating System                                  --
-- Filename        -- havk_operating_system-c-string.adb                     --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Operating_System.C.String
IS
   FUNCTION Memory_Copy -- `memcpy()`.
     (Destination : IN void_pointer;
      Source      : IN void_pointer;
      Byte_Size   : IN size_t)
      RETURN void_pointer
   IS
      Source_Array      : CONSTANT char_array(0 .. Byte_Size - 1)
      WITH
         Import  => true,
         Address => Source.ALL'address;

      Destination_Array : char_array(0 .. Byte_Size - 1)
      WITH
         Import  => true,
         Address => Destination.ALL'address;
   BEGIN
      Destination_Array := Source_Array;
      RETURN Destination;
   END Memory_Copy;

   FUNCTION Memory_Set -- `memset()`.
     (Destination : IN void_pointer;
      Value       : IN int;
      Byte_Size   : IN size_t)
      RETURN void_pointer
   IS
      Destination_Array : char_array(0 .. Byte_Size - 1)
      WITH
         Import  => true,
         Address => Destination.ALL'address;
   BEGIN
      FOR
         Element OF Destination_Array
      LOOP
         Element := char(Value);
      END LOOP;

      RETURN Destination;
   END Memory_Set;

   FUNCTION String_Length -- `strlen()`.
     (Passed_String : ALIASED IN char_array)
      RETURN size_t
   IS
   BEGIN
      FOR
         Index IN size_t'range
      LOOP
         IF
            Passed_String(Index) = 0
         THEN
            RETURN Index;
         END IF;
      END LOOP;

      RETURN size_t'last;
   END String_Length;

   FUNCTION String_Length -- `strnlen()`.
     (Passed_String : ALIASED IN char_array;
      Length_Limit  : IN size_t)
      RETURN size_t
   IS
   BEGIN
      FOR
         Index IN size_t RANGE 0 .. Length_Limit - 1
      LOOP
         IF
            Passed_String(Index) = 0
         THEN
            RETURN Index;
         END IF;
      END LOOP;

      RETURN Length_Limit;
   END String_Length;

   FUNCTION String_Compare -- `strcmp()`.
     (String_1 : ALIASED IN char_array;
      String_2 : ALIASED IN char_array)
      RETURN int
   IS
   BEGIN
      FOR
         Index IN size_t'range
      LOOP
         IF
            String_1(Index) = 0 OR ELSE
            String_2(Index) = 0 OR ELSE
            String_1(Index) /= String_2(Index)
         THEN
            RETURN int(String_1(Index)) - int(String_2(Index));
         END IF;
      END LOOP;

      RETURN 0;
   END String_Compare;

   FUNCTION String_Compare -- `strncmp()`.
     (String_1     : ALIASED IN char_array;
      String_2     : ALIASED IN char_array;
      Length_Limit : IN size_t)
      RETURN int
   IS
   BEGIN
      FOR
         Index IN size_t RANGE 0 .. Length_Limit - 1
      LOOP
         IF
            String_1(Index) = 0 OR ELSE
            String_2(Index) = 0 OR ELSE
            String_1(Index) /= String_2(Index)
         THEN
            RETURN int(String_1(Index)) - int(String_2(Index));
         END IF;
      END LOOP;

      RETURN 0;
   END String_Compare;

   FUNCTION String_Copy -- `strcpy()`.
     (Destination : ALIASED OUT char_array;
      Source      : ALIASED IN char_array)
      RETURN char_pointer
   IS
   BEGIN
      FOR
         Index IN size_t'range
      LOOP
         EXIT WHEN Source(Index) = 0;
         Destination(Index) := Source(Index);
      END LOOP;

      RETURN Destination(Destination'first)'unchecked_access;
   END String_Copy;

   FUNCTION String_Copy -- `strncpy()`.
     (Destination  : ALIASED OUT char_array;
      Source       : ALIASED IN char_array;
      Length_Limit : IN size_t)
      RETURN char_pointer
   IS
   BEGIN
      FOR
         Index IN size_t RANGE 0 .. Length_Limit - 1
      LOOP
         IF
            Source(Index) /= 0
         THEN
            Destination(Index) := Source(Index);
         ELSE
            Destination(Index .. Length_Limit - 1) := (OTHERS => 0);
            EXIT WHEN true;
         END IF;
      END LOOP;

      RETURN Destination(Destination'first)'unchecked_access;
   END String_Copy;

END HAVK_Operating_System.C.String;
