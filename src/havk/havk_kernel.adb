PACKAGE BODY HAVK_Kernel IS
   FUNCTION Memory_Copy(
      Destination          : IN System.Address;
      Source               : IN System.Address;
      Copy_Size            : IN num)
   RETURN System.Address     IS
      Destination_Location : ALIASED u8s(0 .. Copy_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Destination;

      Source_Location      : ALIASED u8s(0 .. Copy_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Source;
   BEGIN
      FOR Copy IN Source_Location'range LOOP
         Destination_Location(Copy) := Source_Location(Copy);
      END LOOP;

      RETURN Destination;
   END Memory_Copy;

   FUNCTION Memory_Set(
      Destination          : IN System.Address;
      Set_Value            : IN u8;
      Set_Size             : IN num)
   RETURN System.Address     IS
      Destination_Location : ALIASED u8s(0 .. Set_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Destination;
   BEGIN
      FOR Set IN Destination_Location'range LOOP
         Destination_Location(Set) := Set_Value;
      END LOOP;

      RETURN Destination;
   END Memory_Set;

   FUNCTION Memory_Move(
      Destination          : IN System.Address;
      Source               : IN System.Address;
      Move_Size            : IN num)
   RETURN System.Address     IS
      Destination_Location : ALIASED u8s(0 .. Move_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Destination;

      Source_Location      : ALIASED u8s(0 .. Move_Size)
      WITH
         Import     => true,
         Convention => C,
         Address    => Source;

      -- No overlap optimizations for now, double copies is quite slow.
      Temporary_Buffer     : ALIASED u8s(0 .. Move_Size)
      WITH
         Import     => true,
         Convention => C;
   BEGIN
      FOR Copy IN Source_Location'range LOOP
         Temporary_Buffer(Copy)     := Source_Location(Copy);
      END LOOP;

      FOR Copy IN Temporary_Buffer'range LOOP
         Destination_Location(Copy) := Temporary_Buffer(Copy);
      END LOOP;

      RETURN Destination;
   END Memory_Move;
END HAVK_Kernel;