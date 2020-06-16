-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-uefi.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE BODY HAVK_Kernel.UEFI
IS
   FUNCTION Get_Memory_Attributes
     (Region : NOT NULL ACCESS CONSTANT memory_descriptor)
      RETURN memory_attributes
   IS
   (
      Uncacheable      =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000000001#) = 0
            THEN false),
      Write_Combining  =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000000002#) = 0
            THEN false),
      Write_Through    =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000000004#) = 0
            THEN false),
      Write_Back       =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000000008#) = 0
            THEN false),
      Inexportable     =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000000010#) = 0
            THEN false),
      Write_Protected  =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000001000#) = 0
            THEN false),
      Read_Protected   =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000002000#) = 0
            THEN false),
      Not_Executable   =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000004000#) = 0
            THEN false),
      Persistent       =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000008000#) = 0
            THEN false),
      High_Reliability =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000010000#) = 0
            THEN false),
      Read_Only        =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000020000#) = 0
            THEN false),
      Specific_Purpose =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000040000#) = 0
            THEN false),
      Crypto_Protected =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#0000000000080000#) = 0
            THEN false),
      Runtime_Service  =>
        (IF (Region.Memory_Attribute_Bitmask AND 16#8000000000000000#) = 0
            THEN false)
   );

   FUNCTION Get_Memory_Map
      RETURN memory_map
   IS
      FUNCTION To_Pointer
        (Memory_Map_Descriptor_Address : IN address)
         RETURN access_memory_descriptor
      WITH
         Import     => true,
         Convention => Intrinsic,
         Pre        => Memory_Map_Descriptor_Address /= 0,
         Post       => To_Pointer'result /= NULL;

      Limit  : CONSTANT address := Bootloader_Arguments.Memory_Map_Address +
         address(Bootloader_Arguments.Memory_Map_Size);
      Offset : address
         RANGE Bootloader_Arguments.Memory_Map_Address .. Limit :=
            Bootloader_Arguments.Memory_Map_Address;
      Map    : memory_map(1 .. Bootloader_Arguments.Memory_Map_Size /
         Bootloader_Arguments.Memory_Map_Descriptor_Size);
   BEGIN
      FOR
         Region OF Map
      LOOP
         Region := To_Pointer(Offset);

         IF
            Offset +
               address(Bootloader_Arguments.Memory_Map_Descriptor_Size) < Limit
         THEN
            Offset := Offset +
               address(Bootloader_Arguments.Memory_Map_Descriptor_Size);
         END IF;
      END LOOP;

      -- Can't figure out how to prove this properly (if that's even possible).
      PRAGMA Assume(FOR ALL Region IN Map'range => Map(Region) /= NULL);

      RETURN Map;
   END Get_Memory_Map;

END HAVK_Kernel.UEFI;
