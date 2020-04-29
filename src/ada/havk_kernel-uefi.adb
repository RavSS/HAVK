-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
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

   FUNCTION Get_Arguments
      RETURN arguments
   IS
      -- The bootloader will never pass a null pointer, so this type is just
      -- here to inform `gnatprove` of it without an explicit pragma.
      TYPE access_arguments  IS NOT NULL ACCESS arguments;
      Bootloader_Arguments : CONSTANT access_arguments
      WITH
         Import     => true,
         Convention => Assembler,
         Link_Name  => "__bootloader_arguments";
   BEGIN
      -- It's not physically possible for a scanline to have less pixels than
      -- the scanline's width, so the UEFI GOP implementation will never return
      -- anything to the contrary.
      PRAGMA Assume(Bootloader_Arguments.Pixels_Per_Scanline >=
         Bootloader_Arguments.Horizontal_Resolution);

      RETURN Bootloader_Arguments.ALL;
   END Get_Arguments;

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

      Bootloader : CONSTANT arguments := Get_Arguments;
      Limit      : CONSTANT address := Bootloader.Memory_Map_Address +
         address(Bootloader.Memory_Map_Size);
      Offset     : address RANGE Bootloader.Memory_Map_Address .. Limit :=
         Bootloader.Memory_Map_Address;
      Map        : memory_map(1 .. Bootloader.Memory_Map_Size /
         Bootloader.Memory_Map_Descriptor_Size);
   BEGIN
      FOR
         Region OF Map
      LOOP
         Region := To_Pointer(Offset);

         IF
            Offset + address(Bootloader.Memory_Map_Descriptor_Size) < Limit
         THEN
            Offset := Offset + address(Bootloader.Memory_Map_Descriptor_Size);
         END IF;
      END LOOP;

      -- Can't figure out how to prove this properly (if that's even possible).
      PRAGMA Assume(FOR ALL Region IN Map'range => Map(Region) /= NULL);

      RETURN Map;
   END Get_Memory_Map;

END HAVK_Kernel.UEFI;
