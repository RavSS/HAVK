-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-memory.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI;

PACKAGE BODY HAVK_Kernel.Memory
WITH
   Refined_State => (Memory_State => NULL)
IS
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   IS
      Temporary : number;
   BEGIN
      -- This only works with power-of-two alignments and two's complement.
      -- It is much easier to prove it than with a modulus involved instead.
      IF
         NOT Round_Up
      THEN
         RETURN Value - (Value AND (Alignment - 1));
      END IF;

      IF
         Value < number'last - Alignment -- First of the two overflow checks.
      THEN -- Rounding up uses a modulus to avoid a bit flip (overflow check).
         Temporary := -- It alone isn't enough for the postcondition to hold.
            Value + ((Alignment - (Value MOD Alignment)) MOD Alignment);
         RETURN Temporary - (Temporary AND (Alignment - 1));
      ELSE
         RETURN Value - (Value AND (Alignment - 1)); -- Can't round up.
      END IF;
   END Align;

   FUNCTION System_Limit
      RETURN number
   IS
      USE TYPE
         UEFI.access_memory_descriptor,
         UEFI.memory_type;

      Attributes : UEFI.memory_attributes;
      Limit      : number := 0;
   BEGIN
      FOR
         Region OF UEFI.Memory_Map
      LOOP
         EXIT WHEN Region = NULL;
         Attributes := UEFI.Get_Memory_Attributes(Region);

         IF -- Check if the region is usable.
            Region.Memory_Region_Type = UEFI.conventional_data AND THEN
            NOT Attributes.Read_Only                           AND THEN
            NOT Attributes.Write_Protected                     AND THEN
            NOT Attributes.Read_Protected
         THEN -- Count the conventional memory only. Ignore reserved memory.
            IF -- Overflow check. I'd rather not use an assumption.
               Limit <= number'last - Paging.Page * Region.Number_Of_Pages
            THEN
               Limit := Limit + Paging.Page * Region.Number_Of_Pages;
            END IF;
         END IF;
      END LOOP;

      RETURN Limit;
   END System_Limit;

   FUNCTION Kernel_Virtual_To_Physical
     (Kernel_Virtual_Address : IN address)
      RETURN address
   IS
      Offset : CONSTANT address :=
         Kernel_Virtual_Base - UEFI.Bootloader_Arguments.Physical_Base_Address;
   BEGIN
      RETURN Kernel_Virtual_Address - Offset;
   END Kernel_Virtual_To_Physical;

END HAVK_Kernel.Memory;
