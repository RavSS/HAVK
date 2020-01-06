-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-memory.adb                                 --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.UEFI,
   HAVK_Kernel.Paging;

PACKAGE BODY HAVK_Kernel.Memory
IS
   -- The formula was taken from here.
   -- https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
   FUNCTION Align
     (Value     : IN number;
      Alignment : IN number;
      Round_Up  : IN boolean := false)
      RETURN number
   IS
   (
      IF
         NOT Round_Up
      THEN
         Value - ((Alignment + (Value MOD Alignment)) MOD Alignment)
      ELSE
         Value + ((Alignment - (Value MOD Alignment)) MOD Alignment)
   );

   FUNCTION System_Limit
      RETURN number
   IS
      USE
         HAVK_Kernel.UEFI;

      Map        : CONSTANT memory_map := Get_Memory_Map;
      Attributes : memory_attributes;
   BEGIN
      RETURN
         Limit   : number := 0
      DO
         FOR
            Region OF Map
         LOOP
            Attributes := Get_Memory_Attributes(Region);

            IF -- Check if the region is useable.
               Region.Memory_Region_Type = conventional_data AND THEN
               NOT Attributes.Read_Only                      AND THEN
               NOT Attributes.Write_Protected                AND THEN
               NOT Attributes.Read_Protected
            THEN
               -- Count the conventional memory only. Ignore reserved memory.
               Limit := Limit + Paging.Page * Region.Number_Of_Pages;
            END IF;
         END LOOP;
      END RETURN;
   END System_Limit;

END HAVK_Kernel.Memory;
