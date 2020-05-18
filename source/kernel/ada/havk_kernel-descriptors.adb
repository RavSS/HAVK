-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-descriptors.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.ISA_IRQs;

PACKAGE BODY HAVK_Kernel.Descriptors
WITH
   Refined_State => (Descriptors_State => (GDT, GDTR, IDTR))
IS
   PROCEDURE Reset_Interrupt_Descriptor_Table
   WITH
      SPARK_Mode => off -- Need the addresses of the ISRs directly.
   IS
      USE
         HAVK_Kernel.Interrupts,
         HAVK_Kernel.Interrupts.Exceptions,
         HAVK_Kernel.Interrupts.ISA_IRQs;
   BEGIN
      IDT :=
      (  -- CPU exceptions.
         000 => Interrupt_Entry(ISR_000_Handler'address),
         001 => Interrupt_Entry(ISR_001_Handler'address),
         002 => Interrupt_Entry(ISR_002_Handler'address),
         003 => Interrupt_Entry(ISR_003_Handler'address, Gate => trap_gate),
         004 => Interrupt_Entry(ISR_004_Handler'address, Gate => trap_gate),
         005 => Interrupt_Entry(ISR_005_Handler'address),
         006 => Interrupt_Entry(ISR_006_Handler'address),
         007 => Interrupt_Entry(ISR_007_Handler'address),
         008 => Interrupt_Entry(ISR_008_Handler'address),
         009 => Interrupt_Entry(ISR_009_Handler'address),
         010 => Interrupt_Entry(ISR_010_Handler'address),
         011 => Interrupt_Entry(ISR_011_Handler'address),
         012 => Interrupt_Entry(ISR_012_Handler'address),
         013 => Interrupt_Entry(ISR_013_Handler'address),
         014 => Interrupt_Entry(ISR_014_Handler'address),
         015 => Interrupt_Entry(ISR_015_Handler'address),
         016 => Interrupt_Entry(ISR_016_Handler'address),
         017 => Interrupt_Entry(ISR_017_Handler'address),
         018 => Interrupt_Entry(ISR_018_Handler'address),
         019 => Interrupt_Entry(ISR_019_Handler'address),
         020 => Interrupt_Entry(ISR_020_Handler'address),
         021 => Interrupt_Entry(ISR_021_Handler'address),
         022 => Interrupt_Entry(ISR_022_Handler'address),
         023 => Interrupt_Entry(ISR_023_Handler'address),
         024 => Interrupt_Entry(ISR_024_Handler'address),
         025 => Interrupt_Entry(ISR_025_Handler'address),
         026 => Interrupt_Entry(ISR_026_Handler'address),
         027 => Interrupt_Entry(ISR_027_Handler'address),
         028 => Interrupt_Entry(ISR_028_Handler'address),
         029 => Interrupt_Entry(ISR_029_Handler'address),
         030 => Interrupt_Entry(ISR_030_Handler'address),
         031 => Interrupt_Entry(ISR_031_Handler'address),

         -- Legacy ISA IRQs.
         032 => Interrupt_Entry(ISR_032_Handler'address), -- IRQ 00.
         033 => Interrupt_Entry(ISR_033_Handler'address), -- IRQ 01.
         034 => Interrupt_Entry(ISR_034_Handler'address), -- IRQ 02.
         035 => Interrupt_Entry(ISR_035_Handler'address), -- IRQ 03.
         036 => Interrupt_Entry(ISR_036_Handler'address), -- IRQ 04.
         037 => Interrupt_Entry(ISR_037_Handler'address), -- IRQ 05.
         038 => Interrupt_Entry(ISR_038_Handler'address), -- IRQ 06.
         039 => Interrupt_Entry(ISR_039_Handler'address), -- IRQ 07.
         040 => Interrupt_Entry(ISR_040_Handler'address), -- IRQ 08.
         041 => Interrupt_Entry(ISR_041_Handler'address), -- IRQ 09.
         042 => Interrupt_Entry(ISR_042_Handler'address), -- IRQ 10.
         043 => Interrupt_Entry(ISR_043_Handler'address), -- IRQ 11.
         044 => Interrupt_Entry(ISR_044_Handler'address), -- IRQ 12.
         045 => Interrupt_Entry(ISR_045_Handler'address), -- IRQ 13.
         046 => Interrupt_Entry(ISR_046_Handler'address), -- IRQ 14.
         047 => Interrupt_Entry(ISR_047_Handler'address), -- IRQ 15.

         -- HAVK-specific interrupt vectors.
         048 => Interrupt_Entry(ISR_048_Handler'address), -- LAPIC.
         100 => Interrupt_Entry(ISR_100_Handler'address), -- Task switching.

         -- Cover the disabled PIC and the APIC (spurious) interrupt vectors.
         -- It is okay if both overlap.
         OTHERS => Interrupt_Entry(Spurious_Interrupt_Handler'address)
      );
   END Reset_Interrupt_Descriptor_Table;

   PROCEDURE Load
   WITH
      SPARK_Mode => off -- Address attributes are needed to continue.
   IS
      -- Need to do a far-jump to set the CS after we've changed the GDTR,
      -- which this procedure does. Note that Ada's machine code package
      -- doesn't support the `__asm__ goto` feature which is found in C, so
      -- this is yet another reason to use external assembly. This also loads
      -- the task register while it's at it. See the "global_descriptor_table"
      -- record type on how the offsets are determined.
      PROCEDURE Load_Global_Descriptor_Table
        (GDTR_Address : IN address := GDTR'address;
         CS_Offset    : IN number  := CS_Ring_0;
         DS_Offset    : IN number  := DS_Ring_0;
         TSS_Offset   : IN number  := 16#30#)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_global_descriptor_table",
         Pre           =>  CS_Offset <= 2**16 - 1 AND THEN
                           DS_Offset <= 2**16 - 1 AND THEN
                          TSS_Offset <= 2**16 - 1;

      PROCEDURE Load_Interrupt_Descriptor_Table
        (IDTR_Address : IN address := IDTR'address)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_interrupt_descriptor_table";

      PROCEDURE Set_Addresses
      WITH
         Inline => true;

      PROCEDURE Set_Addresses
      IS
      BEGIN
         GDT.Descriptor_TSS64.Descriptor_TSS.Base_Address_Low :=
            TSS'address AND 2**16 - 1;
         GDT.Descriptor_TSS64.Descriptor_TSS.Base_Address_Middle :=
            Shift_Right(TSS'address, 16) AND 2**8 - 1;
         GDT.Descriptor_TSS64.Descriptor_TSS.Base_Address_High :=
            Shift_Right(TSS'address, 24) AND 2**8 - 1;

         GDT.Descriptor_TSS64.Base_Address_Extended :=
            Shift_Right(TSS'address, 32) AND 2**32 - 1;

         GDTR.Base_Address := GDT'address;
         IDTR.Base_Address := IDT'address;
      END Set_Addresses;
   BEGIN
      Intrinsics.Disable_Interrupts;

      Set_Addresses;
      Reset_Interrupt_Descriptor_Table;

      Load_Global_Descriptor_Table;
      Load_Interrupt_Descriptor_Table;

      Log("Descriptor tables prepared.", Tag => Descriptors_Tag);
   END Load;

   FUNCTION Interrupt_Entry
     (ISR_Entry : IN address;
      Gate      : IN interrupt_descriptor_table_gate_type := interrupt_gate;
      Ring_3    : IN boolean                              := false)
      RETURN interrupt_descriptor_table_gate
   IS
   (
      ISR_Address_Low    => ISR_Entry AND 2**16 - 1,
      CS_Selector        => (IF Ring_3 THEN 16#28# ELSE CS_Ring_0),
      IST_Offset         => 0,
      Type_Attributes    =>
      (
         Gate            => Gate,
         Storage_Segment => false,
         DPL             => (IF Ring_3 THEN 3 ELSE 0),
         Present         => true
      ),
      ISR_Address_Middle => Shift_Right(ISR_Entry, 16) AND 2**16 - 1,
      ISR_Address_High   => Shift_Right(ISR_Entry, 32) AND 2**32 - 1,
      Zeroed             => 0
   );

END HAVK_Kernel.Descriptors;
