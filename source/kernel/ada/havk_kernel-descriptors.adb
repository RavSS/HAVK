-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-descriptors.adb                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Interrupts,
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.ISA_IRQs;

PACKAGE BODY HAVK_Kernel.Descriptors
WITH
   Refined_State => (Descriptors_State => (GDT, GDTR, IDTR))
IS
   PROCEDURE Reset_Interrupt_Descriptor_Table
   WITH
      SPARK_Mode => off -- Need the addresses of the ISR stubs directly.
   IS
      USE
         HAVK_Kernel.Interrupts,
         HAVK_Kernel.Interrupts.Exceptions,
         HAVK_Kernel.Interrupts.ISA_IRQs;
   BEGIN
      IDT :=
      (  -- CPU exceptions.
         000 => Interrupt_Entry(ISR_000_Stub'address),
         001 => Interrupt_Entry(ISR_001_Stub'address),
         002 => Interrupt_Entry(ISR_002_Stub'address),
         003 => Interrupt_Entry(ISR_003_Stub'address, Gate => trap_gate),
         004 => Interrupt_Entry(ISR_004_Stub'address, Gate => trap_gate),
         005 => Interrupt_Entry(ISR_005_Stub'address),
         006 => Interrupt_Entry(ISR_006_Stub'address),
         007 => Interrupt_Entry(ISR_007_Stub'address),
         008 => Interrupt_Entry(ISR_008_Stub'address),
         009 => Interrupt_Entry(ISR_009_Stub'address),
         010 => Interrupt_Entry(ISR_010_Stub'address),
         011 => Interrupt_Entry(ISR_011_Stub'address),
         012 => Interrupt_Entry(ISR_012_Stub'address),
         013 => Interrupt_Entry(ISR_013_Stub'address),
         014 => Interrupt_Entry(ISR_014_Stub'address),
         015 => Interrupt_Entry(ISR_015_Stub'address),
         016 => Interrupt_Entry(ISR_016_Stub'address),
         017 => Interrupt_Entry(ISR_017_Stub'address),
         018 => Interrupt_Entry(ISR_018_Stub'address),
         019 => Interrupt_Entry(ISR_019_Stub'address),
         020 => Interrupt_Entry(ISR_020_Stub'address),
         021 => Interrupt_Entry(ISR_021_Stub'address),
         022 => Interrupt_Entry(ISR_022_Stub'address),
         023 => Interrupt_Entry(ISR_023_Stub'address),
         024 => Interrupt_Entry(ISR_024_Stub'address),
         025 => Interrupt_Entry(ISR_025_Stub'address),
         026 => Interrupt_Entry(ISR_026_Stub'address),
         027 => Interrupt_Entry(ISR_027_Stub'address),
         028 => Interrupt_Entry(ISR_028_Stub'address),
         029 => Interrupt_Entry(ISR_029_Stub'address),
         030 => Interrupt_Entry(ISR_030_Stub'address),
         031 => Interrupt_Entry(ISR_031_Stub'address),

         -- Legacy ISA IRQs.
         032 => Interrupt_Entry(ISR_032_Stub'address), -- IRQ 00.
         033 => Interrupt_Entry(ISR_033_Stub'address), -- IRQ 01.
         034 => Interrupt_Entry(ISR_034_Stub'address), -- IRQ 02.
         035 => Interrupt_Entry(ISR_035_Stub'address), -- IRQ 03.
         036 => Interrupt_Entry(ISR_036_Stub'address), -- IRQ 04.
         037 => Interrupt_Entry(ISR_037_Stub'address), -- IRQ 05.
         038 => Interrupt_Entry(ISR_038_Stub'address), -- IRQ 06.
         039 => Interrupt_Entry(ISR_039_Stub'address), -- IRQ 07.
         040 => Interrupt_Entry(ISR_040_Stub'address), -- IRQ 08.
         041 => Interrupt_Entry(ISR_041_Stub'address), -- IRQ 09.
         042 => Interrupt_Entry(ISR_042_Stub'address), -- IRQ 10.
         043 => Interrupt_Entry(ISR_043_Stub'address), -- IRQ 11.
         044 => Interrupt_Entry(ISR_044_Stub'address), -- IRQ 12.
         045 => Interrupt_Entry(ISR_045_Stub'address), -- IRQ 13.
         046 => Interrupt_Entry(ISR_046_Stub'address), -- IRQ 14.
         047 => Interrupt_Entry(ISR_047_Stub'address), -- IRQ 15.

         -- HAVK-specific interrupt vectors.
         048 => Interrupt_Entry(ISR_048_Stub'address), -- LAPIC (tasking).

         -- Cover the disabled PIC and the APIC (spurious) interrupt vectors.
         -- It is okay if both overlap.
         OTHERS => Interrupt_Entry(ISR_Default'address)
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
         TSS.RSP_Ring_0    := Entry_Stack_Address;
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
      Ring_3    : IN boolean := false;
      IST       : IN number  := 0)
      RETURN interrupt_descriptor_table_gate
   IS
   (
      ISR_Address_Low    => ISR_Entry AND 2**16 - 1,
      CS_Selector        => (IF Ring_3 THEN 16#28# ELSE CS_Ring_0),
      IST_Offset         => IST,
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
