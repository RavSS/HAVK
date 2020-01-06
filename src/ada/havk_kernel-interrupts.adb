-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts.adb                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.IRQs;
USE
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.IRQs;

PACKAGE BODY HAVK_Kernel.Interrupts
WITH
   SPARK_Mode => off -- Address attributes are used outside of address clauses.
IS
   PROCEDURE Prepare_GDT
   IS
      -- Prepare the GDT register's contents.
      GDTR : ALIASED CONSTANT descriptor_table :=
      (
         Table_Size    => (GDT_entries'size / 8) - 1,
         Start_Address =>  GDT'address
      );

      -- Need to do a far-jump to set the CS after we've changed the GDTR,
      -- which this procedure does. Note that Ada's machine code package
      -- doesn't support the `__asm__ goto` feature which is found in C, so
      -- this is yet another reason to use external assembly. This also loads
      -- the task register while it's at it.
      PROCEDURE LGDT
        (GDTR_Address : IN address;
         CS_Offset    : IN number;
         DS_Offset    : IN number;
         TSS_Offset   : IN number)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_global_descriptor_table",
         Pre           => CS_Offset  <= 16#FFFF# AND THEN
                          DS_Offset  <= 16#FFFF# AND THEN
                          TSS_Offset <= 16#FFFF#;
   BEGIN
      -- See the "GDT_entries" record type on how the offsets are determined.
      LGDT(GDTR'address, 16#08#, 16#10#, 16#28#);
   END Prepare_GDT;

   PROCEDURE Setup_Interrupt
     (Index          : IN number;
      ISR            : IN address;
      Exception_Type : IN IDT_gate_type;
      Ring           : IN number)
   IS
      ISR_Address    : CONSTANT number := Address_Value(ISR);
   BEGIN
      IDT(Index)     :=
      (
         ISR_Address_Low    => ISR_Address AND 16#FFFF#,
         CS_Selector        => 16#8#, -- 0x8 is my DPL 0 CS.
         IST_Offset         => 0,
         Type_Attributes    =>
         (
            Gate            => Exception_Type,
            Storage_Segment => false,
            DPL             => Ring,
            Present         => true
         ),
         ISR_Address_Middle => Shift_Right(ISR_Address, 16) AND 16#FFFF#,
         ISR_Address_High   => Shift_Right(ISR_Address, 32),
         Zeroed             => 0
      );
   END Setup_Interrupt;

   PROCEDURE Prepare_IDT
   IS
      IDTR : ALIASED CONSTANT descriptor_table :=
      (
         Table_Size    => (IDT'size / 8) - 1,
         Start_Address =>  IDT'address
      );

      PROCEDURE LGDT
        (IDTR_Address : IN address)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_interrupt_descriptor_table";
   BEGIN
      -- First setup the CPU exceptions.
      Setup_Interrupt( 0,  ISR_0_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 1,  ISR_1_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 2,  ISR_2_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 3,  ISR_3_Handler'address,      trap_64_bit, 0);
      Setup_Interrupt( 4,  ISR_4_Handler'address,      trap_64_bit, 0);
      Setup_Interrupt( 5,  ISR_5_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 6,  ISR_6_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 7,  ISR_7_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 8,  ISR_8_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt( 9,  ISR_9_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(10, ISR_10_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(11, ISR_11_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(12, ISR_12_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(13, ISR_13_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(14, ISR_14_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(15, ISR_15_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(16, ISR_16_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(17, ISR_17_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(18, ISR_18_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(19, ISR_19_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(20, ISR_20_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(21, ISR_21_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(22, ISR_22_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(23, ISR_23_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(24, ISR_24_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(25, ISR_25_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(26, ISR_26_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(27, ISR_27_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(28, ISR_28_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(29, ISR_29_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(30, ISR_30_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(31, ISR_31_Handler'address, interrupt_64_bit, 0);

      -- Now the IRQs.
      Setup_Interrupt(32, ISR_32_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(33, ISR_33_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(34, ISR_34_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(35, ISR_35_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(36, ISR_36_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(37, ISR_37_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(38, ISR_38_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(39, ISR_39_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(40, ISR_40_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(41, ISR_41_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(42, ISR_42_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(43, ISR_43_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(44, ISR_44_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(45, ISR_45_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(46, ISR_46_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(47, ISR_47_Handler'address, interrupt_64_bit, 0);

      LGDT(IDTR'address);
   END Prepare_IDT;
END HAVK_Kernel.Interrupts;
