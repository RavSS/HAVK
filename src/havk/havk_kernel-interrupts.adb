WITH
   System.Machine_Code,
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.IRQs,
   HAVK_Kernel.Interrupts.PIC,
   HAVK_Kernel.Paging;
USE
   System.Machine_Code,
   HAVK_Kernel.Interrupts.Exceptions,
   HAVK_Kernel.Interrupts.IRQs;

PACKAGE BODY HAVK_Kernel.Interrupts
WITH
   SPARK_Mode => off -- Address attributes are used outside of address clauses.
IS
   PROCEDURE Prepare_GDT
   IS
      -- Prepare the GDT register's contents.
      GDTR : CONSTANT descriptor_table :=
      (
         Table_Size    => (GDT_entries'size / 8) - 1,
         Start_Address =>  GDT'address
      );

      PROCEDURE LGDT(
         GDTR_Address : IN System.Address)
      WITH -- See the explanation later on.
         Import        =>  true,
         Convention    =>  NASM,
         External_Name => "lgdt";
   BEGIN
      -- Now to reload the segment registers. Sadly, Ada's Machine Code
      -- package does not support C's `__asm__ goto`. I need to do a far jump
      -- for this to work, and trying to store the address of labels in
      -- registers for some reason doesn't work at all and puts junk in them
      -- when I use inline assembly and inspect the registers in e.g. QEMU.
      LGDT(GDTR'address);
   END Prepare_GDT;

   PROCEDURE Setup_Interrupt(
      Index          : IN num;
      ISR            : IN System.Address;
      Exception_Type : IN IDT_gate_type;
      Ring           : IN num)
   IS
      ISR_Address    : CONSTANT num := Address_To_num(ISR) -
         Paging.Kernel_Virtual_Base;
   BEGIN
      IDT(Index)     :=
      (
         ISR_Address_Low               => ISR_Address AND 16#FFFF#,
         CS_Selector                   => 16#8#, -- 0x8 is my DPL 0 CS.
         IST_Offset                    => 0,
         Type_Attributes               =>
         (
            Gate                       => Exception_Type,
            Storage_Segment            => false,
            DPL                        => Ring,
            Present                    => true
         ),
         ISR_Address_Middle            => SHR(ISR_Address, 16),
         ISR_Address_High              => SHR(ISR_Address, 32),
         Zeroed                        => 0
      );
   END Setup_Interrupt;

   PROCEDURE Prepare_IDT
   IS
      IDTR : descriptor_table :=
      (
         Table_Size    => (IDT'size / 8) - 1,
         Start_Address =>  IDT'address
      );
   BEGIN
      -- Remap the interrupt vector so no interrupts overlap etc.
      PIC.Remap;

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
      -- TODO: I don't think all of these should be callable
      -- by anything in Ring 3, but it'll do for now.
      Setup_Interrupt(32, ISR_32_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(33, ISR_33_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(34, ISR_34_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(35, ISR_35_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(36, ISR_36_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(37, ISR_37_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(38, ISR_38_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(39, ISR_39_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(40, ISR_40_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(41, ISR_41_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(42, ISR_42_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(43, ISR_43_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(44, ISR_44_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(45, ISR_45_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(46, ISR_46_Handler'address, interrupt_64_bit, 3);
      Setup_Interrupt(47, ISR_47_Handler'address, interrupt_64_bit, 3);

      Asm(
         -- Must disable interrupts before we can setup and use interrupts.
         "CLI;" &
         -- Load my own IDT into the IDTR.
         "LIDT [%0];",
         Inputs   => System.Address'asm_input("r", IDTR'address),
         Volatile => true);
   END Prepare_IDT;
END HAVK_Kernel.Interrupts;
