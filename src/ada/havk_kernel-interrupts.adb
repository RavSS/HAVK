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
         Pre           =>  CS_Offset <= 16#FFFF# AND THEN
                           DS_Offset <= 16#FFFF# AND THEN
                          TSS_Offset <= 16#FFFF#;
   BEGIN
      -- See the "GDT_entries" record type on how the offsets are determined.
      LGDT(GDTR'address, 16#08#, 16#10#, 16#30#);
   END Prepare_GDT;

   PROCEDURE Setup_Interrupt
     (Index          : IN number;
      ISR            : IN address;
      Exception_Type : IN IDT_gate_type;
      Callable_DPL   : IN number)
   IS
      ISR_Address    : CONSTANT number := Address_Value(ISR);
   BEGIN
      IDT(Index)     :=
      (
         ISR_Address_Low    => ISR_Address AND 16#FFFF#,
         CS_Selector        => (IF Callable_DPL = 0 THEN 16#08# ELSE 16#18#),
         IST_Offset         => 0,
         Type_Attributes    =>
         (
            Gate            => Exception_Type,
            Storage_Segment => false,
            DPL             => Callable_DPL,
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

      PROCEDURE Switch_Task
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__switch_task";

      PROCEDURE LGDT
        (IDTR_Address : IN address)
      WITH
         Import        => true,
         Convention    => Assembler,
         External_Name => "assembly__load_interrupt_descriptor_table";
   BEGIN
      -- First setup the CPU exceptions.
      Setup_Interrupt(000,  ISR_0_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(001,  ISR_1_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(002,  ISR_2_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(003,  ISR_3_Handler'address,      trap_64_bit, 0);
      Setup_Interrupt(004,  ISR_4_Handler'address,      trap_64_bit, 0);
      Setup_Interrupt(005,  ISR_5_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(006,  ISR_6_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(007,  ISR_7_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(008,  ISR_8_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(009,  ISR_9_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(010, ISR_10_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(011, ISR_11_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(012, ISR_12_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(013, ISR_13_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(014, ISR_14_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(015, ISR_15_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(016, ISR_16_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(017, ISR_17_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(018, ISR_18_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(019, ISR_19_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(020, ISR_20_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(021, ISR_21_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(022, ISR_22_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(023, ISR_23_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(024, ISR_24_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(025, ISR_25_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(026, ISR_26_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(027, ISR_27_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(028, ISR_28_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(029, ISR_29_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(030, ISR_30_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(031, ISR_31_Handler'address, interrupt_64_bit, 0);

      -- Now the IRQs.
      Setup_Interrupt(032, ISR_32_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(033, ISR_33_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(034, ISR_34_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(035, ISR_35_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(036, ISR_36_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(037, ISR_37_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(038, ISR_38_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(039, ISR_39_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(040, ISR_40_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(041, ISR_41_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(042, ISR_42_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(043, ISR_43_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(044, ISR_44_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(045, ISR_45_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(046, ISR_46_Handler'address, interrupt_64_bit, 0);
      Setup_Interrupt(047, ISR_47_Handler'address, interrupt_64_bit, 0);

      -- Finally, map any other interrupts for scheduling etc.
      Setup_Interrupt(100,    Switch_Task'address, interrupt_64_bit, 0);

      LGDT(IDTR'address);
   END Prepare_IDT;
END HAVK_Kernel.Interrupts;
