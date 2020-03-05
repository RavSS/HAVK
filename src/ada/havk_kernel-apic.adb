-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-apic.adb                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion;

PACKAGE BODY HAVK_Kernel.APIC
WITH
   Refined_State => (Interrupt_Controller_State => (IO_APICs, IRQ_Remaps))
IS
   PROCEDURE Remap_PICs
     (Disable_Emulation : boolean := false)
   IS
      Master_Command    : CONSTANT number := 16#20#;
      Master_Data       : CONSTANT number := 16#21#;
      Slave_Command     : CONSTANT number := 16#A0#;
      Slave_Data        : CONSTANT number := 16#A1#;
   BEGIN
      -- Initialise the PICs.
      Intrinsics.Output_Byte(Master_Command, 16#11#);
      Intrinsics.Output_Byte(Slave_Command,  16#11#);

      -- Start the IRQ vector where IRQ 0 begins at according to us.
      -- The reasoning for this is because IRQ 0 to IRQ 7 are clashed with
      -- the CPU exception interrupts (8 to 15). The slave PIC does not need
      -- to be remapped, but it should for consistency. If we're disabling
      -- PIC emulation, then we move the vector far away in case of any
      -- spurious interrupts that may be raised.
      IF
         Disable_Emulation
      THEN -- SI vector range = 247 to 254. 255 is the LAPIC's SI vector.
         -- TODO: Do these need empty handlers to be installed into the IDT?
         Intrinsics.Output_Byte(Master_Data, 255 - 16);
         Intrinsics.Output_Byte(Slave_Data,  255 - 08);
      ELSE -- Classic ISA IRQ x86 range.
         Intrinsics.Output_Byte(Master_Data, 32);
         Intrinsics.Output_Byte(Slave_Data,  40);
      END IF;

      -- Need to cascade interrupts.
      --     0 - 1 - 2 - 3 - 4 - 5 - 6 - 7
      --         |
      -- 0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8
      -- IRQ 2 cascades to IRQ 1 (second PIC IRQ 1 becomes IRQ 8).
      -- New layout and priority:
      -- 0 > 1 > 8 > 9 > 10 > 11 > 12 > 13 > 14 > 15 > 3 > 4 > 5 > 6 > 7

      -- Inform master PIC that there's another (slave) PIC at IRQ 2.
      -- (1 << [IRQ] 2) = [Binary Form] 100 = [Hex/Dec Form] 4.
      Intrinsics.Output_Byte(Master_Data, 4);

      -- Notify the slave PIC that interrupts will cascade
      -- from the master PIC's IRQ 2.
      -- (1 << [IRQ] 1) = [Binary Form] 10  = [Hex/Dec Form] 2.
      Intrinsics.Output_Byte(Slave_Data,  2);

      -- Put PICs into 8086 mode.
      Intrinsics.Output_Byte(Master_Data, 1);
      Intrinsics.Output_Byte(Slave_Data,  1);

      -- Disable the PICs by masking every interrupt vector if we've disabled
      -- emulation or clear all the masks to receive all PIC interrupts.
      IF
         Disable_Emulation
      THEN -- Mask them all.
         Intrinsics.Output_Byte(Master_Data, 16#FF#);
         Intrinsics.Output_Byte(Slave_Data,  16#FF#);
      ELSE -- Clear all masks.
         Intrinsics.Output_Byte(Master_Data, 0);
         Intrinsics.Output_Byte(Slave_Data,  0);
      END IF;
   END Remap_PICs;

   FUNCTION Read_LAPIC
     (MSR : IN LAPIC_MSR)
      RETURN generic_format
   IS
      FUNCTION MSR_Format IS NEW Ada.Unchecked_Conversion
        (Source => number, Target => generic_format);

      -- This is temporary until GNAT CE 2020 comes around. GNAT CE 2019's
      -- bundled `gnatprove` does not support the "enum_rep" attribute.
      -- It missed the commit for the feature by a few days.
      FUNCTION MSR_Value IS NEW Ada.Unchecked_Conversion
        (Source => LAPIC_MSR, Target => Intrinsics.model_specific_register);

      Register_Value : number;
   BEGIN
      RETURN
         Register_Data : generic_format
      DO
         Intrinsics.Memory_Fence; -- See `Write_LAPIC()` for more.
         Register_Value := Intrinsics.Read_MSR(MSR_Value(MSR));
         Register_Data  := MSR_Format(Register_Value);
         Intrinsics.Memory_Fence;
      END RETURN;
   END Read_LAPIC;

   PROCEDURE Write_LAPIC
     (MSR   : IN LAPIC_MSR;
      Value : IN generic_format)
   IS
      FUNCTION MSR_Value_Number IS NEW Ada.Unchecked_Conversion
        (Source => generic_format, Target => number);

      FUNCTION LAPIC_MSR_Value IS NEW Ada.Unchecked_Conversion
        (Source => LAPIC_MSR, Target => Intrinsics.model_specific_register);
   BEGIN
      -- Intel recommends `MFENCE` then `LFENCE` for the below barrier, but I
      -- don't see how the second instruction does anything the first doesn't.
      -- I'll just slap memory fences everywhere.
      Intrinsics.Memory_Fence; -- `WRMSR` etc. to APIC MSRs is not serialising.
      Intrinsics.Write_MSR(LAPIC_MSR_Value(MSR), MSR_Value_Number(Value));
      Intrinsics.Memory_Fence;
   END Write_LAPIC;

   FUNCTION Read_IO_APIC
     (Mapped_IO_APIC : IN IO_APIC)
     RETURN generic_register
   IS
      SUBTYPE GSI_value IS number RANGE 0 .. 16#FFFFFFFF#; -- Silence warnings.
      FUNCTION Return_Register IS NEW Ada.Unchecked_Conversion
        (Source => GSI_value, Target => generic_register);
   BEGIN
      RETURN
         Register_Data : generic_register
      DO
         Intrinsics.Memory_Fence;
         Register_Data := Return_Register(Mapped_IO_APIC.MMIO.Register_Window);
         Intrinsics.Memory_Fence;
      END RETURN;
   END Read_IO_APIC;

   PROCEDURE Write_IO_APIC
     (Mapped_IO_APIC : IN OUT IO_APIC;
      Register_Data  : IN generic_register)
   IS
      SUBTYPE IOREGWIN IS number RANGE 0 .. 16#FFFFFFFF#;
      FUNCTION Value_Number IS NEW Ada.Unchecked_Conversion
        (Source => generic_register, Target => IOREGWIN);
   BEGIN
      Intrinsics.Memory_Fence;
      Mapped_IO_APIC.MMIO.Register_Window := Value_Number(Register_Data);
      Intrinsics.Memory_Fence;
   END Write_IO_APIC;

   PROCEDURE Switch_IO_APIC_Register
     (Mapped_IO_APIC : IN OUT IO_APIC;
      Register       : IN IO_APIC_register;
      GSI            : IN number := number'last)
   IS
      SUBTYPE IOREGSEL IS number RANGE 0 .. 16#FF#;
      FUNCTION Index_Number IS NEW Ada.Unchecked_Conversion
        (Source => IO_APIC_register, Target => IOREGSEL);

      -- While the index will never get this big, we'll mask it anyway so
      -- proving it is easier without needing a more realistic GSI limit.
      Index : CONSTANT number RANGE 0 .. 16#FFFFFFFF# :=
      (
         IF
            Register IN IOREDTBL_low | IOREDTBL_high
         THEN -- The +1 offset is handled by the register's representation.
            Index_Number(Register) + GSI * 2 AND 16#FFFFFFFF#
         ELSE
            Index_Number(Register)
      );
   BEGIN
      Intrinsics.Memory_Fence;
      Mapped_IO_APIC.MMIO.Register_Index := Index;
      Intrinsics.Memory_Fence;
   END Switch_IO_APIC_Register;

   PROCEDURE Enumerate_MADT
     (Paging_Structure : IN OUT Paging.page_layout)
   WITH
      Refined_Global => (In_Out => (IO_APICs, IRQ_Remaps, CPU_Cores))
   IS
      FUNCTION Get_IO_APIC_Version IS NEW Read_IO_APIC
        (generic_register => IO_APIC_version_register);

      MADT_Address : CONSTANT address :=
         (IF ACPI.Valid_Implementation THEN ACPI.Table_Address("APIC") ELSE 0);

      APICs        : interrupt_controller_descriptors;
   BEGIN
      IF
         MADT_Address /= 0
      THEN
         APICs := ACPI.Get_APICs(MADT_Address);
      ELSE -- Occurs if the ACPI implementation is damaged.
         CPU_Cores := 1; -- A single APIC will always exist.
         RETURN;
      END IF;

      FOR
         APIC_Entry OF APICs
      LOOP
         EXIT WHEN APIC_Entry.Record_Address = 0;

         CASE
            APIC_Entry.Enumeration_Value
         IS
            WHEN local_APIC_entry | local_x2APIC_entry =>
               CPU_Cores := CPU_Cores + 1;
            WHEN IO_APIC_entry =>
               DECLARE
                  FUNCTION To_Pointer
                    (IO_APIC_Address : IN address)
                    RETURN access_IO_APIC_MMIO
                  WITH
                     Import     => true,
                     Convention => Intrinsic,
                     Pre        => IO_APIC_Address /= 0,
                     Post       => To_Pointer'result /= NULL;

                  New_IO_APIC : CONSTANT IO_APIC_descriptor
                  WITH
                     Import     => true,
                     Convention => C,
                     Address    => APIC_Entry.Record_Address;
               BEGIN
                  FOR
                     Mapped_IO_APIC OF IO_APICs
                  LOOP
                     PRAGMA Loop_Invariant
                     (
                        IF
                           Mapped_IO_APIC.MMIO = NULL
                        THEN
                           Mapped_IO_APIC.GSI_Base = 0 AND THEN
                           Mapped_IO_APIC.GSI_Last = 0
                     );

                     IF
                        Mapped_IO_APIC.MMIO = NULL AND THEN
                        New_IO_APIC.IO_APIC_Address /= 0
                     THEN
                        -- All of the I/O APICs could fit inside a single page
                        -- frame of 4-KiB, but repeat this just to be sure.
                        Paging_Structure.Map_Address
                          (New_IO_APIC.IO_APIC_Address,
                           New_IO_APIC.IO_APIC_Address,
                           Write_Access => true);

                        Mapped_IO_APIC.MMIO := To_Pointer
                          (New_IO_APIC.IO_APIC_Address);

                        Switch_IO_APIC_Register(Mapped_IO_APIC, IOAPICVER);

                        Mapped_IO_APIC.GSI_Last := Get_IO_APIC_Version
                          (Mapped_IO_APIC).Maximum_Redirects;

                        Mapped_IO_APIC.GSI_Last := Mapped_IO_APIC.GSI_Last +
                           Mapped_IO_APIC.GSI_Base;

                        -- The below checks are done so `gnatprove` can prove
                        -- that the GSI base vector will always be at or lower
                        -- than the last GSI vector. An assumption can also be
                        -- used for the hardware itself, but avoid it anyway.
                        IF
                           Mapped_IO_APIC.GSI_Last >= New_IO_APIC.GSI_Base
                        THEN
                           Mapped_IO_APIC.GSI_Base := New_IO_APIC.GSI_Base;
                        ELSE
                           Mapped_IO_APIC.GSI_Base := Mapped_IO_APIC.GSI_Last;
                        END IF;

                        EXIT WHEN true;
                     END IF;
                  END LOOP;
               END;
            WHEN interrupt_source_override_entry =>
               DECLARE
                  IRQ_Remap : CONSTANT interrupt_source_override_descriptor
                  WITH
                     Import     => true,
                     Convention => C,
                     Address    => APIC_Entry.Record_Address;
               BEGIN
                  IF
                     IRQ_Remap.IRQ_Value IN IRQ_Remaps'range
                  THEN
                     IRQ_Remaps(IRQ_Remap.IRQ_Value).Present := true;
                     IRQ_Remaps(IRQ_Remap.IRQ_Value).GSI :=
                        IRQ_Remap.GSI_Value;
                     IRQ_Remaps(IRQ_Remap.IRQ_Value).Polarity :=
                        IRQ_Remap.Polarity;
                     IRQ_Remaps(IRQ_Remap.IRQ_Value).Trigger :=
                        IRQ_Remap.Trigger_Mode;
                  END IF;
               END;
            WHEN OTHERS =>
               NULL;
         END CASE;
      END LOOP;

      IF -- Occurs if the MADT is completely empty.
         CPU_Cores  = 0
      THEN
         CPU_Cores := 1;
      END IF;
   END Enumerate_MADT;

   PROCEDURE Reset
   IS
      PROCEDURE Set_End_Of_Interrupt_Register IS NEW Write_LAPIC
        (generic_format => number);
   BEGIN
      Set_End_Of_Interrupt_Register(end_of_interrupt_register, 0);
   END Reset;

   PROCEDURE x2APIC_Mode
   IS
      FUNCTION  Get_IA32_APIC_BASE IS NEW  Read_LAPIC
        (generic_format => IA32_APIC_BASE_format);
      PROCEDURE Set_IA32_APIC_BASE IS NEW Write_LAPIC
        (generic_format => IA32_APIC_BASE_format);

      FUNCTION  Get_Spurious_Interrupt_Vector_Register IS NEW  Read_LAPIC
        (generic_format => spurious_interrupt_vector_register_format);
      PROCEDURE Set_Spurious_Interrupt_Vector_Register IS NEW Write_LAPIC
        (generic_format => spurious_interrupt_vector_register_format);

      PROCEDURE Set_Task_Priority_Register IS NEW Write_LAPIC
        (generic_format => number);

      APIC_Base    : IA32_APIC_BASE_format :=
         Get_IA32_APIC_BASE(IA32_APIC_BASE);
      SIV_Register : spurious_interrupt_vector_register_format;
   BEGIN
      -- I am unsure of how much UEFI has messed with the LAPIC, so I will
      -- be moderately safe and redundant in the setup of it.

      APIC_Base.x2APIC_Mode := true;
      APIC_Base.Global_APIC := true;
      Set_IA32_APIC_BASE(IA32_APIC_BASE, APIC_Base);

      SIV_Register := Get_Spurious_Interrupt_Vector_Register
        (spurious_interrupt_vector_register);

      -- Send all spurious interrupts to the last possible IRQ. I could use
      -- IRQ 7 for this, but this seems better.
      SIV_Register.Interrupt_Vector :=  255;
      SIV_Register.Enabled_APIC     := true;

      Set_Spurious_Interrupt_Vector_Register
        (spurious_interrupt_vector_register, SIV_Register);

      -- Zero out the task priority register so all priority interrupts are
      -- accepted from the I/O APIC, just in case.
      Set_Task_Priority_Register(task_priority_register, 0);

      Log("BSP local APIC enabled in x2APIC mode.", nominal);
   END x2APIC_Mode;

   PROCEDURE Set_IO_APIC_Redirects
   WITH
      Refined_Global => (In_Out => IO_APICs, Input => IRQ_Remaps)
   IS
      PROCEDURE Set_Redirect    IS NEW Write_IO_APIC
        (generic_register => IO_APIC_redirection_table_register_low);
      PROCEDURE Set_Destination IS NEW Write_IO_APIC
        (generic_register => IO_APIC_redirection_table_register_high);

      FUNCTION Get_APIC_Identity IS NEW Read_LAPIC
        (generic_format => number);

      Redirection           : IO_APIC_redirection_table_register_low;
      Current_APIC_Identity : CONSTANT number :=
         Get_APIC_Identity(identity_register);

      -- TODO: The I/O APIC datasheet I'm using seems to be very old and I
      -- cannot find a newer version, but the supposed physical LAPIC ID the
      -- I/O APIC can redirect interrupts to can apparently only be 4 bits in
      -- physical mode. One problem is that it's 8 bits in xAPIC mode and 32
      -- bits in x2APIC mode. The I/O APIC register can fit in 8 bits, even
      -- when it's only told to be possible for logical mode. No idea.
      Destination : CONSTANT IO_APIC_redirection_table_register_high :=
        (Destination => Current_APIC_Identity AND 2#1111#,
         Reserved    => 0);
   BEGIN
      FOR -- First, we identity-map the non-remapped IRQs as an assumption.
         IRQ IN IRQ_Remaps'range
      LOOP
         FOR
            Mapped_IO_APIC OF IO_APICs
         LOOP
            IF
               Mapped_IO_APIC.MMIO /= NULL
            THEN
               IF
                  NOT IRQ_Remaps(IRQ).Present AND THEN
                  IRQ IN Mapped_IO_APIC.GSI_Base .. Mapped_IO_APIC.GSI_Last
               THEN
                  -- ISA IRQs are edge triggered and active high by default.
                  Redirection.Interrupt_Vector := IRQ_Base + IRQ;
                  Redirection.Delivery_Mode    := fixed_delivery;
                  Redirection.Active_Low       := false;
                  Redirection.Level_Sensitive  := false;

                  Switch_IO_APIC_Register(Mapped_IO_APIC, IOREDTBL_low,
                     GSI => IRQ - Mapped_IO_APIC.GSI_Base);
                  Set_Redirect(Mapped_IO_APIC, Redirection);

                  Switch_IO_APIC_Register(Mapped_IO_APIC, IOREDTBL_high,
                     GSI => IRQ - Mapped_IO_APIC.GSI_Base);
                  Set_Destination(Mapped_IO_APIC, Destination);
               END IF;
            END IF;
         END LOOP;
      END LOOP;

      FOR -- Secondly, we do the remaps accordingly.
         IRQ IN IRQ_Remaps'range
      LOOP
         FOR
            Mapped_IO_APIC OF IO_APICs
         LOOP
            IF
               Mapped_IO_APIC.MMIO /= NULL
            THEN
               IF
                  IRQ_Remaps(IRQ).Present AND THEN
                  IRQ_Remaps(IRQ).GSI IN Mapped_IO_APIC.GSI_Base ..
                     Mapped_IO_APIC.GSI_Last
               THEN
                  -- Perform the appropriate changes to the IRQ.
                  Redirection.Interrupt_Vector := IRQ_Base + IRQ;
                  Redirection.Delivery_Mode := fixed_delivery;
                  Redirection.Active_Low := (IF IRQ_Remaps(IRQ).Polarity /=
                     ACPI.active_low_or_level_triggered_signal THEN false);
                  Redirection.Level_Sensitive := (IF IRQ_Remaps(IRQ).Trigger /=
                     ACPI.active_low_or_level_triggered_signal THEN false);

                  Switch_IO_APIC_Register(Mapped_IO_APIC, IOREDTBL_low,
                     GSI => IRQ_Remaps(IRQ).GSI - Mapped_IO_APIC.GSI_Base);
                  Set_Redirect(Mapped_IO_APIC, Redirection);

                  Switch_IO_APIC_Register(Mapped_IO_APIC, IOREDTBL_high,
                     GSI => IRQ_Remaps(IRQ).GSI - Mapped_IO_APIC.GSI_Base);
                  Set_Destination(Mapped_IO_APIC, Destination);
               END IF;
            END IF;
         END LOOP;
      END LOOP;
   END Set_IO_APIC_Redirects;

END HAVK_Kernel.APIC;
