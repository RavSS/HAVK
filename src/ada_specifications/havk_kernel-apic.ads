-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-apic.ads                                   --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.ACPI,
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Paging;
USE
   HAVK_Kernel.ACPI;
USE TYPE
   HAVK_Kernel.Intrinsics.model_specific_register;

-- This package handles everything to do with the modern APIC architecture.
-- That includes the I/O APIC and the LAPIC (only plan x2APIC support for now).
PACKAGE HAVK_Kernel.APIC
WITH
   Abstract_State => Interrupt_Controller_State
IS
   PRAGMA Preelaborate;

   -- Each LAPIC (not I/O APIC) belongs to a logical core. Initialised to
   -- zero so counting it is easier during enumeration without subtraction.
   CPU_Cores : number := 0;

   -- Enumerates the ACPI table that contains APIC information. Also takes
   -- in a page layout so it can map any APICs if MMIO is required.
   PROCEDURE Enumerate_MADT
     (Paging_Structure : IN OUT Paging.page_layout)
   WITH
      Global => (In_Out => (Interrupt_Controller_State, CPU_Cores)),
      Pre    => ACPI.Valid_Implementation,
      Post   => CPU_Cores /= 0;

   -- This remaps the two 8248 PICs and it can optionally disable the emulation
   -- so the APIC architecture can be properly used instead. Check how HAVK
   -- signals an EOI (end of interrupt) signal if you wish to use the PICs for
   -- whatever reason.
   PROCEDURE Remap_PICs
     (Disable_Emulation : boolean := false)
   WITH
      Pre => (IF Disable_Emulation THEN ACPI.Valid_Implementation);

   -- Flips the global and x2APIC-mode bits to enable them, then it enables
   -- the LAPIC itself. Does not check for the relevant CPUID bits as of now.
   PROCEDURE x2APIC_Mode;

   -- Since HAVK (for now) still relies upon PS/2 input, we need the old ISA
   -- IRQs so we can avoid polling the PS/2 controller.
   PROCEDURE Set_IO_APIC_Redirects
   WITH
      Global => (In_Out => Interrupt_Controller_State),
      Pre    => ACPI.Valid_Implementation;

   -- Simply writes a zero to the LAPIC's EOI register.
   PROCEDURE Reset
   WITH
      Inline => true;

PRIVATE
   -- The record for the value that can be read and written to the
   -- IA32_APIC_BASE MSR. Note that not all fields are writable, some
   -- are read-only.
   TYPE IA32_APIC_BASE_format IS RECORD
      -- Used for nothing. Read only.
      Reserved_1  : number RANGE 0 .. 16#00000FF#;
      -- Indicates whether the LAPIC belongs to a BSP or an AP. Read only.
      BSP_APIC    : boolean;
      -- Used for nothing. Read only.
      Reserved_2  : void;
      -- The bit that enables x2APIC mode and disables xAPIC. MMIO access
      -- is replaced with MSR access of the APIC.
      x2APIC_Mode : boolean;
      -- Interrupts are delivered in a flat state or something along those
      -- lines. The old clustering format isn't present in x2APIC. Both
      -- this and the x2APIC mode bit must be set to actually use the x2APIC
      -- interface.
      Global_APIC : boolean;
      -- The base address of the LAPIC unique to the current processor core.
      -- This can be moved, but it must be page-aligned.
      APIC_Base   : number RANGE 0 .. 16#0FFFFFF#;
      -- Used for nothing. Read only.
      Reserved_3  : number RANGE 0 .. 16#FFFFFFF#;
   END RECORD;
   FOR IA32_APIC_BASE_format USE RECORD
      Reserved_1      AT 0 RANGE 0 .. 07;
      BSP_APIC        AT 1 RANGE 0 .. 00;
      Reserved_2      AT 1 RANGE 1 .. 01;
      x2APIC_Mode     AT 1 RANGE 2 .. 02;
      Global_APIC     AT 1 RANGE 3 .. 03;
      APIC_Base       AT 1 RANGE 4 .. 27;
      Reserved_3      AT 4 RANGE 4 .. 31;
   END RECORD;

   -- The record for the value that can be read and written to the local APIC's
   -- spurious interrupt vector register. It is used for enabling the APIC
   -- itself and chosing which interrupt vector spurious interrupts go to.
   TYPE spurious_interrupt_vector_register_format IS RECORD
      -- The interrupt vector to which spurious interrupts are delivered to.
      -- The lower four bits are hard-set to one.
      Interrupt_Vector : number RANGE 15 .. 255;
      -- This is what we're here for. It simply enables the APIC if true.
      Enabled_APIC     : boolean;
      -- In xAPIC mode, the bit for CPU focus checking is here, but it seems
      -- to be missing in x2APIC mode.
      Reserved_1       : number RANGE 00 .. 007;
      -- When true, EOI messages are not sent to the I/O APICs. This is an
      -- exclusive x2APIC feature.
      No_EOI_Broadcast : boolean;
      -- While the register itself is only 32 bits, MSRs have a maximum value
      -- of 64 bits, so this is just empty padding that is handled by the
      -- `WRMSR` instruction.
      Reserved_2       : number RANGE 00 .. 000;
   END RECORD;
   FOR spurious_interrupt_vector_register_format USE RECORD
      Interrupt_Vector     AT 0 RANGE 00 .. 007;
      Enabled_APIC         AT 0 RANGE 08 .. 008;
      Reserved_1           AT 0 RANGE 09 .. 011;
      No_EOI_Broadcast     AT 0 RANGE 12 .. 012;
      Reserved_2           AT 0 RANGE 13 .. 063;
   END RECORD;

   -- The I/O APIC and LAPIC can deliver interrupts in several ways. For the
   -- most part, I expect this to just be set to the fixed delivery. As of
   -- typing this, the delivery modes seem to match up with the two different
   -- APIC types. See the datasheet for the 82093AA (I/O APIC).
   TYPE interrupt_delivery IS
     (fixed_delivery,
      lowest_priority_delivery,
      SMI_delivery,
      reserved_delivery_1,
      NMI_delivery,
      INIT_delivery,
      reserved_delivery_2,
      ExtINT_delivery)
   WITH
      Size => 3;
   FOR interrupt_delivery USE
     (fixed_delivery           => 2#000#,
      lowest_priority_delivery => 2#001#,
      SMI_delivery             => 2#010#,
      reserved_delivery_1      => 2#011#,
      NMI_delivery             => 2#100#,
      INIT_delivery            => 2#101#,
      reserved_delivery_2      => 2#110#,
      ExtINT_delivery          => 2#111#);

   -- For the memory-mapped I/O APIC, there are two registers. The first
   -- (register index) indicates what type of register to expose and the second
   -- register is the register itself (it can be read only or read and write).
   -- Interrupt redirections are calculated by checking the maximum amount
   -- of redirect entries in the IOAPICARB register and then applying the
   -- formula "offset = 0x10 + ($GSI * 2)" to get the e.g. lower IOREDTBL or
   -- by adding a single one to it to receive the higher IOREDTBL register.
   -- It must be read in two reads, as the entries are 64-bit structures, but
   -- we only have a single 32-bit MMIO register. I have not hard-coded in the
   -- offsets for the redirection entries as there can be 24 of them.
   TYPE IO_APIC_register IS
     (IOAPICID,      -- Identification register. Read and write.
      IOAPICVER,     -- Version register. Read only.
      IOAPICARB,     -- Arbitration register. Read only.
      IOREDTBL_low,  -- Redirection table lower register. Field-specific R/W.
      IOREDTBL_high) -- Redirection table higher register. Field-specific R/W.
   WITH
      Size => 8; -- The register index is a 32-bit value, but it has a limit.
   FOR IO_APIC_register USE
     (IOAPICID      => 16#00#,
      IOAPICVER     => 16#01#,
      IOAPICARB     => 16#02#,
      IOREDTBL_low  => 16#10#,  -- GSI/IRQ 0 (lower 32 bits) is the base.
      IOREDTBL_high => 16#11#); -- GSI/IRQ 0 (higher 32 bits) is the base.

   -- The register for the I/O APIC which contains the identity. The index
   -- is zero.
   TYPE IO_APIC_identity_register IS RECORD
      Reserved_1 : number RANGE 00 .. 16#FFFFFF#;
      -- The identity of the I/O APIC. This is also found in the MADT.
      Identity   : number RANGE 00 .. 16#00000F#;
      Reserved_2 : number RANGE 00 .. 16#00000F#;
   END RECORD;
   FOR IO_APIC_identity_register USE RECORD
      Reserved_1     AT 0 RANGE 00 .. 23;
      Identity       AT 0 RANGE 24 .. 27;
      Reserved_2     AT 0 RANGE 28 .. 31;
   END RECORD;

   -- An important register for the I/O APIC which can be used to give a range
   -- of GSIs (I/O APIC interrupt inputs) that it supports. The index is one.
   TYPE IO_APIC_version_register IS RECORD
      -- The I/O APIC's version or iteration.
      Version           : number RANGE 00 .. 16#FF#;
      Reserved_1        : number RANGE 00 .. 16#FF#;
      -- The maximum number of redirect entries for IRQs we can provide.
      -- Note that the count begins from zero. See the I/O APIC register
      -- type's comments for an understanding on how to expose them.
      -- Only 239 (240 from one start) redirects are possible for an I/O APIC.
      Maximum_Redirects : number RANGE 00 .. 16#EF#;
      Reserved_2        : number RANGE 00 .. 16#FF#;
   END RECORD;
   FOR IO_APIC_version_register USE RECORD
      Version               AT 0 RANGE 00 .. 07;
      Reserved_1            AT 0 RANGE 08 .. 15;
      Maximum_Redirects     AT 0 RANGE 16 .. 23;
      Reserved_2            AT 0 RANGE 24 .. 31;
   END RECORD;

   -- A register for the I/O APIC that serves a more advanced purpose to do
   -- with I/O APIC priorities. It is of little use to us. The index is two.
   TYPE IO_APIC_arbitration_register IS RECORD
      Reserved_1        : number RANGE 00 .. 16#FFFFFF#;
      -- The arbitration identity value. This has to do with disputing
      -- bus ownership. Essentially a priority for the APIC.
      Arbitration_Value : number RANGE 00 .. 16#00000F#;
      Reserved_2        : number RANGE 00 .. 16#00000F#;
   END RECORD;
   FOR IO_APIC_arbitration_register USE RECORD
      Reserved_1            AT 0 RANGE 00 .. 23;
      Arbitration_Value     AT 0 RANGE 24 .. 27;
      Reserved_2            AT 0 RANGE 28 .. 31;
   END RECORD;

   -- The most vital register which lets us tell the I/O APIC to redirect GSIs
   -- to LAPIC IRQs by defining the property's of the interrupt. The index is
   -- (16 + ($GSI * 2)) and the IRQ is defined inside that redirection entry.
   TYPE IO_APIC_redirection_table_register_low IS RECORD
      -- The interrupt vector to redirect the interrupt to. Can be read and
      -- written to. The actual vector range is 0x10 to 0xFE, but this is
      -- zeroed out in the register by default.
      Interrupt_Vector  : number RANGE 0 .. 16#FF# := 32;
      -- How the interrupt is delivered. See the enumeration type for
      -- more detail. Can be read and written to.
      Delivery_Mode     : interrupt_delivery := fixed_delivery;
      -- When true, the I/O APIC delivers interrupt in logical mode (as
      -- opposed to physical mode). See the IOREDTBL's higher 32 bits
      -- for an understanding of how this impacts the destination.
      Logical_Mode      : boolean := false;
      -- A read-only bit that indicates (when true) that the interrupt
      -- is pending for delivery. Writes do nothing.
      Delivery_Status   : boolean := false;
      -- When true, the polarity of the signal is active-low. When false,
      -- the polarity of the interrupt signal is active-high. For
      -- example, ISA IRQs are usually active-high unless specified
      -- otherwise in the MADT. Read and write access.
      Active_Low        : boolean := false;
      -- This bit only affects level triggered interrupts and has no
      -- indication for edge triggered interrupts. When true, the LAPIC
      -- has accepted the interrupt and we're waiting for an EOI message
      -- with the exact same interrupt vector we sent it to. Read only.
      Waiting_For_EOI   : boolean := false;
      -- When true, the signal type is level sensitive. When false, it is
      -- edge sensitive. The implementation details in the hardware (like
      -- voltage level) are mostly if not completely irrelevant. This is
      -- important for ISA IRQs and some other minutiae. Reading and
      -- writing is possible.
      Level_Sensitive   : boolean := false;
      -- When true, the interrupt is masked i.e. disabled. There's a few
      -- intricacies to this, so consult the I/O APIC's datasheet.
      -- Reading and writing is possible.
      Interrupt_Masked  : boolean := false;
      Reserved          : number RANGE 00 .. 16#7FFF# := 0;
   END RECORD;
   FOR IO_APIC_redirection_table_register_low USE RECORD
      Interrupt_Vector      AT 0 RANGE 00 .. 07;
      Delivery_Mode         AT 0 RANGE 08 .. 10;
      Logical_Mode          AT 0 RANGE 11 .. 11;
      Delivery_Status       AT 0 RANGE 12 .. 12;
      Active_Low            AT 0 RANGE 13 .. 13;
      Waiting_For_EOI       AT 0 RANGE 14 .. 14;
      Level_Sensitive       AT 0 RANGE 15 .. 15;
      Interrupt_Masked      AT 0 RANGE 16 .. 16;
      Reserved              AT 0 RANGE 17 .. 31;
   END RECORD;

   -- The most vital register which lets us tell the I/O APIC to redirect GSIs
   -- to LAPIC/PIC IRQs by providing the destination. The destination relies on
   -- the address mode and the index is (16 + ($GSI * 2) + 1).
   TYPE IO_APIC_redirection_table_register_high IS RECORD
      Reserved    : number RANGE 00 .. 16#FFFFFF#;
      -- If in logical mode, then this is the logical destination address
      -- for the interrupt; otherwise, it is the physical destination
      -- address. In physical mode, this is actually only 4 bits wide and
      -- it is used to indicate the hard-coded LAPIC identity. In logical mode,
      -- the full 8 bits are used and it indicates a set of processors whose
      -- identities are instead determined by what the LAPIC has been
      -- programmed as. Those registers can be found in Intel's IA-32e guide,
      -- but for now, we will not use them.
      Destination : number RANGE 00 .. 16#0000FF#;
   END RECORD;
   FOR IO_APIC_redirection_table_register_high USE RECORD
      Reserved        AT 0 RANGE 00 .. 23;
      Destination     AT 0 RANGE 24 .. 31;
   END RECORD;

   -- The ACPI MADT will have I/O APIC entries and the address will point
   -- to this record structure. A register is selected in the first record
   -- entry, then the entry at the base address plus 16 "transforms" into that
   -- specified register. Use generics for the register-to-record conversion.
   -- WARNING: The register window and register index must both be written
   -- and read atomically. Making this into an unchecked union and only doing
   -- partial reads of them will not work properly (verified with QEMU).
   -- Each register must be read on a 4-byte address or the above will happen.
   -- See the below (likely outdated) resource for register formats.
   -- READ: https://pdos.csail.mit.edu/6.828/2018/readings/ia32/ioapic.pdf
   TYPE IO_APIC_MMIO IS RECORD
      -- The register type needs to be set here in its numerical form or else
      -- you will get unexpected results. See the I/O APIC register type for
      -- retrieving the IRQ redirection entries and how to access both
      -- registers or use the generic wrappers that take care of it for you.
      Register_Index  : number RANGE 0 .. 16#FFFFFFFF#;
      Padding_1       : number;
      Padding_2       : number RANGE 0 .. 16#FFFFFFFF#;
      Register_Window : number RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH -- The "Atomic" aspect doesn't work, but "Volatile" is sort of similar.
      Volatile => true; -- Essentially "volatile" in C. Good enough.
   FOR IO_APIC_MMIO USE RECORD
      Register_Index     AT 00 RANGE 0 .. 31;
      Padding_1          AT 04 RANGE 0 .. 63;
      Padding_2          AT 12 RANGE 0 .. 31;
      Register_Window    AT 16 RANGE 0 .. 31;
   END RECORD;

   -- Convert the address in the MADT to an access type to utilise this.
   TYPE access_IO_APIC_MMIO IS ACCESS IO_APIC_MMIO;

   -- Just made for convenience so we can escape ACPI's complexity of getting
   -- an I/O APIC's information from the MADT while also storing the GSI limit
   -- without needing to constantly read the I/O APIC.
   TYPE IO_APIC IS RECORD
      -- Access to the I/O APIC itself.
      MMIO     : access_IO_APIC_MMIO := NULL;
      -- The interrupt input base. This is not relative, it is universal,
      -- so if I/O APIC 1 begins and ends at 0 to 23, this will be 24 for the
      -- next I/O APIC. Zero based.
      GSI_Base : number := 0;
      -- The end range of the interrupt inputs. This is retrieved from the
      -- I/O APIC's version register. Zero based.
      GSI_Last : number := 0;
   END RECORD
   WITH
      Dynamic_Predicate => (IF MMIO = NULL THEN
                              GSI_Base = 0  AND THEN
                              GSI_Last = 0) AND THEN
                           GSI_Last >= GSI_Base;

   -- For now, I'll support a maximum of 64 I/O APICs, which is more than
   -- you would find on basically all consumer systems.
   IO_APICs : ARRAY(number RANGE 1 .. 64) OF IO_APIC
   WITH
      Part_Of => Interrupt_Controller_State;

   -- I've decided to copy ACPI information into a more local space, this
   -- helps store it more easily in a manageable format.
   TYPE ISA_IRQ_IO_APIC_mapping IS RECORD
      Present  : boolean := false;
      GSI      : number RANGE 0 .. 16#FFFFFFFF# := 0;
      Polarity : ACPI.interrupt_signal := ACPI.default_ISA_or_EISA_signal;
      Trigger  : ACPI.interrupt_signal := ACPI.default_ISA_or_EISA_signal;
   END RECORD;

   -- The index represents a legacy ISA IRQ vector.
   IRQ_Remaps  : ARRAY(number RANGE 0 .. 15) OF ISA_IRQ_IO_APIC_mapping
   WITH
      Part_Of => Interrupt_Controller_State;

   -- The base address for the ISA IRQs. I've just set this to the regular
   -- remap base that we programmed to the PICs.
   IRQ_Base    : CONSTANT number := 32;

   -- The MSRs for retrieving the current CPU core's (local) APIC.
   -- This is not complete, but the other registers seem to not be of any
   -- interest. Note that the "base" registers have a representation range up
   -- to the next register's representation. For example, the interrupt
   -- request register takes in 256 bits, so increment 1 to the base address
   -- to get the next 64 bits and so on.
   TYPE LAPIC_MSR IS                      ---- See any of the formats for them.
     (IA32_APIC_BASE,                     -- Read/write.
      identity_register,                  -- Read only.
      version_register,                   -- Read only.
      task_priority_register,             -- 0-to-7 read/write. Rest reserved.
      process_priority_register,          -- Read only.
      end_of_interrupt_register,          -- Can only write zero. No reading.
      spurious_interrupt_vector_register, -- 0-to-8 and 12 are writable.
      in_service_register_base,           -- 256-bit. Read only.
      trigger_mode_register_base,         -- 256-bit. Read only.
      interrupt_request_register_base,    -- 256-bit. Read only.
      error_status_register,              -- Read/write. Can only write zero.
      interrupt_command_register,         -- Read/write. 64-bit `WRMSR` write.
      timer_register,                     -- Read/write.
      local_interrupt_0_register,         -- Read/write.
      local_interrupt_1_register,         -- Read/write.
      local_error_status_register,        -- Read/write.
      timer_initial_count_register,       -- Read/write.
      timer_current_count_register,       -- Read only.
      timer_divisor_register)             -- Read/write.
   WITH
      Size => 32;
   FOR LAPIC_MSR USE -- 0x800 is the base MSR for the register offsets.
     (IA32_APIC_BASE                     => 16#1B#, -- Doesn't use the base.
      identity_register                  => 16#800# + 16#02#,
      version_register                   => 16#800# + 16#03#,
      task_priority_register             => 16#800# + 16#08#,
      process_priority_register          => 16#800# + 16#0A#,
      end_of_interrupt_register          => 16#800# + 16#0B#,
      spurious_interrupt_vector_register => 16#800# + 16#0F#,
      in_service_register_base           => 16#800# + 16#10#,
      trigger_mode_register_base         => 16#800# + 16#18#,
      interrupt_request_register_base    => 16#800# + 16#20#,
      error_status_register              => 16#800# + 16#28#,
      interrupt_command_register         => 16#800# + 16#30#,
      timer_register                     => 16#800# + 16#32#,
      local_interrupt_0_register         => 16#800# + 16#35#,
      local_interrupt_1_register         => 16#800# + 16#36#,
      local_error_status_register        => 16#800# + 16#37#,
      timer_initial_count_register       => 16#800# + 16#38#,
      timer_current_count_register       => 16#800# + 16#39#,
      timer_divisor_register             => 16#800# + 16#3E#);

   -- Reads an MSR's value into a number or a format depending on the
   -- instantiation.
   GENERIC
      TYPE generic_format IS PRIVATE;
   FUNCTION Read_LAPIC
     (MSR   : IN LAPIC_MSR)
     RETURN generic_format;

   -- Writes to an MSR a number or a format depending on the instantiation.
   GENERIC
      TYPE generic_format IS PRIVATE;
   PROCEDURE Write_LAPIC
     (MSR   : IN LAPIC_MSR;
      Value : IN generic_format);

   -- Changes the active register window of an I/O APIC. This should be called
   -- before any reads or writes to an I/O APIC. It is not merged into the
   -- I/O procedures and functions because functions cannot have side-effects.
   -- I/O APICs can handle a GSI range of 0 to 23. If the IOREDTBL register(s)
   -- is/are given, then an interrupt must also be provided.
   PROCEDURE Switch_IO_APIC_Register
     (Mapped_IO_APIC : IN OUT IO_APIC;
      Register       : IN IO_APIC_register;
      GSI            : IN number := number'last)
   WITH
      Pre  => Mapped_IO_APIC.MMIO /= NULL AND THEN
             ( -- Make sure the GSI is present for the I/O APIC.
                IF Register IN IOREDTBL_low | IOREDTBL_high THEN
                   GSI <= Mapped_IO_APIC.GSI_Last - Mapped_IO_APIC.GSI_Base
             ),
      Post => Mapped_IO_APIC.MMIO        /= NULL                    AND THEN
              Mapped_IO_APIC.GSI_Base'old = Mapped_IO_APIC.GSI_Base AND THEN
              Mapped_IO_APIC.GSI_Last'old = Mapped_IO_APIC.GSI_Last;

   -- Reads the second register from a memory-mapped I/O APIC.
   GENERIC
      TYPE generic_register IS PRIVATE;
   FUNCTION Read_IO_APIC
     (Mapped_IO_APIC : IN IO_APIC)
     RETURN generic_register
   WITH
      Pre => Mapped_IO_APIC.MMIO /= NULL;

   -- Writes to the second register for a memory-mapped I/O APIC.
   GENERIC
      TYPE generic_register IS PRIVATE;
   PROCEDURE Write_IO_APIC
     (Mapped_IO_APIC : IN OUT IO_APIC;
      Register_Data  : IN generic_register)
   WITH
      Pre  => Mapped_IO_APIC.MMIO        /= NULL,
      Post => Mapped_IO_APIC.MMIO        /= NULL                    AND THEN
              Mapped_IO_APIC.GSI_Base'old = Mapped_IO_APIC.GSI_Base AND THEN
              Mapped_IO_APIC.GSI_Last'old = Mapped_IO_APIC.GSI_Last;

END HAVK_Kernel.APIC;
