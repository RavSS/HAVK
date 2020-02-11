-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-descriptors.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- Handles the CPU's descriptor functionality, which is necessary for many
-- things, such as privilege level shifts and processor interrupts.
PACKAGE HAVK_Kernel.Descriptors
IS
   PRAGMA Preelaborate;

   -- https://wiki.osdev.org/Global_Descriptor_Table
   TYPE global_descriptor_table_segment_access IS RECORD
      -- Only touched by the CPU, never by me (except for setting up a TSS).
      Accessed                : boolean;
      -- Bit depends on the entry's segment. If code, then this indicates
      -- read access. If data, then this indicates write access. Code segments
      -- can never be written to and data segments are always readable.
      Readable_Or_Writable    : boolean;
      -- For a data segment, this indicates the direction of growth
      -- (1 for downwards and vice versa). For a code segment, this is the
      -- conforming bit, which indicates whether the code segment conforms
      -- only to the privilege ring it is set to or it is allowed to be
      -- called by higher privilege rings.
      Direction_Or_Conforming : boolean;
      -- Should obviously always be 1 for a code segment and the opposite
      -- for a data segment.
      Executable              : boolean;
      -- Needs to be set for the code or data segments and only disabled for
      -- system segments e.g. a TSS.
      Descriptor_Type         : boolean;
      -- There's only 3 privilege rings for IA-32e/AMD64, so two bits
      -- are required to store it.
      Privilege_Ring          : number RANGE 0 .. 3;
      -- This must always be 1, or else the selector doesn't exist.
      -- For the GDT entries, I will only have as many as I need (unlike
      -- the IDT gates, where I fill the entire range in).
      Present                 : boolean;
   END RECORD;
   FOR global_descriptor_table_segment_access USE RECORD
      Accessed                    AT 0 RANGE 0 .. 0;
      Readable_Or_Writable        AT 0 RANGE 1 .. 1;
      Direction_Or_Conforming     AT 0 RANGE 2 .. 2;
      Executable                  AT 0 RANGE 3 .. 3;
      Descriptor_Type             AT 0 RANGE 4 .. 4;
      Privilege_Ring              AT 0 RANGE 5 .. 6;
      Present                     AT 0 RANGE 7 .. 7;
   END RECORD;

   TYPE global_descriptor_table_descriptor_flags IS RECORD
      -- This is actually an "Available" field, but we shall ignore it.
      Zeroed                 : number RANGE 0 .. 0;
      -- Clearly must be set to one as this is for x86-64.
      Long_Descriptor_Size   : boolean;
      -- 0 for 16-bit protected mode size, 1 for 32-bit protected mode size.
      -- This must be zero for a long mode descriptor.
      Legacy_Descriptor_Size : boolean;
      -- 0 for byte granularity, 1 for page (4 KiB) granularity.
      -- This applies to the end address value only, I think...
      Granularity            : boolean;
   END RECORD;
   FOR global_descriptor_table_descriptor_flags USE RECORD
      Zeroed                     AT 0 RANGE 0 .. 0;
      Long_Descriptor_Size       AT 0 RANGE 1 .. 1;
      Legacy_Descriptor_Size     AT 0 RANGE 2 .. 2;
      Granularity                AT 0 RANGE 3 .. 3;
   END RECORD;

   TYPE global_descriptor_table_descriptor IS RECORD
      -- The lower 16 bits of the segment's maximum address range.
      End_Address_Low     : address RANGE 0 .. 2**16 - 1;
      -- The lower 16 bits of where the segment's address begins.
      Base_Address_Low    : address RANGE 0 .. 2**16 - 1;
      -- The middle 8 bits of the segment's starting address.
      Base_Address_Middle : address RANGE 0 .. 2**08 - 1;
      -- Information for the descriptor. See "GDT_access" for more.
      Segment_Access      : global_descriptor_table_segment_access;
      -- The final 4 bits of the segment's end address.
      End_Address_High    : address RANGE 0 .. 2**04 - 1;
      -- Descriptor entry flags. See the "GDT_flags" record for more.
      Flags               : global_descriptor_table_descriptor_flags;
      -- End 8 bits of the 32-bit starting address for the segment.
      Base_Address_High   : address RANGE 0 .. 2**08 - 1;
   END RECORD;
   FOR global_descriptor_table_descriptor USE RECORD
      End_Address_Low          AT 0 RANGE 0 .. 15;
      Base_Address_Low         AT 2 RANGE 0 .. 15;
      Base_Address_Middle      AT 4 RANGE 0 .. 07;
      Segment_Access           AT 5 RANGE 0 .. 07;
      End_Address_High         AT 6 RANGE 0 .. 03;
      Flags                    AT 6 RANGE 4 .. 07;
      Base_Address_High        AT 7 RANGE 0 .. 07;
   END RECORD;

   -- The IDT gate entry differs a bit from x86.
   -- For x86-64, there seems to be a few more fields.
   -- https://wiki.osdev.org/Interrupt_Descriptor_Table#Structure_AMD64
   -- The call gates are not mentioned on the OSDev Wiki IDT page.
   -- I dug them out of AMD's AMD64 Architecture Programmer's Manual.
   -- It also doesn't mention 64-bit redefines and 0x4 to 0x7 gates becoming
   -- unusable in long mode as of 2019-05-18. Maybe update the wiki-page?
   TYPE interrupt_descriptor_table_gate_type IS
     (call_gate,
      interrupt_gate,
      trap_gate)
   WITH
      Size => 4;
   FOR interrupt_descriptor_table_gate_type USE
     (call_gate      => 12,
      interrupt_gate => 14,
      trap_gate      => 15);

   TYPE interrupt_descriptor_table_gate_attributes IS RECORD
      -- Specifies the type of gate for the interrupt.
      Gate            : interrupt_descriptor_table_gate_type := interrupt_gate;
      -- If not a storage segment and an interrupt or trap gate, then
      -- this must be set to zero. I am going to set it to that by default.
      Storage_Segment : boolean                              := false;
      -- Minimum privilege level for the descriptor that is trying to
      -- call the interrupt. Useful so userspace doesn't mess with
      -- kernel space interrupts for hardware etc.
      DPL             : number RANGE 0 .. 3                  := 0;
      -- Whether the interrupt is currently active. Set to zero by default,
      -- so all the blank interrupts are not set to present.
      Present         : boolean                              := false;
   END RECORD;
   FOR interrupt_descriptor_table_gate_attributes USE RECORD
      Gate                AT 0 RANGE 0 .. 3;
      Storage_Segment     AT 0 RANGE 4 .. 4;
      DPL                 AT 0 RANGE 5 .. 6;
      Present             AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Every field has been defaulted so it's easier to initialize an IDT array.
   TYPE interrupt_descriptor_table_gate IS RECORD
      -- The lower 16-bits of the 64-bit address belonging to the location
      -- of the ISR handler which handles this interrupt.
      ISR_Address_Low    : address RANGE 0 .. 2**16 - 1 := 0;
      -- Points to a code segment in our GDT depending on which ring can
      -- raise the interrupt.
      CS_Selector        : number  RANGE 0 .. 2**16 - 1 := 0;
      -- Only the first 3-bits do something, and they point to the
      -- the interrupt stack table offset (IST). The remaining 6 bits must
      -- be blank/zeroed. Disabled by default.
      IST_Offset         : number  RANGE 0 .. 2**08 - 1 := 0;
      -- Details the properties and attributes of the IDT gate. See the
      -- "interrupt_descriptor_table_attributes" record for more information.
      Type_Attributes    : interrupt_descriptor_table_gate_attributes;
      -- The 16-bits that reach the center of the 64-bit address for the
      -- ISR handler.
      ISR_Address_Middle : address RANGE 0 .. 2**16 - 1 := 0;
      -- The final 32-bits that complete the 64-bit address.
      ISR_Address_High   : address RANGE 0 .. 2**32 - 1 := 0;
      -- 32-bits of zeroes only and nothing else.
      Zeroed             : number  RANGE 0 .. 2**32 - 1 := 0;
   END RECORD;
   FOR interrupt_descriptor_table_gate USE RECORD
      ISR_Address_Low        AT 00 RANGE 0 .. 15;
      CS_Selector            AT 02 RANGE 0 .. 15;
      IST_Offset             AT 04 RANGE 0 .. 07;
      Type_Attributes        AT 05 RANGE 0 .. 07;
      ISR_Address_Middle     AT 06 RANGE 0 .. 15;
      ISR_Address_High       AT 08 RANGE 0 .. 31;
      Zeroed                 AT 12 RANGE 0 .. 31;
   END RECORD;

   TYPE task_state_segment IS RECORD
      Reserved_1 : number RANGE 0 .. 2**32 - 1 := 000;
      RSP_Ring_0 : address                     := 000;
      RSP_Ring_1 : address                     := 000;
      RSP_Ring_2 : address                     := 000;
      Reserved_2 : number                      := 000;
      IST_1      : address                     := 000;
      IST_2      : address                     := 000;
      IST_3      : address                     := 000;
      IST_4      : address                     := 000;
      IST_5      : address                     := 000;
      IST_6      : address                     := 000;
      IST_7      : address                     := 000;
      Reserved_3 : number                      := 000;
      Reserved_4 : number RANGE 0 .. 2**16 - 1 := 000;
      -- IOPB disabled by default (byte size of TSS).
      IOPB       : number RANGE 0 .. 2**16 - 1 := 104;
   END RECORD;
   FOR task_state_segment USE RECORD
      Reserved_1   AT 000 RANGE 0 .. 31;
      RSP_Ring_0   AT 004 RANGE 0 .. 63;
      RSP_Ring_1   AT 012 RANGE 0 .. 63;
      RSP_Ring_2   AT 020 RANGE 0 .. 63;
      Reserved_2   AT 028 RANGE 0 .. 63;
      IST_1        AT 036 RANGE 0 .. 63;
      IST_2        AT 044 RANGE 0 .. 63;
      IST_3        AT 052 RANGE 0 .. 63;
      IST_4        AT 060 RANGE 0 .. 63;
      IST_5        AT 068 RANGE 0 .. 63;
      IST_6        AT 076 RANGE 0 .. 63;
      IST_7        AT 084 RANGE 0 .. 63;
      Reserved_3   AT 092 RANGE 0 .. 63;
      Reserved_4   AT 100 RANGE 0 .. 15;
      IOPB         AT 102 RANGE 0 .. 15;
   END RECORD;

   -- The TSS descriptor that goes inside the GDT for long mode so that the
   -- address of the TSS can fit inside the hypothetical 64-bit address space.
   TYPE task_state_segment_descriptor IS RECORD
      Descriptor_TSS        : global_descriptor_table_descriptor;
      Base_Address_Extended : address RANGE 000 .. 2**32 - 1;
      Reserved_1            : number  RANGE 000 .. 2**08 - 1;
      Zeroed                : number  RANGE 000 .. 2**04 - 1;
      Reserved_2            : number  RANGE 000 .. 2**20 - 1;
   END RECORD;
   FOR task_state_segment_descriptor USE RECORD
      Descriptor_TSS             AT 0 RANGE 000 .. 063;
      Base_Address_Extended      AT 0 RANGE 064 .. 095;
      Reserved_1                 AT 0 RANGE 096 .. 103;
      Zeroed                     AT 0 RANGE 104 .. 107;
      Reserved_2                 AT 0 RANGE 108 .. 127;
   END RECORD;

   -- The default GDT (global descriptor table) layout that is compatible with
   -- the SCE (system call extension) instruction `REX.W SYSRET`.
   -- Increase the amount of GDT entries if need be. Bits 2 through 15 indicate
   -- the segment selector/descriptor index that I have indicated below whereas
   -- 0 to 1 indicate the RPL (requested privilege level). The order is very
   -- important.
   TYPE global_descriptor_table IS RECORD
      -- The null descriptor, which is never touched by the processor.
      Descriptor_Null      : global_descriptor_table_descriptor;
      -- The CS descriptor for ring 0 code. The index is 0x08.
      Descriptor_Kernel_CS : global_descriptor_table_descriptor;
      -- The DS descriptor for ring 0 data. The index is 0x10.
      Descriptor_Kernel_DS : global_descriptor_table_descriptor;
      -- The reserved descriptor for the system call instruction calculations.
      -- The index is 0x18.
      Descriptor_Reserved  : global_descriptor_table_descriptor;
      -- The DS descriptor for ring 3 data. The index is 0x20.
      Descriptor_User_DS   : global_descriptor_table_descriptor;
      -- The CS descriptor for ring 3 code. The index is 0x28.
      Descriptor_User_CS   : global_descriptor_table_descriptor;
      -- The TSS descriptor for software multi-tasking. The index is 0x30.
      -- Note that it takes up the space of two descriptors.
      Descriptor_TSS64     : task_state_segment_descriptor;
   END RECORD;
   FOR global_descriptor_table USE RECORD
      Descriptor_Null      AT 00 RANGE 0 .. 063;
      Descriptor_Kernel_CS AT 08 RANGE 0 .. 063;
      Descriptor_Kernel_DS AT 16 RANGE 0 .. 063;
      Descriptor_Reserved  AT 24 RANGE 0 .. 063;
      Descriptor_User_DS   AT 32 RANGE 0 .. 063;
      Descriptor_User_CS   AT 40 RANGE 0 .. 063;
      Descriptor_TSS64     AT 48 RANGE 0 .. 127;
   END RECORD;

   TYPE descriptor_table_register IS RECORD
      Table_Size   : number RANGE 0 .. 2**16 - 1;
      Base_Address : address;
   END RECORD;
   FOR descriptor_table_register USE RECORD
      Table_Size       AT 0 RANGE 0 .. 15;
      Base_Address     AT 2 RANGE 0 .. 63;
   END RECORD;

   -- The range is the amount of interrupts a logical core can handle.
   TYPE interrupt_descriptor_table IS ARRAY(number RANGE 0 .. 255)
      OF interrupt_descriptor_table_gate
   WITH
      Component_Size => 128,
      Pack           => true;

   -- The default IDT. Other packages can easily hook a new interrupt gate by
   -- assigning an interrupt entry to a specific vector/index.
   IDT : ALIASED interrupt_descriptor_table;

   -- The default TSS. Only need to use one when not using hardware
   -- multi-tasking (which we cannot use in long mode).
   TSS : ALIASED task_state_segment; -- TODO: Set the ring 0 stacks (IST).

   -- Loads the descriptor table registers with the GDT and IDT specified
   -- within this package.
   PROCEDURE Load;

   -- Fills the IDT with the default entries. This is not declared in the
   -- package specification so as to prevent a longer elaboration period.
   -- It is over 100 elements, so it would not be done statically anyway.
   PROCEDURE Reset_Interrupt_Descriptor_Table;

   -- Returns a value that acts as an entry for the IDT.
   FUNCTION Interrupt_Entry
     (ISR_Entry : IN address;
      Gate      : IN interrupt_descriptor_table_gate_type := interrupt_gate;
      Ring_3    : IN boolean                              := false)
      RETURN interrupt_descriptor_table_gate;

PRIVATE
   -- There are address attributes used in here outside of clauses.
   -- They're needed to configure the GDT[R] and IDTR.
   PRAGMA SPARK_Mode(off);

   -- The GDT is private, as it should not be modified, but it's still not
   -- constant, as the processor can silently modify it.
   GDT : ALIASED global_descriptor_table :=
   (
      Descriptor_Null               => -- Never accessed by the CPU.
      (
         End_Address_Low            => 0,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => false,
            Direction_Or_Conforming => false,
            Executable              => false,
            Descriptor_Type         => false,
            Privilege_Ring          => 0,
            Present                 => false
         ),
         End_Address_High           => 0,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => false,
            Legacy_Descriptor_Size  => false,
            Granularity             => false
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Reserved           => -- A gap between the descriptors.
      (
         End_Address_Low            => 0,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => false,
            Direction_Or_Conforming => false,
            Executable              => false,
            Descriptor_Type         => false,
            Privilege_Ring          => 0,
            Present                 => false
         ),
         End_Address_High           => 0,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => false,
            Legacy_Descriptor_Size  => false,
            Granularity             => false
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Kernel_CS          => -- Ring 0.
      (
         End_Address_Low            => 2**16 - 1,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => true,
            Direction_Or_Conforming => false,
            Executable              => true,
            Descriptor_Type         => true,
            Privilege_Ring          => 0,
            Present                 => true
         ),
         End_Address_High           => 2**04 - 1,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Kernel_DS          => -- Ring 0.
      (
         End_Address_Low            => 2**16 - 1,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => true,
            Direction_Or_Conforming => false,
            Executable              => false,
            Descriptor_Type         => true,
            Privilege_Ring          => 0,
            Present                 => true
         ),
         End_Address_High           => 2**04 - 1,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_User_CS            => -- Ring 3.
      (
         End_Address_Low            => 2**16 - 1,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => true,
            Direction_Or_Conforming => false,
            Executable              => true,
            Descriptor_Type         => true,
            Privilege_Ring          => 3,
            Present                 => true
         ),
         End_Address_High           => 2**04 - 1,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_User_DS            => -- Ring 3.
      (
         End_Address_Low            => 2**16 - 1,
         Base_Address_Low           => 0,
         Base_Address_Middle        => 0,
         Segment_Access             =>
         (
            Accessed                => false,
            Readable_Or_Writable    => true,
            Direction_Or_Conforming => false,
            Executable              => false,
            Descriptor_Type         => true,
            Privilege_Ring          => 3,
            Present                 => true
         ),
         End_Address_High           => 2**04 - 1,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Base_Address_High          => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_TSS64              => (Descriptor_TSS =>
      (
         End_Address_Low            => 2**16 - 1, -- TODO: Confused on this.
         Base_Address_Low           => TSS'address AND 2**16 - 1,
         Base_Address_Middle        => Shift_Right(TSS'address, 16)
                                          AND 2**08 - 1,
         Segment_Access             => -- Must be 0x89.
         (
            Accessed                => true, -- Must be true.
            Readable_Or_Writable    => false, -- Represents the "busy" bit.
            Direction_Or_Conforming => false,
            Executable              => true, -- Must be true.
            Descriptor_Type         => false,
            Privilege_Ring          => 0,
            Present                 => true
         ),
         End_Address_High           => 0,
         -- OSDev Wiki says this must be 0x40, but 0x20 works too?
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => false, -- False even for a 64-bit TSS.
            Legacy_Descriptor_Size  => false,
            Granularity             => false
         ),
         Base_Address_High          => Shift_Right(TSS'address, 24)
                                          AND 2**08 - 1
         ), -- Long mode TSS extension fields begin here.
         Base_Address_Extended      => Shift_Right(TSS'address, 32)
                                          AND 2**32 - 1,
         Reserved_1                 => 0,
         Zeroed                     => 0,
         Reserved_2                 => 0
      )
   )
   WITH
      Volatile => true; -- For the "accessed" bit, which the CPU can change.

   -- Goes inside the register for the GDT. Note that the size of the table is
   -- static and declared in the type itself, so this does not need to be
   -- calculated again and can be constant.
   GDTR : ALIASED CONSTANT descriptor_table_register :=
   (
      Table_Size   => (global_descriptor_table'size / 8) - 1,
      Base_Address => GDT'address
   );

   -- Same as the above, but for the IDT instead.
   IDTR : ALIASED CONSTANT descriptor_table_register :=
   (
      Table_Size   => (interrupt_descriptor_table'size / 8) - 1,
      Base_Address => IDT'address
   );

END HAVK_Kernel.Descriptors;
