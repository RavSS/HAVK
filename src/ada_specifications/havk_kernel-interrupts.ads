-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-interrupts.ads                             --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

PACKAGE HAVK_Kernel.Interrupts
IS
   -- Hopefully I have defined the 64-bit interrupt stack frame properly.
   -- Check Intel's ASD Manual (6-12 Vol. 1) for a description
   -- of the IA-32 interrupt stack frame or the 32-bit version of HAVK.
   -- So far, there does not seem to be any changes to its definition.
   -- Also check Figure 8-13. in AMD's AMD64 manual. The error code
   -- (if relevant) will be passed as a separate parameter. Essentially,
   -- the values change from double-words (4 bytes) to quad-words (8 bytes).
   TYPE interrupt IS RECORD
      RIP    : address;
      CS     : number;
      RFLAGS : number;
      RSP    : number;
      SS     : number;
   END RECORD
   WITH
      Alignment =>  128, -- The stack frame needs 16-byte alignment.
      Pack      => true;

   -- The processor passes a pointer to the interrupt stack frame and GCC
   -- demands that we account for it regardless of interaction. GCC also does
   -- not allow for this to be replaced with an anonymous access parameter.
   TYPE access_interrupt IS NOT NULL ACCESS interrupt;

   -- https://wiki.osdev.org/Global_Descriptor_Table
   TYPE GDT_access IS RECORD
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
   FOR GDT_access USE RECORD
      Accessed                    AT 0 RANGE 0 .. 0;
      Readable_Or_Writable        AT 0 RANGE 1 .. 1;
      Direction_Or_Conforming     AT 0 RANGE 2 .. 2;
      Executable                  AT 0 RANGE 3 .. 3;
      Descriptor_Type             AT 0 RANGE 4 .. 4;
      Privilege_Ring              AT 0 RANGE 5 .. 6;
      Present                     AT 0 RANGE 7 .. 7;
   END RECORD;

   TYPE GDT_flags IS RECORD
      -- This is actually an "Available" field, but we shall ignore it.
      Zeroed                  : number RANGE 0 .. 0;
      -- Clearly must be set to one as this is for x86-64.
      Long_Descriptor_Size    : boolean;
      -- 0 for 16-bit protected mode size, 1 for 32-bit protected mode size.
      -- This must be zero for a long mode descriptor.
      Legacy_Descriptor_Size  : boolean;
      -- 0 for byte granularity, 1 for page (4 KiB) granularity.
      -- This applies to the end address value only, I think...
      Granularity             : boolean;
   END RECORD;
   FOR GDT_flags USE RECORD
      Zeroed                      AT 0 RANGE 0 .. 0;
      Long_Descriptor_Size        AT 0 RANGE 1 .. 1;
      Legacy_Descriptor_Size      AT 0 RANGE 2 .. 2;
      Granularity                 AT 0 RANGE 3 .. 3;
   END RECORD;

   TYPE GDT_entry IS RECORD
      -- The lower 16 bits of the segment's maximum address range.
      End_Address_Low         : number RANGE 0 .. 16#FFFF#;
      -- The lower 16 bits of where the segment's address begins.
      Start_Address_Low       : number RANGE 0 .. 16#FFFF#;
      -- The middle 8 bits of the segment's starting address.
      Start_Address_Middle    : number RANGE 0 .. 16#00FF#;
      -- Information for the descriptor. See "GDT_access" for more.
      Segment_Access          : GDT_access;
      -- The final 4 bits of the segment's end address.
      End_Address_High        : number RANGE 0 .. 16#000F#;
      -- Descriptor entry flags. See the "GDT_flags" record for more.
      Flags                   : GDT_flags;
      -- End 8 bits of the 32-bit starting address for the segment.
      Start_Address_High      : number RANGE 0 .. 16#00FF#;
   END RECORD;
   FOR GDT_entry USE RECORD
      End_Address_Low             AT 0 RANGE 0 .. 15;
      Start_Address_Low           AT 2 RANGE 0 .. 15;
      Start_Address_Middle        AT 4 RANGE 0 .. 07;
      Segment_Access              AT 5 RANGE 0 .. 07;
      End_Address_High            AT 6 RANGE 0 .. 03;
      Flags                       AT 6 RANGE 4 .. 07;
      Start_Address_High          AT 7 RANGE 0 .. 07;
   END RECORD;

   -- The IDT gate entry differs a bit from x86.
   -- For x86-64, there seems to be a few more fields.
   -- https://wiki.osdev.org/Interrupt_Descriptor_Table#Structure_AMD64
   -- The call gates are not mentioned on the OSDev Wiki IDT page.
   -- I dug them out of AMD's AMD64 Architecture Programmer's Manual.
   -- It also doesn't mention 64-bit redefines and 0x4 to 0x7 gates becoming
   -- unusable in long mode as of 2019-05-18. Maybe update the wiki-page?
   TYPE IDT_gate_type IS
     (call_64_bit,
      interrupt_64_bit,
      trap_64_bit)
   WITH
      Size => 4;
   FOR IDT_gate_type USE
     (call_64_bit      => 12,
      interrupt_64_bit => 14,
      trap_64_bit      => 15);

   TYPE IDT_attributes IS RECORD
      -- Specifies the type of gate for the interrupt.
      Gate               : IDT_gate_type       := interrupt_64_bit;
      -- If not a storage segment and an interrupt or trap gate, then
      -- this must be set to zero. I am going to set it to that by default.
      Storage_Segment    : boolean             := false;
      -- Minimum privilege level for the descriptor that is trying to
      -- call the interrupt. Useful so userspace doesn't mess with
      -- kernel space interrupts for hardware etc.
      DPL                : number RANGE 0 .. 3 := 0;
      -- Whether the interrupt is currently active. Set to zero by default,
      -- so all the blank interrupts are not set to present.
      Present            : boolean             := false;
   END RECORD;
   FOR IDT_attributes USE RECORD
      Gate                   AT 0 RANGE 0 .. 3;
      Storage_Segment        AT 0 RANGE 4 .. 4;
      DPL                    AT 0 RANGE 5 .. 6;
      Present                AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Every field has been defaulted so it's easier to initialize an IDT array.
   TYPE IDT_gate IS RECORD
      -- The lower 16-bits of the 64-bit address belonging to the location
      -- of the ISR handler which handles this interrupt.
      ISR_Address_Low    : number  RANGE 0 .. 16#0000FFFF# := 0;
      -- Points to a code segment in our GDT depending on which ring can
      -- raise the interrupt.
      CS_Selector        : number  RANGE 0 .. 16#0000FFFF# := 0;
      -- Only the first 3-bits do something, and they point to the
      -- the interrupt stack table offset (IST). The remaining 6 bits must
      -- be blank/zeroed. Disabled by default.
      IST_Offset         : number  RANGE 0 .. 16#000000FF# := 0;
      -- Details the properties and attributes of the IDT gate.
      -- See the "IDT_attributes" record for more information.
      Type_Attributes    : IDT_attributes;
      -- The 16-bits that reach the center of the 64-bit address for the
      -- ISR handler.
      ISR_Address_Middle : number  RANGE 0 .. 16#0000FFFF# := 0;
      -- The final 32-bits that complete the 64-bit address.
      ISR_Address_High   : number  RANGE 0 .. 16#FFFFFFFF# := 0;
      -- 32-bits of zeroes only and nothing else.
      Zeroed             : number  RANGE 0 .. 16#FFFFFFFF# := 0;
   END RECORD;
   FOR IDT_gate USE RECORD
      ISR_Address_Low     AT 00 RANGE 0 .. 15;
      CS_Selector         AT 02 RANGE 0 .. 15;
      IST_Offset          AT 04 RANGE 0 .. 07;
      Type_Attributes     AT 05 RANGE 0 .. 07;
      ISR_Address_Middle  AT 06 RANGE 0 .. 15;
      ISR_Address_High    AT 08 RANGE 0 .. 31;
      Zeroed              AT 12 RANGE 0 .. 31;
   END RECORD;

   TYPE TSS_structure IS RECORD
      Reserved_1  : number RANGE 0 .. 16#FFFFFFFF# := 000;
      RSP_Ring_0  : address                        := 000;
      RSP_Ring_1  : address                        := 000;
      RSP_Ring_2  : address                        := 000;
      Reserved_2  : number                         := 000;
      IST_1       : address                        := 000;
      IST_2       : address                        := 000;
      IST_3       : address                        := 000;
      IST_4       : address                        := 000;
      IST_5       : address                        := 000;
      IST_6       : address                        := 000;
      IST_7       : address                        := 000;
      Reserved_3  : number                         := 000;
      Reserved_4  : number RANGE 0 .. 16#0000FFFF# := 000;
      -- IOPB disabled by default (byte size of TSS).
      IOPB        : number RANGE 0 .. 16#0000FFFF# := 104;
   END RECORD;
   FOR TSS_structure USE RECORD
      Reserved_1    AT 000 RANGE 0 .. 31;
      RSP_Ring_0    AT 004 RANGE 0 .. 63;
      RSP_Ring_1    AT 012 RANGE 0 .. 63;
      RSP_Ring_2    AT 020 RANGE 0 .. 63;
      Reserved_2    AT 028 RANGE 0 .. 63;
      IST_1         AT 036 RANGE 0 .. 63;
      IST_2         AT 044 RANGE 0 .. 63;
      IST_3         AT 052 RANGE 0 .. 63;
      IST_4         AT 060 RANGE 0 .. 63;
      IST_5         AT 068 RANGE 0 .. 63;
      IST_6         AT 076 RANGE 0 .. 63;
      IST_7         AT 084 RANGE 0 .. 63;
      Reserved_3    AT 092 RANGE 0 .. 63;
      Reserved_4    AT 100 RANGE 0 .. 15;
      IOPB          AT 102 RANGE 0 .. 15;
   END RECORD;

   TYPE GDT_entry_TSS64 IS RECORD
      Descriptor_TSS         : GDT_entry;
      Start_Address_Extended : number RANGE 000 .. 16#FFFFFFFF#;
      Reserved_1             : number RANGE 000 .. 16#000000FF#;
      Zeroed                 : number RANGE 000 .. 16#0000000F#;
      Reserved_2             : number RANGE 000 .. 16#000FFFFF#;
   END RECORD;
   FOR GDT_entry_TSS64 USE RECORD
      Descriptor_TSS             AT 0 RANGE 000 .. 063;
      Start_Address_Extended     AT 0 RANGE 064 .. 095;
      Reserved_1                 AT 0 RANGE 096 .. 103;
      Zeroed                     AT 0 RANGE 104 .. 107;
      Reserved_2                 AT 0 RANGE 108 .. 127;
   END RECORD;

   -- Increase the amount of GDT entries if need be. Bits 2 through 15 indicate
   -- the segment selector/descriptor index that I have indicated below whereas
   -- 0 to 1 indicate the RPL (requested privilege level). The order is very
   -- important.
   TYPE GDT_entries IS RECORD
      -- The null descriptor, which is never touched by the processor.
      Descriptor_Null      : GDT_entry;
      -- The CS descriptor for ring 0 code. The index is 0x08.
      Descriptor_Kernel_CS : GDT_entry;
      -- The DS descriptor for ring 0 data. The index is 0x10.
      Descriptor_Kernel_DS : GDT_entry;
      -- The reserved descriptor for the system call instruction calculations.
      -- The index is 0x18.
      Descriptor_Reserved  : GDT_entry;
      -- The DS descriptor for ring 3 data. The index is 0x20.
      Descriptor_User_DS   : GDT_entry;
      -- The CS descriptor for ring 3 code. The index is 0x28.
      Descriptor_User_CS   : GDT_entry;
      -- The TSS descriptor for software multi-tasking. The index is 0x30.
      Descriptor_TSS64     : GDT_entry_TSS64;
   END RECORD;
   FOR GDT_entries USE RECORD
      Descriptor_Null      AT 00 RANGE 0 .. 063;
      Descriptor_Kernel_CS AT 08 RANGE 0 .. 063;
      Descriptor_Kernel_DS AT 16 RANGE 0 .. 063;
      Descriptor_Reserved  AT 24 RANGE 0 .. 063;
      Descriptor_User_DS   AT 32 RANGE 0 .. 063;
      Descriptor_User_CS   AT 40 RANGE 0 .. 063;
      Descriptor_TSS64     AT 48 RANGE 0 .. 127;
   END RECORD;

   TYPE descriptor_table IS RECORD
      Table_Size    : number RANGE 0 .. 16#FFFF#;
      Start_Address : address;
   END RECORD;
   FOR descriptor_table USE RECORD
      Table_Size        AT 0 RANGE 0 .. 15;
      Start_Address     AT 2 RANGE 0 .. 63;
   END RECORD;

   -- The amount of interrupts a logical core can handle.
   TYPE IDT_gates IS ARRAY(number RANGE 0 .. 255) OF IDT_gate
   WITH
      Component_Size => 128,
      Pack           => true;

   -- Sets up the global descriptor table.
   PROCEDURE Prepare_GDT;

   -- Fills in an IDT entry with specific settings.
   PROCEDURE Setup_Interrupt
     (Index          : IN number;
      ISR            : IN address;
      Exception_Type : IN IDT_gate_type;
      Callable_DPL   : IN number)
   WITH
      Pre => Index <= IDT_gates'last AND THEN Callable_DPL <= 3;

   -- Sets up the interrupt descriptor table.
   PROCEDURE Prepare_IDT;

   -- TODO: Store IRQ 0 interrupts here temporarily in a counter.
   -- No idea what the timer's frequency is, so I can't exactly
   -- count seconds right now.
   Ticker : number := 0;

   -- Declare the tables.
   IDT    : ALIASED IDT_gates;
   TSS    : ALIASED TSS_structure; -- TODO: Set the kernel stacks later.
PRIVATE
   -- There are address attributes used in here outside of clauses. They're
   -- needed to configure the GDT.
   -- TODO: Move them (only) to `Prepare_GDT` and make them null here.
   PRAGMA SPARK_Mode(off);

   TSS_address : CONSTANT number     := Address_Value(TSS'address);
   GDT         : ALIASED GDT_entries :=
   (
      Descriptor_Null               => -- Never accessed by the CPU.
      (
         End_Address_Low            => 0,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Reserved           => -- A gap between the descriptors.
      (
         End_Address_Low            => 0,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Kernel_CS          => -- Ring 0.
      (
         End_Address_Low            => 16#FFFF#,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         End_Address_High           => 16#F#,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_Kernel_DS          => -- Ring 0.
      (
         End_Address_Low            => 16#FFFF#,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         End_Address_High           => 16#F#,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_User_CS            => -- Ring 3.
      (
         End_Address_Low            => 16#FFFF#,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         End_Address_High           => 16#F#,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_User_DS            => -- Ring 3.
      (
         End_Address_Low            => 16#FFFF#,
         Start_Address_Low          => 0,
         Start_Address_Middle       => 0,
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
         End_Address_High           => 16#F#,
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true,
            Legacy_Descriptor_Size  => false,
            Granularity             => true
         ),
         Start_Address_High         => 0
      ),
      -------------------------------------------------------------------------
      Descriptor_TSS64              => (Descriptor_TSS =>
      (
         End_Address_Low            => 16#FFFF#, -- TODO: Confused on this.
         Start_Address_Low          => TSS_address AND 16#FFFF#,
         Start_Address_Middle       => Shift_Right(TSS_address, 16) AND 16#FF#,
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
         Start_Address_High         => Shift_Right(TSS_address, 24) AND
                                          16#FF#),
         Start_Address_Extended     => Shift_Right(TSS_address, 32) AND
                                          16#FFFFFFFF#,
         Reserved_1                 => 0,
         Zeroed                     => 0,
         Reserved_2                 => 0
      )
   )
   WITH
      Volatile => true; -- For the "accessed" bit, which the CPU can change.
END HAVK_Kernel.Interrupts;