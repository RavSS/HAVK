WITH
   HAVK_Kernel.Intrinsics,
   System;
USE
   HAVK_Kernel.Intrinsics;

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
      RIP    : num;
      CS     : num;
      RFLAGS : num;
      RSP    : num;
      SS     : num;
   END RECORD
   WITH
      Alignment => 16, -- I believe the stack frame needs 16 byte alignment?
      Pack      => true;

   -- GCC wants the handlers to take in a pointer
   -- to the interrupt stack frame as a parameter.
   TYPE access_interrupt        IS ACCESS CONSTANT interrupt;

   -- https://wiki.osdev.org/Global_Descriptor_Table
   TYPE GDT_access              IS RECORD
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
      -- There's only 3 privilege rings for IA-32/AMD64, so two bits
      -- are required to store it.
      Privilege_Ring          : num RANGE 0 .. 3;
      -- This must always be 1, or else the selector doesn't exist.
      -- For the GDT entries, I will only have as many as I need (unlike
      -- the IDT gates, where I fill the entire range in).
      Present                 : boolean;
   END RECORD;
   FOR GDT_access               USE RECORD
      Accessed                  AT 0 RANGE 0 .. 0;
      Readable_Or_Writable      AT 0 RANGE 1 .. 1;
      Direction_Or_Conforming   AT 0 RANGE 2 .. 2;
      Executable                AT 0 RANGE 3 .. 3;
      Descriptor_Type           AT 0 RANGE 4 .. 4;
      Privilege_Ring            AT 0 RANGE 5 .. 6;
      Present                   AT 0 RANGE 7 .. 7;
   END RECORD;

   TYPE GDT_flags               IS RECORD
      Zeroed                  : num  RANGE 0 .. 0;
      -- Clearly must be set to one as this is for x86-64.
      Long_Descriptor_Size    : boolean;
      -- 0 for 16-bit protected mode size, 1 for 32-bit protected mode size.
      -- This must be zero for a long mode descriptor.
      Legacy_Descriptor_Size  : boolean;
      -- 0 for byte granularity, 1 for page (4 KiB) granularity.
      -- This applies to the end address value only, I think...
      Granularity             : boolean;
   END RECORD;
   FOR GDT_flags                USE RECORD
      Zeroed                    AT 0 RANGE 0 .. 0;
      Long_Descriptor_Size      AT 0 RANGE 1 .. 1;
      Legacy_Descriptor_Size    AT 0 RANGE 2 .. 2;
      Granularity               AT 0 RANGE 3 .. 3;
   END RECORD;

   TYPE GDT_entry               IS RECORD
      -- The lower 16 bits of the segment's maximum address range.
      End_Address_Low         : num  RANGE 0 .. 16#FFFF#;
      -- The lower 16 bits of where the segment's address begins.
      Start_Address_Low       : num  RANGE 0 .. 16#FFFF#;
      -- The middle 8 bits of the segment's starting address.
      Start_Address_Middle    : num  RANGE 0 ..   16#FF#;
      -- Information for the descriptor. See "GDT_access" for more.
      Segment_Access          : GDT_access;
      -- The final 4 bits of the segment's end address.
      End_Address_High        : num  RANGE 0 ..    16#F#;
      -- Descriptor entry flags. See the "GDT_flags" record for more.
      Flags                   : GDT_flags;
      -- End 8 bits of the 32-bit starting address for the segment.
      Start_Address_High      : num  RANGE 0 ..   16#FF#;
   END RECORD;
   FOR GDT_entry                USE RECORD
      End_Address_Low           AT 0 RANGE 0 .. 15;
      Start_Address_Low         AT 2 RANGE 0 .. 15;
      Start_Address_Middle      AT 4 RANGE 0 .. 7;
      Segment_Access            AT 5 RANGE 0 .. 7;
      End_Address_High          AT 6 RANGE 0 .. 3;
      Flags                     AT 6 RANGE 4 .. 7;
      Start_Address_High        AT 7 RANGE 0 .. 7;
   END RECORD;

   -- The IDT gate entry differs a bit from x86.
   -- For x86-64, there seems to be a few more fields.
   -- https://wiki.osdev.org/Interrupt_Descriptor_Table#Structure_AMD64

   -- The call gates are not mentioned on the OSDev Wiki IDT page.
   -- I dug them out of AMD's AMD64 Architecture Programmer's Manual.
   -- It also doesn't mention 64-bit redefines and 0x4 to 0x7 gates becoming
   -- unusable in long mode as of 2019-05-18. Maybe update the wiki-page?
   TYPE IDT_gate_type IS(          -- Types of IDT gates:
   -- call_16_bit,                 -- 80286 16-Bit      Call Gate   (Illegal).
   -- task_32_bit,                 -- 80386 32 Bit      Task Gate   (Illegal).
   -- interrupt_16_bit,            -- 80286 16-Bit Interrupt Gate   (Illegal).
   -- trap_16_bit,                 -- 80286 16-Bit      Trap Gate   (Illegal).
      call_64_bit,                 -- 80386 32-Bit      Call Gate (Redefined).
      interrupt_64_bit,            -- 80386 32-Bit Interrupt Gate (Redefined).
      trap_64_bit);                -- 80386 32-Bit      Trap Gate (Redefined).
   FOR IDT_gate_type USE(          -- Commonly a 64-bit interrupt is utilized.
   -- call_16_bit         =>  4,   -- 0b0100, 0x4,  4,   Illegal in Long Mode.
   -- task_32_bit         =>  5,   -- 0b0101, 0x5,  5,   Illegal in Long Mode.
   -- interrupt_16_bit    =>  6,   -- 0b0110, 0x6,  6,   Illegal in Long Mode.
   -- trap_16_bit         =>  7,   -- 0b0111, 0x7,  7,   Illegal in Long Mode.
      call_64_bit         => 12,   -- 0b1100, 0xC, 12, Redefined in Long Mode.
      interrupt_64_bit    => 14,   -- 0b1110, 0xE, 14, Redefined in Long Mode.
      trap_64_bit         => 15);  -- 0b1111, 0xF, 15, Redefined in Long Mode.
   FOR IDT_gate_type'size USE 4;   --             4 bits define the gate type.

   TYPE IDT_attributes     IS RECORD
      -- Specifies the type of gate for the interrupt.
      Gate               : IDT_gate_type;
      -- If not a storage segment and an interrupt or trap gate, then
      -- this must be set to zero. I am going to set it to that by default.
      Storage_Segment    : boolean;
      -- Minimum privilege level for the descriptor that is trying to
      -- call the interrupt. Useful so userspace doesn't mess with
      -- kernel space interrupts for hardware etc.
      DPL                : num  RANGE  0 .. 3;
      -- Whether the interrupt is currently active. Set to zero by default,
      -- so all the blank interrupts are not set to present.
      Present            : boolean;
   END RECORD;
   FOR IDT_attributes      USE RECORD
      Gate                 AT 0 RANGE 0 .. 3;
      Storage_Segment      AT 0 RANGE 4 .. 4;
      DPL                  AT 0 RANGE 5 .. 6;
      Present              AT 0 RANGE 7 .. 7;
   END RECORD;

   -- Every field has been defaulted so it's easier to initialize an IDT array.
   TYPE IDT_gate           IS RECORD
      -- The lower 16 bits of the 64-bit address belonging to the location
      -- of the ISR handler which handles this interrupt.
      ISR_Address_Low    : num  RANGE 0 ..     16#FFFF#;
      -- Points to the code segment in our GDT. Not sure which DPL CS, though.
      CS_Selector        : num  RANGE 0 ..     16#FFFF#;
      -- Only the first 3 bits do something, and they point to the
      -- the interrupt stack table offset (IST). The remaining 6 bits must
      -- be blank/zeroed. Disabled by default.
      IST_Offset         : num  RANGE 0 ..       16#FF#;
      -- Details the properties and attributes of the IDT gate.
      -- See the "IDT_attributes" record for more information.
      Type_Attributes    : IDT_attributes;
      -- The 16 bits that reach the center of the 64-bit address for the
      -- ISR handler.
      ISR_Address_Middle : num  RANGE 0 ..     16#FFFF#;
      -- The final 32 bits that complete the 64-bit address.
      ISR_Address_High   : num  RANGE 0 .. 16#FFFFFFFF#;
      -- 32 bits of zeroes only and nothing else.
      Zeroed             : num  RANGE 0 .. 16#FFFFFFFF#;
   END RECORD;
   FOR IDT_gate           USE RECORD
      ISR_Address_Low     AT  0 RANGE 0 .. 15;
      CS_Selector         AT  2 RANGE 0 .. 15;
      IST_Offset          AT  4 RANGE 0 ..  7;
      Type_Attributes     AT  5 RANGE 0 ..  7;
      ISR_Address_Middle  AT  6 RANGE 0 .. 15;
      ISR_Address_High    AT  8 RANGE 0 .. 31;
      Zeroed              AT 12 RANGE 0 .. 31;
   END RECORD;

   TYPE TSS_structure    IS RECORD
      Reserved_1  : num  RANGE 0 .. 16#FFFFFFFF# :=   0;
      RSP_Ring_0  : num  RANGE 0 ..     num'last :=   0; -- Ring 0 stack.
      RSP_Ring_1  : num  RANGE 0 ..     num'last :=   0; -- Ring 1 stack.
      RSP_Ring_2  : num  RANGE 0 ..     num'last :=   0; -- Ring 2 stack.
      Reserved_2  : num  RANGE 0 ..     num'last :=   0;
      IST_1       : num  RANGE 0 ..     num'last :=   0;
      IST_2       : num  RANGE 0 ..     num'last :=   0;
      IST_3       : num  RANGE 0 ..     num'last :=   0;
      IST_4       : num  RANGE 0 ..     num'last :=   0;
      IST_5       : num  RANGE 0 ..     num'last :=   0;
      IST_6       : num  RANGE 0 ..     num'last :=   0;
      IST_7       : num  RANGE 0 ..     num'last :=   0;
      Reserved_3  : num  RANGE 0 ..     num'last :=   0;
      Reserved_4  : num  RANGE 0 ..     16#FFFF# :=   0;
      -- IOPB disabled by default (byte size of TSS).
      IOPB        : num  RANGE 0 ..     16#FFFF# := 104;
   END RECORD;
   FOR TSS_structure           USE RECORD
      Reserved_1 AT   0  RANGE 0 .. 31;
      RSP_Ring_0 AT   4  RANGE 0 .. 63;
      RSP_Ring_1 AT  12  RANGE 0 .. 63;
      RSP_Ring_2 AT  20  RANGE 0 .. 63;
      Reserved_2 AT  28  RANGE 0 .. 63;
      IST_1      AT  36  RANGE 0 .. 63;
      IST_2      AT  44  RANGE 0 .. 63;
      IST_3      AT  52  RANGE 0 .. 63;
      IST_4      AT  60  RANGE 0 .. 63;
      IST_5      AT  68  RANGE 0 .. 63;
      IST_6      AT  76  RANGE 0 .. 63;
      IST_7      AT  84  RANGE 0 .. 63;
      Reserved_3 AT  92  RANGE 0 .. 63;
      Reserved_4 AT 100  RANGE 0 .. 15;
      IOPB       AT 102  RANGE 0 .. 15;
   END RECORD;

   TYPE GDT_entry_TSS64           IS RECORD
      Descriptor_TSS            : GDT_entry;
      Start_Address_Extended    : num RANGE 0 .. 16#FFFFFFFF#;
      Reserved_1                : num RANGE 0 ..       16#FF#;
      Zeroed                    : num RANGE 0 ..        16#F#;
      Reserved_2                : num RANGE 0 ..    16#FFFFF#;
   END RECORD;
   FOR GDT_entry_TSS64            USE RECORD
      Descriptor_TSS              AT 0 RANGE   0 ..  63; -- 64 bits.
      Start_Address_Extended      AT 0 RANGE  64 ..  95; -- 32 bits.
      Reserved_1                  AT 0 RANGE  96 .. 103; --  8 bits.
      Zeroed                      AT 0 RANGE 104 .. 107; --  4 bits.
      Reserved_2                  AT 0 RANGE 108 .. 127; -- 20 bits.
   END RECORD;

   -- Increase the amount of GDT entries if need be.
   -- Tip: Bits 3 through 15 indicate the segment selector/descriptor index.
   TYPE GDT_entries               IS RECORD
      Descriptor_Null           : GDT_entry;       --  0x4 =    100.
      Descriptor_Kernel_CS      : GDT_entry;       --  0x8 =   1000.
      Descriptor_Kernel_DS      : GDT_entry;       -- 0x10 =  10000.
      Descriptor_User_CS        : GDT_entry;       -- 0x20 = 100000.
      Descriptor_User_DS        : GDT_entry;       -- 0x24 = 100100.
      Descriptor_TSS64          : GDT_entry_TSS64; -- 0x28 = 101000.
   END RECORD;
   FOR GDT_entries                USE RECORD
      Descriptor_Null             AT  0 RANGE 0 .. 63;
      Descriptor_Kernel_CS        AT  8 RANGE 0 .. 63;
      Descriptor_Kernel_DS        AT 16 RANGE 0 .. 63;
      Descriptor_User_CS          AT 24 RANGE 0 .. 63;
      Descriptor_User_DS          AT 32 RANGE 0 .. 63;
      Descriptor_TSS64            AT 40 RANGE 0 .. 127;
   END RECORD;

   TYPE descriptor_table          IS RECORD
      Table_Size                : num RANGE 0 .. 16#FFFF#;
      Start_Address             : System.Address;
   END RECORD;
   FOR descriptor_table           USE RECORD
      Table_Size                  AT 0 RANGE 0 .. 15;
      Start_Address               AT 2 RANGE 0 .. 63;
   END RECORD;

   -- Increase this later on if need be.
   TYPE IDT_gates IS ARRAY(num RANGE 0 .. 47) OF IDT_gate
   WITH
      Component_Size => 128,
      Pack           => true;

   PROCEDURE Prepare_GDT;

   PROCEDURE Setup_Interrupt(
      Index          : IN num;
      ISR            : IN System.Address;
      Exception_Type : IN IDT_gate_type;
      Ring           : IN num);

   PROCEDURE Prepare_IDT;

   -- Declare the tables.
   IDT         : IDT_gates; -- Fill this in later to avoid elaboration time.
   TSS         : TSS_structure; -- Set the kernel stacks later.
   TSS_address : CONSTANT num := Address_To_num(TSS'address);
   GDT         : GDT_entries :=
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
         End_Address_Low            => TSS'size, -- Must be 104 bytes.
         Start_Address_Low          => TSS_address AND 16#FFFF#,
         Start_Address_Middle       => SHR(TSS_address, 16),
         Segment_Access             => -- Must be 0x89.
         (
            Accessed                => true,
            Readable_Or_Writable    => false,
            Direction_Or_Conforming => false,
            Executable              => true,
            Descriptor_Type         => false,
            Privilege_Ring          => 0,
            Present                 => true
         ),
         End_Address_High           => 0,
         -- OSDev Wiki says this must be 0x40, but 0x20 works too?
         Flags                      =>
         (
            Zeroed                  => 0,
            Long_Descriptor_Size    => true, -- TODO: Is this actually valid?
            Legacy_Descriptor_Size  => false,
            Granularity             => false
         ),
         Start_Address_High         => SHR(TSS_address, 24)),
         -- 64-bit extension starts here:
         Start_Address_Extended     => SHR(TSS_address, 32),
         Reserved_1                 => 0,
         Zeroed                     => 0,
         Reserved_2                 => 0
      )
   )
   WITH
      Volatile => true; -- For the "accessed" bit, which the CPU can change.

   -- TODO: Store IRQ 0 interrupts here temporarily.
   -- No idea what the timer's frequency is, so I can't exactly
   -- count seconds right now.
   Ticker : num := 0
   WITH
      Volatile => true;
END HAVK_Kernel.Interrupts;
