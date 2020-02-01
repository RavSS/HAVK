-------------------------------------------------------------------------------
-- Program         -- The HAVK Operating System                              --
-- Filename        -- havk_kernel-intrinsics-cpuid.ads                       --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2019-2020                --
-------------------------------------------------------------------------------

-- This package details CPU support for features using the `CPUID` instruction.
-- There's a package called "intel_cpu.ads" that is detailed in an old version
-- of GCC's documentation, but it seems very out-of-date and limited. This
-- package is mostly independent, meaning it can even be used for x86 in
-- protected mode. All it requires is the `CPUID` instruction's information;
-- thus, making this relatively portable to other x86 Ada projects!
PACKAGE HAVK_Kernel.Intrinsics.CPUID
IS
   -- `CPUID` takes in an argument inside the EAX register called a "leaf" and
   -- sometimes another argument inside the ECX register. It then returns a
   -- variety of information in EAX, EBX, ECX, and RDX depending on the
   -- parameters passed. Note that the upper bits of the registers e.g. RAX
   -- are all zeroed out, so they do not need to be stored. If an invalid leaf
   -- is passed, then the CPU is supposed to zero out all the aforementioned
   -- registers, but I find that does not happen with QEMU, so it's unreliable.
   TYPE leaf IS
     (highest_function_parameter_leaf,
      processor_information_leaf,
      cache_and_TLB_leaf,
      processor_serial_number_leaf,
      core_and_cache_topology_leaf,
      MONITOR_and_MWAIT_leaf,
      thermal_and_power_leaf,
      extended_features_leaf,
      direct_cache_access_information_leaf,
      performance_monitoring_leaf,
      processor_topology_leaf,
      processor_state_leaf,
      L3_cache_and_resource_monitoring_leaf,
      L3_cache_and_resource_allocation_leaf,
      software_guard_extensions_leaf,
      processor_trace_leaf,
      timestamp_counter_and_core_crystal_leaf,
      processor_frequency_leaf,
      SOC_vendor_leaf,
      paging_and_address_translation_leaf,
      processor_topology_V2_leaf,
      highest_extended_function_parameter_leaf,
      extended_processor_information_leaf,
      processor_brand_string_leaf,
      L1_cache_and_TLB_leaf,
      L2_features_leaf,
      APM_information_leaf,
      address_sizes_leaf)
   WITH
      Size => 32;
   FOR leaf USE
     (highest_function_parameter_leaf          => 16#00000000#,
      processor_information_leaf               => 16#00000001#,
      cache_and_TLB_leaf                       => 16#00000002#,
      processor_serial_number_leaf             => 16#00000003#,
      core_and_cache_topology_leaf             => 16#00000004#,
      MONITOR_and_MWAIT_leaf                   => 16#00000005#,
      thermal_and_power_leaf                   => 16#00000006#,
      extended_features_leaf                   => 16#00000007#,
      direct_cache_access_information_leaf     => 16#00000009#,
      performance_monitoring_leaf              => 16#0000000A#,
      processor_topology_leaf                  => 16#0000000B#,
      processor_state_leaf                     => 16#0000000D#,
      L3_cache_and_resource_monitoring_leaf    => 16#0000000F#,
      L3_cache_and_resource_allocation_leaf    => 16#00000010#,
      software_guard_extensions_leaf           => 16#00000012#,
      processor_trace_leaf                     => 16#00000014#,
      timestamp_counter_and_core_crystal_leaf  => 16#00000015#,
      processor_frequency_leaf                 => 16#00000016#,
      SOC_vendor_leaf                          => 16#00000017#,
      paging_and_address_translation_leaf      => 16#00000018#,
      processor_topology_V2_leaf               => 16#0000001F#,
      highest_extended_function_parameter_leaf => 16#80000000#,
      extended_processor_information_leaf      => 16#80000001#,
      processor_brand_string_leaf              => 16#80000002#, -- +0 to +2.
      L1_cache_and_TLB_leaf                    => 16#80000005#,
      L2_features_leaf                         => 16#80000006#,
      APM_information_leaf                     => 16#80000007#,
      address_sizes_leaf                       => 16#80000008#);

   -- Update the below subtypes if you wish to support and acknowledge a
   -- new/larger range of CPUID functions/leaves.

   -- The range we will support for the normal/regular leaves.
   SUBTYPE normal_leaf IS leaf RANGE
      highest_function_parameter_leaf .. processor_topology_V2_leaf;

   -- The range we will support for the extended leaves.
   SUBTYPE extended_leaf IS leaf RANGE
      highest_extended_function_parameter_leaf .. address_sizes_leaf;

   -- TODO: Remove the below when the "enum_rep" attribute is fully supported.
   PRAGMA Warnings(off, "types for unchecked conversion have different sizes",
      Reason => "32-bit value properly fits into a larger 64-bit value.");
   FUNCTION Enum_Rep IS NEW Ada.Unchecked_Conversion
     (Source => leaf, Target => number);

   -- This is how my `CPUID` function in assembly will return the value. It is
   -- a 24-byte record/structure. All records that attempt to describe this
   -- returned record should keep the format of bits so it's very easy to
   -- convert in an unchecked manner. Otherwise, it would get messy.
   -- This format must be matched in the `assembly__cpuid` assembly routine.
   TYPE CPUID_return_values IS RECORD
      Passed_Leaf     : leaf;
      Passed_Argument : number RANGE 0 .. 16#FFFFFFFF#;
      EAX             : number RANGE 0 .. 16#FFFFFFFF#;
      EBX             : number RANGE 0 .. 16#FFFFFFFF#;
      ECX             : number RANGE 0 .. 16#FFFFFFFF#;
      EDX             : number RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => Assembler;
   FOR CPUID_return_values USE RECORD
      Passed_Leaf        AT 00 RANGE 0 .. 31;
      Passed_Argument    AT 04 RANGE 0 .. 31;
      EAX                AT 08 RANGE 0 .. 31;
      EBX                AT 12 RANGE 0 .. 31;
      ECX                AT 16 RANGE 0 .. 31;
      EDX                AT 20 RANGE 0 .. 31;
   END RECORD;

   -- Simply returns the max leaf value and then three strings (without any
   -- nulls) which show the CPU's ID like e.g. "GenuineIntel".
   TYPE highest_function_parameter_leaf_format IS RECORD
      Passed_Leaf     : leaf;
      Passed_Argument : number RANGE 0 .. 16#FFFFFFFF#;
      Highest_Leaf    : number RANGE 0 .. 16#FFFFFFFF#;  -- EAX.
      CPU_Identity_1  :       string(1 .. 16#00000004#); -- EBX.
      CPU_Identity_3  :       string(1 .. 16#00000004#); -- ECX.
      CPU_Identity_2  :       string(1 .. 16#00000004#); -- EDX.
   END RECORD
   WITH
      Convention => Assembler;
   FOR highest_function_parameter_leaf_format USE RECORD
      Passed_Leaf        AT 00 RANGE 0 .. 31;
      Passed_Argument    AT 04 RANGE 0 .. 31;
      Highest_Leaf       AT 08 RANGE 0 .. 31;
      CPU_Identity_1     AT 12 RANGE 0 .. 31;
      CPU_Identity_3     AT 16 RANGE 0 .. 31;
      CPU_Identity_2     AT 20 RANGE 0 .. 31;
   END RECORD;

   -- This leaf is useful for calibrating the local APIC timer. To get the
   -- current core crystal frequency, the nominal core crystal frequency must
   -- be multiplied by the core crystal ratio. The ratio can obviously be
   -- calculated with the numerator divided by the denominator. If the ratio
   -- is unable to be calculated, I suppose we can assume that the ratio is
   -- just one. If the nominal frequency isn't enumerated, then we must find
   -- some other way to calibrate the LAPIC timer. If there is no nominal
   -- crystal clock frequency available and only the ratio values are there,
   -- then you can also use the CPU frequency leaf and get the base core
   -- frequency to obtain the value instead by multiplying it by the
   -- denominator divided by the numerator. That is also consistent.
   TYPE timestamp_counter_and_core_crystal_leaf_format IS RECORD
      Passed_Leaf     : leaf;
      Passed_Argument : number RANGE 0 .. 16#FFFFFFFF#;
      -- The denominator of the timestamp counter or core crystal clock.
      -- This is presumably never zero, as that can cause a zero division.
      Denominator     : number RANGE 0 .. 16#FFFFFFFF#; -- EAX.
      -- The numerator of the timestamp counter or core crystal clock.
      -- When zero, the core crystal clock's ratio isn't enumerated.
      Numerator       : number RANGE 0 .. 16#FFFFFFFF#; -- EBX.
      -- The nominal frequency of the timestamp counter or core crystal clock.
      -- When zero, the nominal frequency isn't enumerated.
      Frequency_Hz    : number RANGE 0 .. 16#FFFFFFFF#; -- ECX.
      Reserved        : number RANGE 0 .. 16#FFFFFFFF#; -- EDX.
   END RECORD
   WITH
      Convention => Assembler;
   FOR timestamp_counter_and_core_crystal_leaf_format USE RECORD
      Passed_Leaf        AT 00 RANGE 0 .. 31;
      Passed_Argument    AT 04 RANGE 0 .. 31;
      Denominator        AT 08 RANGE 0 .. 31;
      Numerator          AT 12 RANGE 0 .. 31;
      Frequency_Hz       AT 16 RANGE 0 .. 31;
      Reserved           AT 20 RANGE 0 .. 31;
   END RECORD;

   -- Shows which frequencies the processor's cores run at. Note that these are
   -- likely not to be actual values and instead specification values. They can
   -- still be used for some calculations and especially for displaying them to
   -- the user. There's also a chance that they (some or all) can be zero.
   TYPE processor_frequency_leaf_format IS RECORD
      Passed_Leaf        : leaf;
      Passed_Argument    : number RANGE 0 .. 16#FFFFFFFF#;
      -- The processor cores' base frequency.
      Base_Frequency_MHz : number RANGE 0 .. 16#0000FFFF#;
      Reserved_1         : number RANGE 0 .. 16#0000FFFF#;
      -- The processor cores' maximum frequency.
      Max_Frequency_MHz  : number RANGE 0 .. 16#0000FFFF#;
      Reserved_2         : number RANGE 0 .. 16#0000FFFF#;
      -- The bus frequency (reference only).
      Bus_Frequency_MHz  : number RANGE 0 .. 16#0000FFFF#;
      Reserved_3         : number RANGE 0 .. 16#0000FFFF#;
      Reserved_4         : number RANGE 0 .. 16#FFFFFFFF#;
   END RECORD
   WITH
      Convention => Assembler;
   FOR processor_frequency_leaf_format USE RECORD
      Passed_Leaf           AT 00 RANGE 0 .. 31;
      Passed_Argument       AT 04 RANGE 0 .. 31;
      Base_Frequency_MHz    AT 08 RANGE 0 .. 15;
      Reserved_1            AT 10 RANGE 0 .. 15;
      Max_Frequency_MHz     AT 12 RANGE 0 .. 15;
      Reserved_2            AT 14 RANGE 0 .. 15;
      Bus_Frequency_MHz     AT 16 RANGE 0 .. 15;
      Reserved_3            AT 18 RANGE 0 .. 15;
      Reserved_4            AT 20 RANGE 0 .. 31;
   END RECORD;

   -- A simple type that details the highest normal leaf supported and the same
   -- for the highest extended leaf. This entire record should fit inside a
   -- general purpose x86-64 register.
   TYPE highest_leaf_set IS RECORD
      Normal   : normal_leaf;
      Extended : extended_leaf;
   END RECORD
   WITH
      Convention => Assembler;
   FOR highest_leaf_set USE RECORD
      Normal   AT 0 RANGE 0 .. 31;
      Extended AT 4 RANGE 0 .. 31;
   END RECORD;

   -- Returns the maximum CPUID function/leaf depending on what the maximum
   -- leaf value our Ada enumeration (the "leaf" type) supports and if it's for
   -- the extended leaf values or the normal ones. You should only need to call
   -- this after elaboration for usage with an AP and not the BSP, and even
   -- then, I don't believe it would vary.
   FUNCTION Max_Leaves
     (Leaf_Mask          : normal_leaf;
      Extended_Leaf_Mask : extended_leaf)
      RETURN highest_leaf_set
   WITH
      Global            => NULL,
      Volatile_Function => true,
      Import            => true,
      Convention        => Assembler,
      External_Name     => "assembly__cpuid_highest_leaves",
      Post              => Max_Leaves'result.Normal   <= Leaf_Mask AND THEN
                           Max_Leaves'result.Extended <= Extended_Leaf_Mask;

   -- The below variable indicates the maximum CPUID functions/leaves
   -- that are supported by the bootstrap processor in a static manner.
   Highest_Leaves : CONSTANT highest_leaf_set :=
      Max_Leaves(normal_leaf'last, extended_leaf'last);

   -- This returns the appropriate information in the return record. You should
   -- not call the assembly routine itself directly and instead it should be
   -- called from a generic instantiation that returns the specialised record.
   -- That avoids needing to manually do bit tests and it's also important to
   -- note that `CPUID` can return a wildly different structure not only
   -- depending on what leaf it was passed, but also the additional argument.
   -- One must also take into account that not all CPUID leaves are supported,
   -- whether they be extended leaves or in the original range.
   GENERIC
      TYPE generic_information IS PRIVATE;
      Leaf_Name       : leaf;
   FUNCTION Get_CPUID_Information
     (Passed_Leaf     : IN leaf   := Leaf_Name;
      Passed_Argument : IN number := 0)
      RETURN generic_information
   WITH
      Global        => NULL,
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__cpuid";

   FUNCTION Get_Highest_Function_Parameter IS NEW Get_CPUID_Information
     (generic_information => highest_function_parameter_leaf_format,
      Leaf_Name           => highest_function_parameter_leaf);

   FUNCTION Get_Timestamp_Counter_And_Core_Crystal IS NEW Get_CPUID_Information
     (generic_information => timestamp_counter_and_core_crystal_leaf_format,
      Leaf_Name           => timestamp_counter_and_core_crystal_leaf);

   FUNCTION Get_Processor_Frequency IS NEW Get_CPUID_Information
     (generic_information => processor_frequency_leaf_format,
      Leaf_Name           => processor_frequency_leaf);

END HAVK_Kernel.Intrinsics.CPUID;
