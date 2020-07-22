-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call-handler.ads                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Memory;
USE TYPE
   HAVK_Kernel.Intrinsics.model_specific_register;

-- This package houses a part of the system call handler that gets entered upon
-- the `SYSCALL` instruction.
PACKAGE HAVK_Kernel.System_Call.Handler
IS
   -- This sets the MSRs related to the `SYSCALL` address, notably for
   -- telling it where the system call entry address is and what the code
   -- segment's descriptor indices are for each ring. Also sets the flag mask.
   PROCEDURE Set_MSRs;

PRIVATE
   System_Call_Entry : CONSTANT address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "global__system_call_entry_address";

   -- A pointer to this record is passed to the system call handler. That way
   -- we can provide a return code without using functions in Ada. Passing the
   -- values normally with the "IN OUT" mode means that each value needs to be
   -- placed onto the stack anyway with a pointer to it. This ensures
   -- consistency. Also make sure that this consistent with the push/pop
   -- sequence macros defined in the "system_call.S" file.
   TYPE arguments IS LIMITED RECORD
      -- RCX is clobbered by `SYSCALL` and it puts the instruction's RIP into
      -- the RCX register itself.
      RCX_RIP    : Memory.canonical_address; -- Should probably not touch this.
      -- Similar to the RCX register, the R11 register is clobbered as well and
      -- gets replaced with the value of RFLAGS before `SYSCALL` was executed.
      R11_RFLAGS : register;  -- Should probably not touch this.
      RDI        : operation; -- Validate this before reading it.
      RSI        : register;  -- Argument 1.
      RDX        : register;  -- Argument 2.
      R8         : register;  -- Argument 3.
      R9         : register;  -- Argument 4.
      R10        : register;  -- Argument 5.
      RAX        : register;  -- Not passed by the user, but returned by us.
   END RECORD
   WITH
      Convention => Assembler;
   FOR arguments USE RECORD
      RCX_RIP    AT 00 RANGE 0 .. 63;
      R11_RFLAGS AT 08 RANGE 0 .. 63;
      RDI        AT 16 RANGE 0 .. 63;
      RSI        AT 24 RANGE 0 .. 63;
      RDX        AT 32 RANGE 0 .. 63;
      R8         AT 40 RANGE 0 .. 63;
      R9         AT 48 RANGE 0 .. 63;
      R10        AT 56 RANGE 0 .. 63;
      RAX        AT 64 RANGE 0 .. 63; -- Clobbers RAX, not just EAX.
   END RECORD;

   -- This procedure passes the arguments to an operation according to the
   -- system call. It expects that the entry stub for the system call passes a
   -- pointer/access to the record up above. They must match, or else trouble
   -- will occur.
   PROCEDURE System_Call_Handler
     (Values : NOT NULL ACCESS arguments)
   WITH
      Export        => true,
      Convention    => Assembler,
      External_Name => "ada__system_call_handler";

   -- These MSRs below are mostly related to the `SYSCALL` instruction.

   -- The Extended Feature Enable Register (EFER). This contains a few things,
   -- like the ability to enter and exit long mode (no use for us) and most
   -- importantly the ability to enable System Call Extensions (SCE) for the
   -- `SYSCALL` and `SYSRET` instructions. If not enabled, you'll get a UD
   -- exception (undefined opcode) when using them. All that needs to be done
   -- is the bottom bit (bit zero) needs to be set.
   IA32_EFER  : CONSTANT Intrinsics.model_specific_register := 16#C0000080#;

   -- The MSR which contains the kernel-level CS descriptor index and a
   -- reserved descriptor index for managing system call privileges. So how
   -- this works is quite subtle. `SYSCALL` and 64-bit `SYSRET` expect a very
   -- particular layout in the GDT. The ring 0 CS must come after the first
   -- null entry and its value at 32-to-47 bits is used as the base ring 0 CS,
   -- while it adds 8 to get the SS. Meanwhile, it gets more bizarre with
   -- `REX.W SYSRET`, which expects a different null entry just after the
   -- kernel-level descriptors. That index is added at 48-to-63 bits of the MSR
   -- value and the microcode operation adds 16 to it for the ring 3 CS
   -- calculation while adding 8 to it for the ring 3 SS (DPL3 descriptor).
   -- So again, I need to provide two 16-bit values, with the higher one
   -- being a descriptor which is a gap between the different DPLs and the
   -- lower one just being the ring 0 CS. This basically limits the GDT to:
   -- [Null, CS_0, DS_0, <...>, Reserved/Null, DS_3, CS_3]. It's confusing and
   -- it's hilarious how I had to find an short and obscure OSDev forum post
   -- implying it and also how the documentation does not explicitly state it.
   -- If anyone's reading this, then make a PSA about it for long mode, since
   -- the reserved descriptor index only gets 16 bytes added to it in a 64-bit
   -- `SYSRET` (`REX.W SYSRET`) and not the regular `SYSRET`. An explanation
   -- for this is likely due to compatibility with protected mode descriptor
   -- indices or something along those lines, as some kernels and operating
   -- systems still support 32-bit mode (unlike HAVK).
   IA32_STAR  : CONSTANT Intrinsics.model_specific_register := 16#C0000081#;

   -- The address of the system call entry address that gets loaded into RIP.
   -- The value written to it must be a 64-bit canonical address, which is a
   -- sign-extended address in other words.
   IA32_LSTAR : CONSTANT Intrinsics.model_specific_register := 16#C0000082#;

   -- A value written to this acts as a mask for the RFLAGS register; thus,
   -- a flag mask. The masking operation begins in the `SYSCALL` instruction
   -- itself. This can be useful for masking the interrupts enabled bit.
   -- Every bit set in the mask means every flag bit disabled upon entry.
   IA32_FMASK : CONSTANT Intrinsics.model_specific_register := 16#C0000084#;
END HAVK_Kernel.System_Call.Handler;
