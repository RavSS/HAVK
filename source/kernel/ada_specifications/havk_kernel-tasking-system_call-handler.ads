-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-system_call-handler.ads            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics;
USE TYPE
   HAVK_Kernel.Intrinsics.model_specific_register;

-- This package houses a part of the system call handler that gets entered upon
-- the `SYSCALL` instruction.
PACKAGE HAVK_Kernel.Tasking.System_Call.Handler
IS
   -- This sets the MSRs related to the `SYSCALL` address, notably for
   -- telling it where the system call entry address is and what the code
   -- segment's descriptor indices are for each ring. Also sets the flag mask.
   PROCEDURE Set_MSRs;

PRIVATE
   System_Call_Entry : CONSTANT Memory.canonical_address
   WITH
      Import        => true,
      Convention    => Assembler,
      External_Name => "assembly__system_call_entry_address";

   -- This procedure passes the arguments to an operation according to the
   -- system call. The System V ABI method of passing arguments to a function
   -- is similar to how this procedure passes said arguments to the actual
   -- system call procedures.
   PROCEDURE System_Call_Handler
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
END HAVK_Kernel.Tasking.System_Call.Handler;
