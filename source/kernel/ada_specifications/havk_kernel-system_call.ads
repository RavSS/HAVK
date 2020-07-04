-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

-- In this package, there is an API to interact with the kernel from ring 3.
-- The actual handling of the system calls is handled from a child package, but
-- the call logic is implemented here for the sake of organisation. When
-- creating new system calls, assume nearly everything is passed as an invalid
-- value and avoid placing preconditions on the system call subprograms.
PACKAGE HAVK_Kernel.System_Call
IS
   -- The system calls that HAVK supports. The indicated call should always be
   -- in the RDI register, with the following function argument registers for
   -- the x86-64 System V ABI being utilised for the call-specific arguments.
   -- TODO: Implement more of them once we have a functioning user space.
   TYPE operation IS
     (null_operation,
      write_operation, -- TODO: Still creating it.
      read_operation,  -- TODO: Still creating it.
      create_thread_operation)
   WITH
      Convention => C;

   -- This system call does nothing and only logs the arguments passed.
   PROCEDURE Null_Operation_Call
     (RSI : IN number;  -- Argument 1.
      RDX : IN number;  -- Argument 2.
      R8  : IN number;  -- Argument 3.
      R9  : IN number;  -- Argument 4.
      RIP : IN address; -- Call address.
      RAX : OUT error); -- An error status returned by us.

   -- This creates a thread. The only thing passed at this stage is a canonical
   -- virtual address which should be pointing to code, meaning that it must be
   -- below 2 GiB (in the range of a 32-bit signed integer).
   PROCEDURE Create_Thread_Operation_Call
     (RSI : IN number;  -- The entry address. Must be canonical.
      RDX : IN number;  -- The stack address. Must be canonical.
      RAX : OUT error); -- An error status returned by us.

PRIVATE
   System_Call_Tag : CONSTANT string := "SYSCALL";

END HAVK_Kernel.System_Call;
