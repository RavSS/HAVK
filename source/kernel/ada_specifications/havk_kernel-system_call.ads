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
-- value and avoid placing preconditions on the system call subprograms. Only
-- user tasks should be "calling" the subprograms in here, not kernel code
-- elsewhere in other packages.
PACKAGE HAVK_Kernel.System_Call
IS
PRIVATE
   System_Call_Tag : CONSTANT string := "SYSCALL";

   -- This describes a general register (64-bit variant) for x86-64. It's kept
   -- different from the "number" and "address" types just in case of future
   -- differences in those types. It is unsigned by default, so if signed
   -- values need to be returned, then make a different type.
   TYPE register IS MOD 2**64
   WITH
      Size        => 64,
      Object_Size => 64,
      Annotate    => (GNATprove, No_Wrap_Around);
   PRAGMA Provide_Shift_Operators(register);

   -- The system calls that HAVK supports. The indicated call should always be
   -- in the RDI register, with the following function argument registers for
   -- the x86-64 System V ABI being utilised for the call-specific arguments.
   -- TODO: Implement more of them once we have a functioning user space.
   TYPE operation IS
     (null_operation,
      exit_thread_operation,
      create_thread_operation,
      framebuffer_access_operation,
      future_operation) -- Just to silence case statement warnings.
   WITH
      Convention => C;

   -- This system call does nothing and only logs the arguments passed.
   PROCEDURE Null_Operation_Call
     (RSI : IN register;   -- Argument 1.
      RDX : IN register;   -- Argument 2.
      R8  : IN register;   -- Argument 3.
      R9  : IN register;   -- Argument 4.
      R10 : IN register;   -- Argument 5.
      RIP : IN register;   -- Call address.
      RAX : OUT register); -- An error status returned by us.

   -- This simply marks the thread it was called from to be killed later and
   -- stores the exit code for future reference. An infinite loop should be
   -- placed after the system call for this operation, as the task still has
   -- a remaining time slice left.
   -- TODO: Right now, an exit code of anything above zero is considered an
   -- error. This perhaps should be expanded.
   PROCEDURE Exit_Thread_Operation_Call
     (RSI : IN register); -- Exit code returned by the task's thread.

   -- This creates a thread. The primary thing passed to the call at this stage
   -- is a canonical virtual address which should be pointing to code, meaning
   -- that it must be below 2 GiB (in the range of a 32-bit signed integer).
   -- The stack address is not necessary and can be anything, as invalid stack
   -- addresses should cause an issue in user space due to the fact that we do
   -- nothing except load it into the RSP register.
   PROCEDURE Create_Thread_Operation_Call
     (RSI : IN register;   -- The entry address. Must be canonical.
      RDX : IN register;   -- The stack address. Must be canonical.
      RAX : OUT register); -- An error status returned by us.

   -- This hands over the framebuffer to a task by mapping it into the task's
   -- virtual memory.
   -- TODO: Right now, this just hands it over to any task.
   -- TODO: Does not yet support giving pixel formats that are defined via a
   -- bitmask.
   PROCEDURE Framebuffer_Access_Operation_Call
     (RSI : OUT register;  -- Returned framebuffer base address.
      RDX : OUT register;  -- Returned byte size of the framebuffer.
      R8  : OUT register;  -- 63:32 = Width. 31:0 = Height.
      R9  : OUT register;  -- The amount of pixels in a scanline.
      R10 : OUT register;  -- The pixel format.
      RAX : OUT register); -- An error status returned by us.

END HAVK_Kernel.System_Call;
