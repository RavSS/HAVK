-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-tasking-system_call.ads                    --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020-2021                --
-------------------------------------------------------------------------------

WITH
   HAVK_Kernel.Intrinsics,
   HAVK_Kernel.Memory;
USE TYPE
   HAVK_Kernel.Intrinsics.XMM_registers;

-- In this package, there is an API to interact with the kernel from ring 3.
-- The actual handling of the system calls is handled from a child package, but
-- the call logic is implemented here for the sake of organisation. When
-- creating new system calls, assume nearly everything is passed as an invalid
-- value and avoid placing preconditions on the system call subprograms. Only
-- user tasks should be "calling" the subprograms in here, not kernel code
-- elsewhere in other packages.
-- TODO: Document the system calls more carefully.
PACKAGE HAVK_Kernel.Tasking.System_Call
IS
PRIVATE
   System_Call_Tag : CONSTANT string := "SYSCALL";

   -- The "HAVK_Kernel.Memory" package's name conflicts with the
   -- "HAVK_Kernel.Tasking.Memory" package's name by default. We need the
   -- former most of the time.
   PACKAGE Memory RENAMES HAVK_Kernel.Memory;

   -- The system calls that HAVK supports. The indicated call should always be
   -- in the RDI register, with the following function argument registers for
   -- the x86-64 System V ABI being utilised for the call-specific arguments.
   -- TODO: Implement more of them once we have a functioning user space.
   TYPE operation IS NEW Intrinsics.general_register;
   Null_Operation               : CONSTANT operation := 00;
   Exit_Task_Operation          : CONSTANT operation := 01;
   Receive_Message_Operation    : CONSTANT operation := 02;
   Send_Message_Operation       : CONSTANT operation := 03;
   Identify_Task_Operation      : CONSTANT operation := 04;
   Load_ELF_Operation           : CONSTANT operation := 05;
   Heap_Increase_Operation      : CONSTANT operation := 06;
   Yield_Operation              : CONSTANT operation := 07;
   Log_Operation                : CONSTANT operation := 08;
   IRQ_Statistics_Operation     : CONSTANT operation := 09;
   Buffer_Operation             : CONSTANT operation := 10;
   Framebuffer_Access_Operation : CONSTANT operation := 11;

   -- This system call does nothing and only logs the arguments passed.
   PROCEDURE Null_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN Intrinsics.general_register;
      Argument_4   : IN Intrinsics.general_register;
      Argument_5   : IN Intrinsics.general_register;
      Call_Address : IN Memory.canonical_address;
      Error_Status : OUT Intrinsics.general_register);

   -- This simply marks the task it was called from to be removed later and
   -- stores the exit code for future reference. An infinite loop should be
   -- placed after the system call for this operation, as the task still has
   -- a remaining time slice left.
   -- TODO: Right now, an exit code of anything above zero is considered an
   -- error. This perhaps should be expanded.
   -- @param Argument_1 Exit code returned by the task.
   PROCEDURE Exit_Task_Operation_Call
     (Argument_1 : IN Intrinsics.general_register)
   WITH
      No_Return => true;

   -- Receives a message from a particular task.
   -- @param Argument_1 The sending task's index/identity which the task wishes
   -- to receive a message from. Zero can also be specified to get the first
   -- message where the message box is full.
   -- @param Argument_2 A 64-bit "header" field. Can be anything.
   -- @param Argument_3 A 64-bit "subheader" field. Can be anything.
   -- @param Argument_4 A buffer of data which fits inside all XMM registers.
   PROCEDURE Receive_Message_Operation_Call
     (Argument_1   : IN OUT Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Argument_3   : OUT Intrinsics.general_register;
      Argument_4   : OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- Creates a new message directed towards the task index specified and holds
   -- it in a message box until it is received.
   -- @param Argument_1 The receiving task's index/identity.
   -- @param Argument_2 A 64-bit "header" field. Can be anything.
   -- @param Argument_3 A 64-bit "subheader" field. Can be anything.
   -- @param Argument_4 A buffer of data which fits inside all XMM registers.
   PROCEDURE Send_Message_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN Intrinsics.general_register;
      Argument_4   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- Returns the task index/identity and the task's string name.
   -- @param Argument_1 The task index/identity. If this is zero, then the
   -- status of the calling task is returned.
   -- @param Argument_2 The task's status. See the "task_status" type in the
   -- "HAVK_Kernel.Tasking" package for its representation.
   PROCEDURE Identify_Task_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- Loads an ELF file off the current task's kernel buffer.
   -- @param Argument_1 The name for the newly created task.
   PROCEDURE Load_ELF_Operation_Call
     (Argument_1   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- Increases the total heap area of the task's memory space by a single
   -- physical frame (4 KiB page).
   -- @param Argument_1 The new end of the heap memory area (if successful).
   PROCEDURE Heap_Increase_Operation_Call
     (Argument_1   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register);

   -- This system call simply ends the time slice for the operation caller.
   -- TODO: Does not yet support switching to another preferred task index.
   -- Need to use IPC for that.
   PROCEDURE Yield_Operation_Call
     (Error_Status : OUT Intrinsics.general_register);

   -- Simply outputs a string to the UART.
   -- TODO: Right now, any task can call this. It's only for debugging user
   -- tasks at this stage. In the future, giving a specified "logging program"
   -- the capability to call this would be good.
   -- @param Argument_1 The 256-byte string. Null characters are internally
   -- ignored.
   PROCEDURE Log_Operation_Call
     (Argument_1   : IN Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- This retrieves statistics for an IRQ vector.
   -- TODO: Define whether this is per-CPU or global.
   -- TODO: Don't let just any task call this.
   -- @param Argument_1 The IRQ vector that you wish to retrieve statistics
   -- for.
   -- @param Argument_2 The amount of times the IRQ has been triggered.
   PROCEDURE IRQ_Statistics_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register);

   -- Interacts with a kernel buffer. Each task can have a single kernel buffer
   -- which belongs to itself.
   -- @param Argument_1 The operation to perform on the buffer.
   -- 1 = Create, 2 = Reading, 3 = Writing, 4 = Delete.
   -- @param Argument_2 If creating, then this is the size of the buffer. If
   -- reading/writing, then this is the one-based byte index of the buffer. If
   -- deleting, then this is ignored.
   -- @param Argument_3 This contains the data if you are reading the buffer or
   -- writing to buffer.
   PROCEDURE Buffer_Operation_Call
     (Argument_1   : IN Intrinsics.general_register;
      Argument_2   : IN Intrinsics.general_register;
      Argument_3   : IN OUT Intrinsics.XMM_registers;
      Error_Status : OUT Intrinsics.general_register);

   -- This hands over the framebuffer to a task by mapping it into the task's
   -- virtual memory.
   -- TODO: Right now, this just hands it over to any task.
   -- TODO: Does not yet support giving pixel formats that are defined via a
   -- bitmask.
   -- @param Argument_1 Returned framebuffer base address.
   -- @param Argument_2 Returned byte size of the framebuffer.
   -- @param Argument_3 63:32 = Width. 31:0 = Height.
   -- @param Argument_4 The amount of pixels in a scanline.
   -- @param Argument_5 The pixel format.
   PROCEDURE Framebuffer_Access_Operation_Call
     (Argument_1   : OUT Intrinsics.general_register;
      Argument_2   : OUT Intrinsics.general_register;
      Argument_3   : OUT Intrinsics.general_register;
      Argument_4   : OUT Intrinsics.general_register;
      Argument_5   : OUT Intrinsics.general_register;
      Error_Status : OUT Intrinsics.general_register);

END HAVK_Kernel.Tasking.System_Call;
