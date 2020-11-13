-------------------------------------------------------------------------------
-- Program         -- HAVK                                                   --
-- Filename        -- havk_kernel-system_call.ads                            --
-- License         -- GNU General Public License version 3.0                 --
-- Original Author -- Ravjot Singh Samra, Copyright 2020                     --
-------------------------------------------------------------------------------

WITH
   Ada.Unchecked_Conversion,
   HAVK_Kernel.Intrinsics;
USE TYPE
   HAVK_Kernel.Intrinsics.general_register,
   HAVK_Kernel.Intrinsics.XMM_registers;

-- In this package, there is an API to interact with the kernel from ring 3.
-- The actual handling of the system calls is handled from a child package, but
-- the call logic is implemented here for the sake of organisation. When
-- creating new system calls, assume nearly everything is passed as an invalid
-- value and avoid placing preconditions on the system call subprograms. Only
-- user tasks should be "calling" the subprograms in here, not kernel code
-- elsewhere in other packages.
-- TODO: Document the system calls more carefully.
PACKAGE HAVK_Kernel.System_Call
IS
PRIVATE
   System_Call_Tag : CONSTANT string := "SYSCALL";

   -- The system calls that HAVK supports. The indicated call should always be
   -- in the RDI register, with the following function argument registers for
   -- the x86-64 System V ABI being utilised for the call-specific arguments.
   -- TODO: Implement more of them once we have a functioning user space.
   TYPE operation IS
     (null_operation,
      exit_task_operation,
      receive_message_operation,
      send_message_operation,
      identify_task_operation,
      load_elf_operation,
      heap_increase_operation,
      yield_operation,
      log_operation,
      irq_statistics_operation,
      io_port_operation,
      buffer_operation,
      framebuffer_access_operation)
   WITH
      Convention => C;

   -- This system call does nothing and only logs the arguments passed.
   -- @param RSI Argument 1.
   -- @param RDX Argument 2.
   -- @param R8 Argument 3.
   -- @param R9 Argument 4.
   -- @param R10 Argument 5.
   -- @param RAX An error status.
   PROCEDURE Null_Operation_Call
     (RSI : IN Intrinsics.general_register;
      RDX : IN Intrinsics.general_register;
      R8  : IN Intrinsics.general_register;
      R9  : IN Intrinsics.general_register;
      R10 : IN Intrinsics.general_register;
      RAX : OUT Intrinsics.general_register;
      RIP : IN Intrinsics.general_register);

   -- This simply marks the task it was called from to be removed later and
   -- stores the exit code for future reference. An infinite loop should be
   -- placed after the system call for this operation, as the task still has
   -- a remaining time slice left.
   -- TODO: Right now, an exit code of anything above zero is considered an
   -- error. This perhaps should be expanded.
   -- @param RSI Exit code returned by the task.
   PROCEDURE Exit_Task_Operation_Call
     (RSI : IN Intrinsics.general_register);

   -- Obtains the oldest message sent to the task that called this operation.
   -- @param RSI The sending task's index/identity. This is not supplied by the
   -- task, but is rather returned to indicate who truly send the message data.
   -- @param RDX The length of the data. It will be at or below the total XMM
   -- bit space, which is 2048 bits or 256 bytes. This is only provided for
   -- convenience, as you could use your own header in the data itself.
   -- @param R8 The port the message was sent to.
   -- @param XMM A buffer of data which fits inside all XMM registers.
   -- @param RAX An error status.
   PROCEDURE Receive_Message_Operation_Call
     (RSI : OUT Intrinsics.general_register;
      RDX : OUT Intrinsics.general_register;
      R8  : OUT Intrinsics.general_register;
      XMM : OUT Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- Creates a new message directed towards the task index specified and holds
   -- it in a buffer until it is received.
   -- @param RSI The receiving task's index/identity.
   -- @param RDX The length of the data. Must be at or below the total XMM bit
   -- space, which is 2048 bits or 256 bytes. This is only provided for
   -- convenience, as you could use your own header in the data itself.
   -- @param R8 The port the message will be sent to.
   -- @param XMM A buffer of data which fits inside all XMM registers.
   -- @param RAX An error status.
   PROCEDURE Send_Message_Operation_Call
     (RSI : IN Intrinsics.general_register;
      RDX : IN Intrinsics.general_register;
      R8  : IN Intrinsics.general_register;
      XMM : IN Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- Returns the task index/identity and the task's string name.
   -- @param RSI The task index/identity. If this is zero, then the status of
   -- the calling task is returned.
   -- @param XMM The task's status. See the "task_status" type in the
   -- "HAVK_Kernel.Tasking" package for its representation.
   -- @param RAX An error status.
   PROCEDURE Identify_Task_Operation_Call
     (RSI : IN Intrinsics.general_register;
      XMM : OUT Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- Loads an ELF file off the current task's kernel buffer.
   -- @param XMM The name for the newly created task.
   -- @param RAX An error status.
   PROCEDURE Load_ELF_Operation_Call
     (XMM : IN Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- Increases the total heap area of the task's memory space by a single
   -- physical frame (4 KiB page).
   -- @param RSI The new end of the heap memory area (if successful).
   -- @param RAX An error status.
   PROCEDURE Heap_Increase_Operation_Call
     (RSI : OUT Intrinsics.general_register;
      RAX : OUT Intrinsics.general_register);

   -- This system call simply ends the time slice for the operation caller.
   -- @param RAX An error status. This always returns no error.
   PROCEDURE Yield_Operation_Call
     (RAX : OUT Intrinsics.general_register);

   -- Simply outputs a string to the UART.
   -- TODO: Right now, any task can call this. It's only for debugging user
   -- tasks at this stage. In the future, giving a specified "logging program"
   -- the capability to call this would be good.
   -- @param XMM The 256-byte string. Null characters are internally ignored.
   -- @param RAX An error status. As of now, no error can occur.
   PROCEDURE Log_Operation_Call
     (XMM : IN Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- This retrieves statistics for an IRQ vector.
   -- TODO: Define whether this is per-CPU or global.
   -- TODO: Don't let just any task call this.
   -- @param RSI The IRQ vector that you wish to retrieve statistics for.
   -- @param RDX The amount of times the IRQ has been triggered.
   PROCEDURE IRQ_Statistics_Operation_Call
     (RSI : IN Intrinsics.general_register;
      RDX : OUT Intrinsics.general_register;
      RAX : OUT Intrinsics.general_register);

   -- Interacts with the x86 I/O ports.
   -- TODO: This currently lets any task mess with them, so that will need to
   -- change sooner or later.
   -- TODO: This is also a temporary substitute for proper I/O permissions in
   -- the TSS; however, that seems to be an obscure x86 feature that may be
   -- valuable in potentially avoiding?
   -- @param RSI The 16-bit port address.
   -- @param RDX The value to send/output or the value to retrieve/input
   -- depending on the call options. The size of the value also depends.
   -- @param R8 A boolean value indicating whether to send (false) or retrieve
   -- (true). Any value that is not zero is interpreted as true.
   -- @param R9 A boolean value indicating whether to send or retrieve an 8-bit
   -- value (false) or a 16-bit value (true). Any value that is not zero is
   -- interpreted as true.
   -- @param RAX An error status.
   PROCEDURE IO_Port_Operation_Call
     (RSI : IN Intrinsics.general_register;
      RDX : IN OUT Intrinsics.general_register;
      R8  : IN Intrinsics.general_register;
      R9  : IN Intrinsics.general_register;
      RAX : OUT Intrinsics.general_register);

   -- Interacts with a kernel buffer. Each task can have a single kernel buffer
   -- which belongs to itself.
   -- @param RSI The operation to perform on the buffer.
   -- 1 = Create, 2 = Reading, 3 = Writing, 4 = Grant Ownership, 5 = Delete.
   -- @param RDX If creating, then this is the size of the buffer. If
   -- reading/writing, then this is the byte offset of the buffer. If changing
   -- ownership, then this is the task identity you wish to grant access to. If
   -- deleting, then this is ignored.
   -- @param XMM This contains the data if you are reading the buffer or
   -- writing to buffer.
   -- @param RAX An error status.
   PROCEDURE Buffer_Operation_Call
     (RSI : IN Intrinsics.general_register;
      RDX : IN Intrinsics.general_register;
      XMM : IN OUT Intrinsics.XMM_registers;
      RAX : OUT Intrinsics.general_register);

   -- This hands over the framebuffer to a task by mapping it into the task's
   -- virtual memory.
   -- TODO: Right now, this just hands it over to any task.
   -- TODO: Does not yet support giving pixel formats that are defined via a
   -- bitmask.
   -- @param RSI Returned framebuffer base address.
   -- @param RDX Returned byte size of the framebuffer.
   -- @param R8 63:32 = Width. 31:0 = Height.
   -- @param R9 The amount of pixels in a scanline.
   -- @param R10 The pixel format.
   -- @param RAX An error status.
   PROCEDURE Framebuffer_Access_Operation_Call
     (RSI : OUT Intrinsics.general_register;
      RDX : OUT Intrinsics.general_register;
      R8  : OUT Intrinsics.general_register;
      R9  : OUT Intrinsics.general_register;
      R10 : OUT Intrinsics.general_register;
      RAX : OUT Intrinsics.general_register);

   -- A type to help with converting the data buffer to a string.
   TYPE XMM_registers_characters IS ARRAY(1 .. 256) OF ALIASED character
   WITH
      Size        => 128 * Intrinsics.XMM_registers'length,
      Object_Size => 128 * Intrinsics.XMM_registers'length;

   FUNCTION To_Characters IS NEW Ada.Unchecked_Conversion
     (source => Intrinsics.XMM_registers,
      target => XMM_registers_characters);
   PRAGMA Annotate(GNATprove, False_Positive, "type with constraints *",
      "It's just an array with each element being a byte.");

END HAVK_Kernel.System_Call;
