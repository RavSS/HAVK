###############################################################################
## Program         -- The HAVK Operating System                              ##
## Filename        -- system_call.S                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .bss

# A different stack is used to handle system calls. This is for a single core
# only at this stage, so the BSP should be loading this in its RSP register.
.ALIGN 16
__system_call_stack_end:
	.SPACE 16384
__system_call_stack_base:

.SECTION .data

# An address which lets you store the user task's stack temporarily.
# It should only ever contain the below pointer or a user's stack pointer.
# The stack should also be reset upon each subsequent system call.
__system_call_stack_swap:
	.QUAD __system_call_stack_base

__system_call_locked: # 1 means locked, 0 means unlocked.
	.WORD 0 # Only a bit is used; can't make this into a byte due to `BTS`.

.SECTION .rodata

# Instead of using the address attribute in Ada, I'll create a pointer here.
# Reduces the amount of code needed to be written or any SPARK mode exclusions.
.GLOBAL assembly__system_call_entry_address
assembly__system_call_entry_address:
	.QUAD assembly__system_call_entry

.SECTION .text

.GLOBAL assembly__system_call_entry
# (RCX => return address, R11 => previous RFLAGS value) >> (RAX => return code)
assembly__system_call_entry:
	.locked: # Keep the spinlock simple for now, make it performant later.
		LOCK BTS WORD PTR [__system_call_locked], 0
		JC .locked # No deadlock checks implemented yet, just spin.

	# The stack is not changed upon entry, we currently have the task's
	# stack loaded into RSP, so exchange it for the system call stack.
	XCHG RSP, [__system_call_stack_swap] # A lock is implied.
	PUSH RCX # Save the return address.
	PUSH R11 # Save the previous RFLAGS value.
	# Ready to handle the call.

	CALL ada__system_call_handler

	# Handled call. Prepare to return to ring 3.
	POP R11 # Get back the values in the callee-modifiable registers.
	POP RCX # Return address.
	XCHG RSP, [__system_call_stack_swap] # Switch back to the user's stack.

	MOV WORD PTR [__system_call_locked], 0 # Disable the lock.

	# WARNING: The address `SYSCALL` was executed from can only ever be a
	# canonical address, so `REX.W SYSRET` will only ever return to that
	# address. There's an interesting vulnerability with certain CPUs where
	# the check for a canonical address happens in ring 0 instead of
	# ring 3, causing the GPF in the former. That makes it a kernel issue
	# instead of a user program issue. I don't think that can be a problem
	# here, but please do verify and check for yourself.

	# Note that there's only a single `SYSCALL`, but there's two `SYSRET`
	# versions. Without the REX prefix, it returns us to 32-bit operation.
	# I have zero intentions of supporting protected/compatibility mode.
	REX.W SYSRET # This uses RCX's value, not a popped 64-bit stack value.
