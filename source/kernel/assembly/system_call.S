###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- system_call.S                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.SECTION .isolated_bss

# A different stack is used to handle system calls. This is for a single core
# only at this stage, so the BSP should be loading this in its RSP register.
# Note that this gets mapped into the user's page layout as supervisor only,
# so if we wish to avoid that, then maybe add another even smaller stack here.
.ALIGN 16
var__system_call_stack_end:
	.SPACE 16384
var__system_call_stack_base:

.ALIGN 4

# An address which lets you store the user task's stack temporarily.
# It should only ever contain the stack pointer used for handling system calls
# or the current task's stack pointer. The stack should also be reset upon each
# subsequent system call.
var__system_call_stack_store:
	.SPACE 8

var__system_call_locked: # 1 means locked, 0 means unlocked.
	.SPACE 2 # Only a bit is used; can't make it into a byte due to `BTS`.

.SECTION .rodata

# Instead of using the address attribute in Ada, I'll create a pointer here.
# Reduces the amount of code needed to be written or any SPARK mode exclusions.
.GLOBAL var__system_call_entry_address
var__system_call_entry_address:
	.QUAD assembly__system_call_entry

.SECTION .isolated_text

.GLOBAL assembly__system_call_entry
# (RCX => return address, R11 => previous RFLAGS value, OTHERS => specific to
# the system call) >> (RAX => return code)
assembly__system_call_entry:
	.locked: # Keep the spinlock simple for now, make it performant later.
		LOCK BTS WORD PTR [var__system_call_locked], 0
		JC .locked # No deadlock checks implemented yet, just spin.

	# The stack is not changed upon entry, we currently have the task's
	# stack loaded into RSP, so change it for the system call stack.
	MOV [RIP + var__system_call_stack_store], RSP # Save their stack.
	LEA RSP, [RIP + var__system_call_stack_base] # Load ours.
	PUSH RCX # Save the return address.
	PUSH R11 # Save the previous RFLAGS value.
	CALL assembly__switch_to_kernel_cr3 # We need full kernel mappings.
	# Ready to handle the call.

	CALL ada__system_call_handler

	# Handled call. Prepare to return to ring 3.
	CALL assembly__switch_to_task_cr3 # Vanish the full kernel mappings.
	POP R11 # Get back the values in the callee-modifiable registers.
	POP RCX # Return address.
	MOV RSP, [RIP + var__system_call_stack_store] # Load their stack back.

	MOV WORD PTR [var__system_call_locked], 0 # Disable the lock.

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
