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
# TODO: Maybe just use the task's kernel stack that's used for interrupts...
.ALIGN 16
assembly__system_call_stack_end:
	.SPACE 16384
assembly__system_call_stack_base:

.ALIGN 4

# An address which lets you store the user task's stack temporarily.
# It should only ever contain the stack pointer used for handling system calls
# or the current task's stack pointer. The stack should also be reset upon each
# subsequent system call.
assembly__system_call_stack_store:
	.SPACE 8

assembly__system_call_locked: # 1 means locked, 0 means unlocked.
	.SPACE 2 # Only a bit is used; can't make it into a byte due to `BTS`.

.SECTION .rodata

# Instead of using the address attribute in Ada, I'll create a pointer here.
# Reduces the amount of code needed to be written or any SPARK mode exclusions.
.GLOBAL global__system_call_entry_address
global__system_call_entry_address:
	.QUAD assembly__system_call_entry

.SECTION .isolated_text

# These two macros must be consistent with the record shown in the system call
# handler package called "arguments".
.MACRO REGISTER_ARGUMENTS_TO_STACK_ARGUMENTS
	PUSH RAX
	PUSH R10
	PUSH R9
	PUSH R8
	PUSH RDX
	PUSH RSI
	PUSH RDI
	PUSH R11 # Task's RFLAGS.
	PUSH RCX # Task's RIP.
.ENDM
# Be sure to use this after any and all function calls, as we don't want
# anything in here to be modified at all.
.MACRO STACK_ARGUMENTS_TO_REGISTER_ARGUMENTS
	POP RCX
	POP R11
	POP RDI
	POP RSI
	POP RDX
	POP R8
	POP R9
	POP R10
	POP RAX
.ENDM

.GLOBAL assembly__system_call_entry
# (RCX => return address, R11 => previous RFLAGS value, OTHERS => specific to
# the system call)
assembly__system_call_entry:
	.L_locked: # Keep the spinlock simple for now, make it better later.
		LOCK BTS WORD PTR [RIP + assembly__system_call_locked], 0
		JC .L_locked # No deadlock checks implemented yet, just spin.

	# The stack is not changed upon entry, we currently have the task's
	# stack loaded into RSP, so change it for the system call stack.
	MOV [RIP + assembly__system_call_stack_store], RSP # Save their stack.
	LEA RSP, [RIP + assembly__system_call_stack_base] # Load ours.

	# Load the kernel's page layout, as we need it to handle system calls.
	MOV RAX, [RIP + global__kernel_page_map_base_address]
	MOV CR3, RAX
	# Ready to handle the call.

	REGISTER_ARGUMENTS_TO_STACK_ARGUMENTS
	MOV RDI, RSP # RDI is the pointer to the arguments now.

	CALL ada__system_call_handler

	CALL assembly__switch_to_task_cr3 # Vanish the full kernel mappings.

	STACK_ARGUMENTS_TO_REGISTER_ARGUMENTS

	MOV RSP, [RIP + assembly__system_call_stack_store] # Load their stack.

	# Handled the call. Disable the entry lock and enter ring 3 again.
	MOV WORD PTR [RIP + assembly__system_call_locked], 0

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
