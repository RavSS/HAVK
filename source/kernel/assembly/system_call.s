###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- system_call.s                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.INCLUDE "macros.s"

.SECTION .rodata

# Instead of using the address attribute in Ada, I'll create a pointer here.
# Reduces the amount of code needed to be written or any SPARK mode exclusions.
.GLOBAL global__system_call_entry_address
global__system_call_entry_address:
	.QUAD assembly__system_call_entry

.SECTION .isolated_text

# These two macros must be consistent with the record shown in the system call
# handler package called "arguments". A reminder that the stack grows
# downwards, so the arguments are pushed in reverse to the record shown in the
# system call handler package in the Ada code. Note that before this is called,
# the stack must be 16-byte aligned, as `MOVDQA` is used instead of `MOVDQU`.
.MACRO REGISTER_ARGUMENTS_TO_STACK_ARGUMENTS POINTER_REGISTER:req
	# Push the SIMD registers first.
	SUB RSP, 16 * 17 # RSP must be aligned after this.
	M_SAVE_XMM_REGISTERS RSP

	# Now push the general registers.
	PUSH RAX # Previously clobbered by us.
	PUSH R10
	PUSH R9
	PUSH R8
	PUSH RDX
	PUSH RSI
	PUSH RDI
	PUSH R11 # Task's RFLAGS.
	PUSH RCX # Task's RIP.

	# The stack cannot be 16-byte aligned before the call instruction, as
	# that places a return address onto the stack and would ruin alignment
	# upon the actual entry; thus, increasing it is necessary until
	# another field in the arguments record/structure is added or removed.
	MOV \POINTER_REGISTER, RSP
	SUB RSP, 8
.ENDM
# Be sure to use this after any and all function calls, as we don't want
# anything in here to be modified at all. The user should be seeing all these
# values.
.MACRO STACK_ARGUMENTS_TO_REGISTER_ARGUMENTS
	ADD RSP, 8

	POP RCX
	POP R11
	POP RDI
	POP RSI
	POP RDX
	POP R8
	POP R9
	POP R10
	POP RAX

	M_LOAD_XMM_REGISTERS RSP
	ADD RSP, 16 * 17
.ENDM

.GLOBAL assembly__system_call_entry
.TYPE assembly__system_call_entry, @function
# (RCX => return address, R11 => previous RFLAGS value, OTHERS => specific to
# the system call)
assembly__system_call_entry:
	# The stack is not changed upon entry, we currently have the task's
	# stack loaded into RSP, so change it for the system call stack.

	# Get the task's ring 0 kernel stack (four bytes into the TSS).
	# Don't clobber RSP and RBP just yet, as we need to store their stack
	# pointer and their stack frame pointer.
	MOV RAX, [RIP + global__task_state_segment + 4]

	SUB RAX, 8 # Create enough room for a pointer.
	MOV [RAX], RSP # Save their stack.
	MOV RSP, RAX # Now we're using the task's kernel stack.

	PUSH RBP # Save their stack frame base while realigning the stack too.
	MOV RBP, RSP # Create our own stack frame.

	# Load the kernel's page layout, as we need it to handle system calls.
	MOV RAX, [RIP + global__kernel_page_map_base_address]
	MOV CR3, RAX
	# Ready to handle the call.

	REGISTER_ARGUMENTS_TO_STACK_ARGUMENTS RDI
	# RDI is the pointer to the arguments on the stack now.

	CALL ada__system_call_handler

	M_SWITCH_TO_TASK_CR3 # Remove the full kernel mappings.

	STACK_ARGUMENTS_TO_REGISTER_ARGUMENTS

	# Load the user's stack again that was stored on the kernel stack.
	POP RBP
	MOV RSP, [RSP]

	# Handled the call. Enter ring 3 again.
	SFENCE # Memory fence (make everything prior to this globally visible).

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
