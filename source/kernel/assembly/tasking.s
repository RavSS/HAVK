###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- tasking.s                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2021                ##
###############################################################################

.INCLUDE "macros.s"

.SECTION .isolated_text

.GLOBAL assembly__start_tasking
.TYPE assembly__start_tasking, @function
# ()
assembly__start_tasking:
	CLI # Disable interrupts again if need be just in case.
	SUB RSP, 256 # Get enough space to store the interrupt frame + extras.
	CALL ada__get_active_task_state
	M_TASK_STATE LOAD
	POP RBX
	POP RAX
	MOV CR3, RAX
	POP RAX
	MOV BYTE PTR [RIP + ada__tasking_enabled], 0x1
	WBINVD
	REX.W IRET

# This is triggered on every LAPIC timer interrupt. It switches the context
# depending on what the scheduling functions that are called do in their
# execution.
.GLOBAL assembly__interrupt_handler_stub_048
.TYPE assembly__interrupt_handler_stub_048, @function
# ()
assembly__interrupt_handler_stub_048:
	PUSH RAX # Need a temporary register. Save the user's RAX value.
	MOV RAX, CR3
	PUSH RAX # Store the task's page layout.

	# Increment the interrupt counter for the LAPIC timer.
	LEA RAX, [RIP + ada__interrupt_counters]
	LOCK INC QWORD PTR [RAX + 8 * 48] # 8-byte elements.

	# Check if tasking is disabled. If disabled, then jump to the end.
	CMP BYTE PTR [RIP + ada__tasking_enabled], 0x0
	JZ .L_interrupt_return

	# See the interrupts assembly file's generic handler for more on this.
	# Can't assume the direction in here.
	CLD

	# Switch to the kernel's page layout.
	MOV RAX, [RIP + assembly__kernel_page_map_base_address]
	MOV CR3, RAX

	# Get the active task's register state structure/record according to
	# which privilege ring the interruption happened in.
	CMP QWORD PTR [RSP + CS_ON_STACK - 8], 0x8
	# TODO: Would branching be faster here or using CMOVcc?
	CMOVE RAX, [RIP + ada__ring_0_tasking_context]
	CMOVNE RAX, [RIP + ada__ring_3_tasking_context]

	# Save the task's registers. RAX, RBX, and CR3 are stored on the
	# interrupt handler's stack (RSP), with the kernel stack also
	# containing the interrupt frame. Since I need RAX to hold the pointer
	# to the state record, RBX is used for other operations.
	PUSH RBX
	M_TASK_STATE SAVE

	XOR RDI, RDI # Assume we interrupted the task in user space (false).
	CMP QWORD PTR [RSP + CS_ON_STACK], 0x2B
	JE 1f
	MOV RDI, 1 # If we didn't, then set RDI to true (interrupted ring 0).
	1: CALL ada__set_active_task_state_mode

	# Pass a pointer to handler for the interrupt frame properly.
	LEA RDI, [RSP + RIP_ON_STACK]

	# Call the Ada handler. The active task context may or may not change
	# depending on how the call to the scheduler in the Ada handler ends.
	CALL ada__interrupt_handler_048

	# The scheduler has finished. Get the potentially new tasking context.
	CALL ada__get_active_task_state

	# Obtain the interrupt stack again in case we won't be able to access
	# the last one (since it may be a ring 0 accessible stack only) when
	# we're switching the page mappings.
	MOV RSP, [RIP + ada__task_state_segment + 4]
	SUB RSP, 256 # Should be more than enough space for stack operations.

	M_TASK_STATE LOAD
	POP RBX

	.L_interrupt_return:
		POP RAX # CR3's contents in RAX.
		MOV CR3, RAX

		# Signal EOI at the very end. Save clobbered registers first
		# except for RAX.
		PUSH RCX
		PUSH RDX
		M_LAPIC_EOI
		POP RDX
		POP RCX

		POP RAX # User task's RAX value is now back in RAX.

		REX.W IRET

.GLOBAL assembly__tasking_yield
.TYPE assembly__tasking_yield, @function
assembly__tasking_yield:
	INT 48
	RET
