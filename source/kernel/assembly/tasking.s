###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- tasking.s                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

.INCLUDE "macros.s"

# This structure must match the Ada "register_state" record.
.STRUCT 0
	CR3_STATE:
.STRUCT CR3_STATE + 8
	CS_STATE:
.STRUCT CS_STATE + 8
	MMX_STATE:
.STRUCT MMX_STATE + 64
	R8_STATE:
.STRUCT R8_STATE + 8
	R9_STATE:
.STRUCT R9_STATE + 8
	R10_STATE:
.STRUCT R10_STATE + 8
	R11_STATE:
.STRUCT R11_STATE + 8
	R12_STATE:
.STRUCT R12_STATE + 8
	R13_STATE:
.STRUCT R13_STATE + 8
	R14_STATE:
.STRUCT R14_STATE + 8
	R15_STATE:
.STRUCT R15_STATE + 8
	RAX_STATE:
.STRUCT RAX_STATE + 8
	RBP_STATE:
.STRUCT RBP_STATE + 8
	RBX_STATE:
.STRUCT RBX_STATE + 8
	RCX_STATE:
.STRUCT RCX_STATE + 8
	RDI_STATE:
.STRUCT RDI_STATE + 8
	RDX_STATE:
.STRUCT RDX_STATE + 8
	RFLAGS_STATE:
.STRUCT RFLAGS_STATE + 8
	RIP_STATE:
.STRUCT RIP_STATE + 8
	RSI_STATE:
.STRUCT RSI_STATE + 8
	RSP_STATE:
.STRUCT RSP_STATE + 8
	SS_STATE:
.STRUCT SS_STATE + 8
	XMM_STATE: # 16-byte alignment not required.
.STRUCT XMM_STATE + 256

# Lays out an interrupt frame, along with the CR3 value and temporary registers
# space (I use RAX and RBX).
.STRUCT 0
	RBX_ON_STACK:
.STRUCT RBX_ON_STACK + 8
	CR3_ON_STACK:
.STRUCT CR3_ON_STACK + 8
	RAX_ON_STACK:
.STRUCT RAX_ON_STACK + 8
	RIP_ON_STACK:
.STRUCT RIP_ON_STACK + 8
	CS_ON_STACK:
.STRUCT CS_ON_STACK + 8
	RFLAGS_ON_STACK:
.STRUCT RFLAGS_ON_STACK + 8
	RSP_ON_STACK:
.STRUCT RSP_ON_STACK + 8
	SS_ON_STACK:
.STRUCT SS_ON_STACK + 8

# Transfers the registers which are on the stack as seen above.
.MACRO STACK_TRANSFER OPERATION:req REGISTER_NAME:req
	.IFC \OPERATION, LOAD
		MOV RBX, [RAX + \REGISTER_NAME\()_STATE]
		MOV [RSP + \REGISTER_NAME\()_ON_STACK], RBX
		.EXITM
	.ENDIF

	.IFC \OPERATION, SAVE
		MOV RBX, [RSP + \REGISTER_NAME\()_ON_STACK]
		MOV [RAX + \REGISTER_NAME\()_STATE], RBX
		.EXITM
	.ENDIF

	.ERROR "the stack transfer operation is neither SAVE nor LOAD"
.ENDM

# Transfers integer registers and the SIMD registers (limited to MMX and XMM).
.MACRO REGISTER_TRANSFER OPERATION:req REGISTER_NAME:req
	.IFC \OPERATION, LOAD
		.IFC \REGISTER_NAME, XMM
			LEA RBX, [RAX + XMM_STATE]
			M_UNALIGNED_LOAD_XMM_REGISTERS RBX
			.EXITM
		.ENDIF

		.IFC \REGISTER_NAME, MMX
			LEA RBX, [RAX + MMX_STATE]
			M_LOAD_MMX_REGISTERS RBX
			.EXITM
		.ENDIF

		MOV \REGISTER_NAME, [RAX + \REGISTER_NAME\()_STATE]
		.EXITM
	.ENDIF

	.IFC \OPERATION, SAVE
		.IFC \REGISTER_NAME, XMM
			LEA RBX, [RAX + XMM_STATE]
			M_UNALIGNED_SAVE_XMM_REGISTERS RBX
			.EXITM
		.ENDIF

		.IFC \REGISTER_NAME, MMX
			LEA RBX, [RAX + MMX_STATE]
			M_SAVE_MMX_REGISTERS RBX
			.EXITM
		.ENDIF

		MOV [RAX + \REGISTER_NAME\()_STATE], \REGISTER_NAME
		.EXITM
	.ENDIF

	.ERROR "the register transfer operation is neither SAVE nor LOAD"
.ENDM

# This macro saves the current task's state to the state structure that RAX
# points to. The stack structure must also be at the address RSP is pointing to
# and RBX is clobbered. "OPERATION" can either be "SAVE" or "LOAD".
.MACRO TASK_STATE OPERATION:req
	STACK_TRANSFER \OPERATION CR3
	STACK_TRANSFER \OPERATION CS
	REGISTER_TRANSFER \OPERATION MMX
	REGISTER_TRANSFER \OPERATION R8
	REGISTER_TRANSFER \OPERATION R9
	REGISTER_TRANSFER \OPERATION R10
	REGISTER_TRANSFER \OPERATION R11
	REGISTER_TRANSFER \OPERATION R12
	REGISTER_TRANSFER \OPERATION R13
	REGISTER_TRANSFER \OPERATION R14
	REGISTER_TRANSFER \OPERATION R15
	STACK_TRANSFER \OPERATION RAX
	REGISTER_TRANSFER \OPERATION RBP
	STACK_TRANSFER \OPERATION RBX
	REGISTER_TRANSFER \OPERATION RCX
	REGISTER_TRANSFER \OPERATION RDI
	REGISTER_TRANSFER \OPERATION RDX
	STACK_TRANSFER \OPERATION RFLAGS
	STACK_TRANSFER \OPERATION RIP
	REGISTER_TRANSFER \OPERATION RSI
	STACK_TRANSFER \OPERATION RSP
	STACK_TRANSFER \OPERATION SS
	REGISTER_TRANSFER \OPERATION XMM
.ENDM

.SECTION .isolated_text

.GLOBAL assembly__start_tasking
.TYPE assembly__start_tasking, @function
# ()
assembly__start_tasking:
	CLI # Disable interrupts again if need be just in case.
	SUB RSP, 256 # Get enough space to store the interrupt frame + extras.
	MOV RAX, [RIP + ada__tasking_context]
	TASK_STATE LOAD
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

	# Get the active task's register state structure/record.
	MOV RAX, [RIP + ada__tasking_context]

	# Save the task's registers. RAX, RBX, and CR3 are stored on the
	# interrupt handler's stack (RSP), with the kernel stack also
	# containing the interrupt frame. Since I need RAX to hold the pointer
	# to the state record, RBX is used for other operations.
	PUSH RBX
	TASK_STATE SAVE

	# Call the Ada handler. The active task context may or may not change
	# depending on how the call to the scheduler in the Ada handler ends.
	CALL ada__interrupt_handler_048

	# The scheduler has finished. Get the potentially new tasking context.
	MOV RAX, [RIP + ada__tasking_context]
	TASK_STATE LOAD
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
