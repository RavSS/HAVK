###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- tasking.s                                              ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# This structure must match the Ada "callee_saved_register" record.
.STRUCT 0
	RBX_STATE:
.STRUCT RBX_STATE + 8
	RBP_STATE:
.STRUCT RBP_STATE + 8
	RSP_STATE:
.STRUCT RSP_STATE + 8
	R12_STATE:
.STRUCT R12_STATE + 8
	R13_STATE:
.STRUCT R13_STATE + 8
	R14_STATE:
.STRUCT R14_STATE + 8
	R15_STATE:
.STRUCT R15_STATE + 8
	# In long mode, SS is basically always the same as the other segment
	# selector registers. I'll treat FS and GS specially later on.
	SS_STATE:
.STRUCT SS_STATE + 8

.MACRO SAVE_REGISTERS_TO_STATE STATE_POINTER:req
	MOV [\STATE_POINTER + RBX_STATE], RBX
	MOV [\STATE_POINTER + RBP_STATE], RBP
	MOV [\STATE_POINTER + RSP_STATE], RSP
	MOV [\STATE_POINTER + R12_STATE], R12
	MOV [\STATE_POINTER + R13_STATE], R13
	MOV [\STATE_POINTER + R14_STATE], R14
	MOV [\STATE_POINTER + R15_STATE], R15
	# Don't bother saving segment register states, as we handle the switch
	# from ring 0 and don't want to give the wrong DPL to the task. Assign
	# SS statically at task creation.
.ENDM

.MACRO LOAD_REGISTERS_FROM_STATE STATE_POINTER:req
	MOV RBX, [\STATE_POINTER + RBX_STATE]
	MOV RBP, [\STATE_POINTER + RBP_STATE]
	MOV RSP, [\STATE_POINTER + RSP_STATE]
	MOV R12, [\STATE_POINTER + R12_STATE]
	MOV R13, [\STATE_POINTER + R13_STATE]
	MOV R14, [\STATE_POINTER + R14_STATE]
	MOV R15, [\STATE_POINTER + R15_STATE]
	MOV DS, [\STATE_POINTER + SS_STATE]
	MOV ES, [\STATE_POINTER + SS_STATE]
	MOV FS, [\STATE_POINTER + SS_STATE]
	MOV GS, [\STATE_POINTER + SS_STATE]
	# CS and SS are already handled by `REX.W IRET`.
.ENDM

.SECTION .isolated_text

.GLOBAL assembly__start_tasking
.TYPE assembly__start_tasking, @function
# ()
assembly__start_tasking:
	CLI # Disable any interrupts just in case as usual.
	LOCK INC BYTE PTR [RIP + global__tasking_enabled] # Enable tasking.

	CALL ada__get_active_task_state
	LOAD_REGISTERS_FROM_STATE RAX # Switched to the task's kernel stack.

	CALL ada__get_active_task_cr3 # Get the CR3 value and load it.
	MOV CR3, RAX
	XOR RAX, RAX # Don't leak it by accident.

	# No need to check for the CPL and DPL of the interrupted state frame,
	# as we will always start multi-tasking mode from ring 0 directly into
	# ring 3 without nested interrupts.
	REX.W IRET # We have finally entered multi-tasking mode.

# Reminder that this is an ISR. It is not a simple procedure/function.
# It can be called from another interrupt handler via interrupt nesting.
# Note that it must be called from ring 0, not ring 3.
.GLOBAL assembly__switch_task
.TYPE assembly__switch_task, @function
# ()
assembly__switch_task:
	# Switch to the kernel's page layout just in case we haven't already
	# done it before or are touching a new task's kernel stack.
	MOV RAX, [RIP + global__kernel_page_map_base_address]
	MOV CR3, RAX

	CALL ada__get_active_task_state
	SAVE_REGISTERS_TO_STATE RAX # Save and continue to clobber if required.

	# This switches the active task.
	CALL ada__round_robin_cycle

	# We are now ready to get the new task's register state.
	CALL ada__get_active_task_state
	LOAD_REGISTERS_FROM_STATE RAX

	# If not equal, then we're returning to ring 0 from here, not the task.
	CMP WORD PTR [RSP + 8 * 4], 0x23
	JNE .L_ring_0_return

	.L_ring_3_return:
		CALL ada__get_active_task_cr3 # Get the CR3 value and load it.
		MOV CR3, RAX

		# Reset the LAPIC (hopefully in x2APIC mode). Need to do this
		# as we're not always returning to ISR 48's (LAPIC timer's)
		# interrupt stub, which signals end-of-interrupt to the LAPIC.
		XOR RAX, RAX # Just need to write zero to the register.
		XOR RDX, RDX
		MOV RCX, 0x80B # The EOI MSR index.
		WRMSR

	.L_ring_0_return:

	# This is costly, but invalidating the L1 instruction cache is a
	# countermeasure against cache attacks; however, that might not
	# actually be effective. I can't find any x86(-64) instructions which
	# only flush the L1 cache and leave other caches alone. I'll just leave
	# this instruction here purely for the sake of interest alone.
	# READ: https://arxiv.org/abs/2005.13853
	WBINVD

	REX.W IRET
