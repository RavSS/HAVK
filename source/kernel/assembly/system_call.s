###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- system_call.s                                          ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2021                ##
###############################################################################

.INCLUDE "macros.s"

.SECTION .rodata

# Instead of using the address attribute in Ada, I'll create a pointer here.
# Reduces the amount of code needed to be written or any SPARK mode exclusions.
.GLOBAL assembly__system_call_entry_address
assembly__system_call_entry_address:
	.QUAD assembly__system_call_entry

.SECTION .isolated_text

.GLOBAL assembly__system_call_entry
.TYPE assembly__system_call_entry, @function
# (RCX => return address, R11 => previous RFLAGS value, OTHERS => specific to
#  the system call)
assembly__system_call_entry:
	# The stack is not changed upon entry, we currently have the task's
	# stack loaded into RSP, so change it for the system call stack.

	# Get the task's IST 1 stack inside the TSS. Don't clobber RSP and RBP
	# just yet, as we need to store their stack pointer and their stack
	# frame pointer. Use R10 as the temporary register.
	MOV R10, [RIP + ada__task_state_segment + 36]

	SUB R10, 8 # Create enough room for a pointer.
	MOV [R10], RSP # Save their stack.
	MOV RSP, R10 # Now we're using the task's kernel stack.

	# Save the registers used for context saving purposes.
	PUSH RAX
	MOV RAX, CR3
	PUSH RAX
	PUSH RBX

	# Full view of the kernel memory.
	MOV R10, [RIP + assembly__kernel_page_map_base_address]
	MOV CR3, R10

	# The `SYSCALL` instruction should only ever be called from ring 3, so
	# we already know that we need to save the state to the ring 3 one.
	MOV RAX, [RIP + ada__ring_3_tasking_context]

	M_TASK_STATE SAVE STACK_TRANSFERS=0 # Manual stack saves.
	M_STACK_TRANSFER SAVE RAX
	M_STACK_TRANSFER SAVE CR3
	M_STACK_TRANSFER SAVE RBX
	# Don't bother saving the segment selector registers.

	# Now do some corrections for the `SYSCALL` instructions oddities,
	# along with saving their stack pointer. Effectively double saving
	# some of these values i.e. RCX is saved twice to the RCX state and
	# the RIP state.
	MOV RBX, [RSP + 24] # User's RSP.
	MOV [RAX + RSP_STATE], RBX
	MOV [RAX + RIP_STATE], RCX # User's RIP.
	MOV [RAX + RFLAGS_STATE], R11 # User's RFLAGS.

	MOV RDI, 1 # System call mode.
	CALL ada__set_active_task_state_mode

	CALL ada__system_call_handler # Handle the system call.

	# In the future, I may want to return to ring 0 from here for whatever
	# purpose, so I'm not using the double pointer for the ring 3 state.
	CALL ada__get_active_task_state

	# Refresh the stack in case it changed.
	MOV RSP, [RIP + ada__task_state_segment + 36]
	SUB RSP, 256

	M_TASK_STATE LOAD

	POP RBX # The user's RBX value.
	POP RAX # The user's CR3 value.
	MOV CR3, RAX
	POP RAX # The user's RAX value.

	# As mentioned before, I may return to ring 0 from here. This is very
	# unlikely, but I'm going to keep this functionality just in case
	# instead of using `REX.W SYSRET`. This is slower, so when
	# optimisation is finally an issue, multiple entry+return paths will
	# have to be used for the system call handler.
	REX.W IRET

	# Regarding the `REX.W SYSRET` instruction: the address `SYSCALL` was
	# executed from can only ever be a canonical address, so `REX.W
	# SYSRET` will only ever return to that address. There's an
	# interesting vulnerability with certain CPUs where the check for a
	# canonical address happens in ring 0 instead of ring 3, causing the
	# GPF in the former. That makes it a kernel issue instead of a user
	# program issue. I don't think that can be a problem here, but please
	# do verify and check for yourself. Also note that there's only a
	# single `SYSCALL`, but there's two `SYSRET` versions. Without the REX
	# prefix, it returns us to 32-bit operation. I have zero intentions of
	# supporting protected/compatibility mode.
