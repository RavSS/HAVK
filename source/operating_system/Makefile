###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- Makefile                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

# This file builds the various programs integral to HAVK's operating system.
# If a particular program gets too large, then give it its own Makefile or move
# it out of the main repository entirely.

ifndef BUILD_PATH
	$(error This must be called from HAVK's main Makefile)
endif

# TODO: I'm still using generic x86-64 targeting GCC, which only lets me create
# freestanding programs, so I need to compile a version of GCC that targets
# explicitly HAVK.
CC=gcc
LD=ld
AS=as

CC_FLAGS=-std=c11 -ffreestanding -nostdlib -I ./include/ -Wall -Wextra \
	-Werror -Wpedantic -pedantic-errors -masm=intel
LD_FLAGS=-nostdlib -static -z max-page-size=0x1000 -z common-page-size=0x1000 \
	-pie -Ttext 0x1000 -e 0x1000 -A x86-64 -b elf64-x86-64

$(BUILD_PATH)syscall_tester.bin: syscall_tester/syscall_tester.c
	$(CC) -c "$<" $(CC_FLAGS) \
		-o "$(BUILD_PATH)syscall_tester.o"
	$(LD) $(BUILD_PATH)syscall_tester.o $(LD_FLAGS) \
		--oformat=binary -o "$@"

$(BUILD_PATH)shell.elf: shell/shell.c
	$(CC) "$^" $(CC_FLAGS) -o "$@"
