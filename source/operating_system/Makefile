###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- Makefile                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 202                      ##
###############################################################################

# This file builds the various programs integral to HAVK's operating system.
# If a particular program gets too large, then give it its own Makefile or move
# it out of the main repository entirely.

ifndef BUILD_PATH
	$(error This must be called from HAVK's main Makefile)
endif

# TODO: I'm still compiling freestanding programs with GCC, so I need to
# compile a version of GCC that explicitly targets HAVK's hosted
# implementation.
CC=gcc
LD=ld
AS=as

INCLUDE=-I ./include/ -L $(BUILD_PATH)
BASELIB=$(BUILD_PATH)start.o $(BUILD_PATH)havk.o

CC_FLAGS=-c -std=c11 -ffreestanding -nostdlib -Wall -Wextra \
	-Wpedantic -pedantic-errors -masm=intel -no-pie

# TODO: A few debug flags for now (required for e.g. `addr2line`).
CC_FLAGS+= -Og -ggdb3

LD_FLAGS=-Tlinker.ld -zcommon-page-size=0x1000 -zmax-page-size=0x1000

AS_FLAGS=-msyntax=intel -mmnemonic=intel -mindex-reg -mnaked-reg \
	-march=generic64 -mtune=generic64 -mamd64 --64 --fatal-warnings \
	--noexecstack

$(BUILD_PATH)start.o: start.S # Make sure to link this first.
	$(AS) $(AS_FLAGS) -o "$@" $^

$(BUILD_PATH)havk.o: ./include/havk.c
	$(CC) $(CC_FLAGS) $(INCLUDE) -o "$@" $^

$(BUILD_PATH)thread_tester.o: $(BASELIB) thread_tester/thread_tester.c
	$(CC) $(CC_FLAGS) $(INCLUDE) -o "$@" thread_tester/thread_tester.c

$(BUILD_PATH)thread_tester.elf: $(BASELIB) $(BUILD_PATH)thread_tester.o
	$(LD) $(LD_FLAGS) -o "$@" $^

$(BUILD_PATH)framebuffer_tester.o: $(BASELIB) \
framebuffer_tester/framebuffer_tester.c
	$(CC) $(CC_FLAGS) $(INCLUDE) -o "$@" \
		framebuffer_tester/framebuffer_tester.c

$(BUILD_PATH)framebuffer_tester.elf: $(BASELIB) \
$(BUILD_PATH)framebuffer_tester.o
	$(LD) $(LD_FLAGS) -o "$@" $^
