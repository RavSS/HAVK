###############################################################################
## Program         -- HAVK Operating System                                  ##
## Filename        -- Makefile                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

# This file builds the various programs integral to HAVK's operating system.
# If a particular program gets too large, then give it its own Makefile or move
# it out of the main repository entirely.

ifndef BUILD_PATH
	$(error This must be called from HAVK's main Makefile)
endif

SYSTEM_FILES?=\
	initialiser \
	ipc_tester \
	thread_tester \
	framebuffer_tester

# TODO: I'm still compiling freestanding programs with GCC, so I need to
# compile a version of GCC that explicitly targets HAVK's hosted
# implementation.
CC=gcc
LD=ld
AS=as

INCLUDE=-I ./include/

LIBRARY_NAME?=havkos
LIBRARY=$(BUILD_PATH)/lib$(LIBRARY_NAME).a

CC_FLAGS=-c -std=c11 -ffreestanding -nostdlib -Wall -Wextra \
	-Wpedantic -pedantic-errors -masm=intel -no-pie \
	-L $(BUILD_PATH) -l$(LIBRARY_NAME)

# TODO: A few debug flags for now (required for e.g. `addr2line`).
CC_FLAGS+= -Og -ggdb3

LD_FLAGS=-Tlinker.ld -zcommon-page-size=0x1000 -zmax-page-size=0x1000 -static \
	-L $(BUILD_PATH) -l$(LIBRARY_NAME)

AS_FLAGS=-msyntax=intel -mmnemonic=intel -mindex-reg -mnaked-reg \
	-march=generic64 -mtune=generic64 -mamd64 --64 --fatal-warnings \
	--noexecstack

.DEFAULT_GOAL: all
.PHONY: all
all: $(addprefix $(BUILD_PATH), $(addsuffix .elf, $(SYSTEM_FILES)))

$(LIBRARY):
	@$(MAKE) -C ./include/ "$@"

$(BUILD_PATH)%.o: */%.c
	$(CC) $(CC_FLAGS) $(INCLUDE) -o "$@" $<

$(BUILD_PATH)%.elf: $(BUILD_PATH)%.o $(LIBRARY)
	$(LD) $(LD_FLAGS) -o "$@" $^
