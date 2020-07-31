###############################################################################
## Program         -- HAVK Libc                                              ##
## Filename        -- Makefile                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

# This makefile's purpose is to build HAVK's libc and other files which would
# be used by user programs.

ifndef BUILD_PATH
	$(error This must be called from HAVK's operating system Makefile)
endif

# These are repeated in case you wish to independently build this user library.
LIBRARY_NAME?=havkos
LIBRARY?=$(BUILD_PATH)/lib$(LIBRARY_NAME).a
LIBRARY_BUILD_PATH=$(BUILD_PATH)/$(LIBRARY_NAME)/

# TODO: I'm still compiling freestanding programs with GCC, so I need to
# compile a version of GCC that explicitly targets HAVK's hosted
# implementation.
CC=gcc
AS=as
AR=ar

CC_FLAGS=-c -ffreestanding -nostdlib -Wall -Wextra \
	-Wpedantic -pedantic-errors -masm=intel -no-pie

AS_FLAGS=-msyntax=intel -mmnemonic=intel -mindex-reg -mnaked-reg \
	-march=generic64 -mtune=generic64 -mamd64 --64 --fatal-warnings \
	--noexecstack

AR_FLAGS=rcs

HAVK_LIBC_SOURCES:=$(wildcard ./*.c)
HAVK_LIBC_OBJECTS:=$(patsubst ./%.c, $(LIBRARY_BUILD_PATH)/%.o, \
	$(HAVK_LIBC_SOURCES))

HAVK_OS_C_SOURCES:=$(wildcard ./havk/*.c)
HAVK_OS_C_OBJECTS:=$(patsubst ./havk/%.c, $(LIBRARY_BUILD_PATH)/%.o, \
	$(HAVK_OS_C_SOURCES))

HAVK_OS_S_SOURCES:=$(wildcard ./havk/*.s)
HAVK_OS_S_OBJECTS:=$(patsubst ./havk/%.s, $(LIBRARY_BUILD_PATH)/%.o, \
	$(HAVK_OS_S_SOURCES))

.DEFAULT_GOAL: all
.PHONY: all
all: $(LIBRARY)

$(LIBRARY_BUILD_PATH):
	@if [ ! -d "$(LIBRARY_BUILD_PATH)" ]; then \
		mkdir "$(LIBRARY_BUILD_PATH)"; \
	fi

$(LIBRARY_BUILD_PATH)/%.o: %.c | $(LIBRARY_BUILD_PATH)
	$(CC) $(CC_FLAGS) -std=c17 $< -o "$@"

$(LIBRARY_BUILD_PATH)/%.o: ./havk/%.c | $(LIBRARY_BUILD_PATH)
	$(CC) $(CC_FLAGS) -std=c17 $< -o "$@"

$(LIBRARY_BUILD_PATH)/%.o: ./havk/%.s | $(LIBRARY_BUILD_PATH)
	$(AS) $(AS_FLAGS) -o "$@" $<

$(LIBRARY): $(HAVK_LIBC_OBJECTS) $(HAVK_OS_C_OBJECTS) $(HAVK_OS_S_OBJECTS)
	$(AR) $(AR_FLAGS) "$@" $^
