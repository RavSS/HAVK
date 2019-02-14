CCDIR=crosscc/bin/
CC=$(CCDIR)i686-elf-gcc
AS=nasm

NAME=HAVK
VERSION=\"0.03-00\"
HAVK=$(NAME).iso

# Enabling optimization forces inline functions no matter what,
# which mess with the stack protector (-fstack-protector-all).
OPT=-O0 -fno-inline
WARN=-Wall -Wextra -pedantic
LIB=-lgcc
DEF=-D VERSION=$(VERSION)
STD=-std=c99 -masm=intel -m32
CFLAGS=$(WARN) $(DEF) $(STD) $(LIB) $(OPT) $(INCLUDEDIR)

SRCDIR=./src/
BUILDDIR=./build/
INCLUDEDIR=-I ./src/include/
IMGDIR=./img/

# Disable the useless built-in implicit rules here, speeds up `make` by a lot.
MAKEFLAGS += --no-builtin-rules --no-builtin-variables

ASMFILE=$(SRCDIR)%.asm
ASMFILES=$(wildcard $(SRCDIR)*.asm)

CFILE=$(SRCDIR)havk/%.c
CFILES=$(wildcard $(SRCDIR)havk/*.c)

OBJFILE=$(BUILDDIR)%.o

# System V ABI stuff.
CRTPREV=$(BUILDDIR)crti.o $(shell $(CC) $(LIB) -print-file-name=crtbegin.o)
CRTPOST=$(shell $(CC) $(LIB) -print-file-name=crtend.o) $(BUILDDIR)crtn.o

# Just specifying '-lgcc' doesn't work for some reason.
# This is needed so I can get access to stuff like 64-bit types
# on a 32-bit kernel e.g. `int64_t`. It of course must be linked.
LIBGCC=$(shell $(CC) $(LIB) -print-libgcc-file-name)

LINKEDFILE=$(IMGDIR)boot/$(NAME).bin
# Link "boot.o" first or there will be confusion when failing to boot.
# Link "libgcc.a" at the end of the kernel objects as the kernel will
# depend on its functions.
LINKFILES=\
$(CRTPREV) \
$(BUILDDIR)boot.o \
$(patsubst $(SRCDIR)havk/%.c, $(BUILDDIR)%.o, $(wildcard $(SRCDIR)havk/*.c)) \
$(LIBGCC) \
$(CRTPOST)\

# Makes it easier to see what's happening during the make.
# Doesn't line up with errors/warnings
# when `make` has several jobs via '-j'.
define echo
	@echo "\033[4;96m             $1\033[0m"
endef

.DEFAULT_GOAL: all
.PHONY: all
all: $(LINKEDFILE)

.PHONY: clean
clean:
	if [ ! -d "$(BUILDDIR)" ]; then mkdir $(BUILDDIR);\
	else rm -r $(BUILDDIR) && mkdir $(BUILDDIR); fi

$(OBJFILE): $(ASMFILE)
	$(call echo, "ASSEMBLING '$^'") &&\
	$(AS) -f elf32 $< -o $@

# New options allows usage of special interrupt handling functions.
$(BUILDDIR)interrupts.o: $(SRCDIR)havk/interrupts.c
	$(call echo, "COMPILING '$^'") &&\
	$(CC) -ffreestanding -mno-80387 -mgeneral-regs-only $(CFLAGS) -c $< -o $@

$(OBJFILE): $(CFILE)
	$(call echo, "COMPILING '$^'") &&\
	$(CC) -ffreestanding $(CFLAGS) -c $< -o $@

$(LINKEDFILE): $(LINKFILES)
	$(call echo, "LINKING EVERYTHING TO '$@'") &&\
	$(CC) -nostdlib -ffreestanding $(LIB) $(WARN) -T $(SRCDIR)linker.ld $^ -o $@

$(HAVK): $(LINKEDFILE)
	$(call echo, "CREATING BOOTABLE '$@'") &&\
	grub-mkrescue -o $@ $(IMGDIR)

.PHONY: qemu-iso
qemu-iso: $(HAVK)
	qemu-system-i386 -boot d -cdrom $< -m 1024 -vnc :0 -monitor stdio -s

.PHONY: bochs
bochs: $(HAVK)
	bochs -f havk_bochsrc.txt

.PHONY: qemu-gdb
qemu-gdb: $(LINKEDFILE)
	-gdb -ex "file $<" -ex "target remote localhost:1234"

.PHONY: qemu-kernel
qemu-kernel: $(LINKEDFILE)
	qemu-system-i386 -boot d -kernel $< -m 1024 -vnc :0 -monitor stdio -s
