CCDIR=crosscc/bin/
CC=$(CCDIR)i686-elf-gcc
AS=nasm
#$(CCDIR)i686-elf-as $(SRCDIR)boot.s -o $(BUILDDIR)boot.o

NAME=HAVK
VERSION=\"1.00-00 (TESTING)\"
HAVK=$(NAME).iso

# Enabling optimization forces inline functions, which mess with the stack protector.
OPT=-O0 -fno-inline
#-fstack-protector-all
WARN=-Wall -Wextra -pedantic
LIB=-lgcc
DEF=-D VERSION=$(VERSION)
STD=-std=c99 -masm=intel
CFLAGS=$(OPT) $(WARN) $(LIB) $(DEF) $(STD)

SRCDIR=./src/
BUILDDIR=./build/
INCLUDEDIR=-I ./src/include/
IMGDIR=./img/

ASMFILE=$(SRCDIR)%.asm
ASMFILES=$(wildcard $(SRCDIR)*.asm)

CFILE=$(SRCDIR)havk/%.c
CFILES=$(wildcard $(SRCDIR)havk/*.c)

OBJFILE=$(BUILDDIR)%.o
# System V ABI stuff.
CRTPREV=$(BUILDDIR)crti.o $(shell $(CC) -lgcc -print-file-name=crtbegin.o)
CRTPOST=$(shell $(CC) -lgcc -print-file-name=crtend.o) $(BUILDDIR)crtn.o

LINKEDFILE=$(IMGDIR)boot/$(NAME).bin

LINKFILES2=\
$(BUILDDIR)boot.o \
$(patsubst $(SRCDIR)havk/%.c, $(BUILDDIR)%.o, $(wildcard $(SRCDIR)havk/*.c)) \

# Link "boot.o" first or there will be confusion when failing to boot...
LINKFILES=\
$(CRTPREV) \
$(BUILDDIR)boot.o \
$(patsubst $(SRCDIR)havk/%.c, $(BUILDDIR)%.o, $(wildcard $(SRCDIR)havk/*.c)) \
$(CRTPOST)\

# Makes it easier to see what's happening during the make.
define echo
	@tput setaf 6
	@tput bold
	@echo -n "============================== "
	@echo $1
	@tput sgr0
endef

.DEFAULT_GOAL: all
.PHONY: all
all: $(HAVK)

.PHONY: clean
clean:
	if [ ! -d "$(BUILDDIR)" ]; then mkdir $(BUILDDIR);\
	else rm -r $(BUILDDIR) && mkdir $(BUILDDIR); fi

$(OBJFILE): $(ASMFILE)
	$(call echo, "ASSEMBLING '$^'")
	$(AS) -f elf32 $< -o $@

$(OBJFILE): $(CFILE)
	$(call echo, "COMPILING '$^'")
	$(CC) -ffreestanding $(WARN) $(STD) $(OPT) $(INCLUDEDIR) -c $< -o $@

$(LINKEDFILE): $(LINKFILES)
	$(call echo, "LINKING EVERYTHING TO '$@'")
	$(CC) -nostdlib -ffreestanding $(WARN) -T $(SRCDIR)linker.ld $^ -o $@

$(HAVK): $(LINKEDFILE)
	$(call echo, "CREATING BOOTABLE '$@'")
	grub-mkrescue -o $@ $(IMGDIR)

.PHONY: qemu-iso
qemu-iso: $(HAVK)
	qemu-system-i386 -boot d -cdrom $< -m 1024 -vnc :1 -monitor stdio -s

.PHONY: qemu-kernel
qemu-kernel: $(LINKEDFILE)
	qemu-system-i386 -boot d -kernel $< -m 1024 -vnc :1 -monitor stdio -s
