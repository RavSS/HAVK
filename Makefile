###############################################################################
######################### HAVK - An Operating System. #########################
###############################################################################
NAME:=HAVK
VERSION=00.06-00
BUILD?=Final

GNAT_DIR:=./com/gnatgpl/bin/
BUILD_DIR:=./build/
SRC_DIR:=./src/

ifneq ($(notdir $(shell pwd)), $(NAME))
$(error Only run the Makefile from the root directory of HAVK's source 'HAVK')
endif

# Add the GNAT GPL bin folder to the beginning of the path for `gprbuild`.
# This "overrides" any current `gnat` tools you may have for `gprbuild`.
# Eliminate this line if you want to use your own GNAT installation.
export PATH:=$(PATH):$(shell pwd)/$(GNAT_DIR)

# Disable the useless built-in implicit rules here. Speeds up `make` by a lot.
MAKEFLAGS+=--no-builtin-rules --no-builtin-variables

# A normal x86-64 targeting GCC seems to be able to compile
# EFI applications, so I do not think a cross-compiler is necessary.
# Clang is also capable. You need both C and Ada enabled for GCC to build HAVK.
# GNAT GPL comes with a version of x86-64 targeting GCC, so that may be used
# as the system may not contain the exact target name. The system's current
# linker (`ld`) will be used instead of the supplied one, as that's more
# common among installations (no matter bfd or gold).
CC:=x86_64-pc-linux-gnu-gcc
LD:=ld
AS:=nasm

HAVK_PROJECT:=$(NAME).gpr
HAVK_KERNEL_ENTRY:=$(BUILD_DIR)entry.o
HAVK_BOOTLOADER:=$(BUILD_DIR)$(NAME).efi
HAVK_KERNEL:=$(BUILD_DIR)$(NAME).elf
HAVK_PARTITION:=$(BUILD_DIR)$(NAME).part
HAVK_IMAGE:=$(BUILD_DIR)$(NAME).img

OVMF_DIR=./ext/ovmf-x64/
LIB_DIR=/usr/lib/

EFI_DIR=/usr/include/efi/
EFI_CRT0=$(SRC_DIR)crt0-efi-x86_64.o
EFI_LINK=$(SRC_DIR)elf_x86_64_efi.lds

EFI_C_STD=-std=c11
EFI_C_WARN=-Wall
EFI_C_OPT=-nostdlib -fpic -fno-stack-protector -fno-strict-aliasing \
	-fno-builtin -fshort-wchar -mno-red-zone -O3 -g0
EFI_C_INC=-I $(EFI_DIR) -I $(EFI_DIR)x86_64 -I $(EFI_DIR)protocol
EFI_C_LIB=-l efi -l gnuefi
EFI_C_DEF=-D EFI_FUNCTION_WRAPPER -D VERSION=L\"$(VERSION)\"
EFI_C_FLAGS=$(EFI_C_STD) $(EFI_C_WARN) $(EFI_C_OPT) $(EFI_C_INC) \
	$(EFI_C_LIB) $(EFI_C_DEF)

EFI_LD_OPT=-nostdlib -Bsymbolic -shared -no-undefined -znocombreloc
EFI_LD_INC=-L $(LIB_DIR) -T $(EFI_LINK)
EFI_LD_LIB=-l efi -l gnuefi
EFI_LD_FLAGS=$(EFI_LD_OPT) $(EFI_LD_INC) $(EFI_CRT0)

GDB_REMOTE_DEBUG_PORT=40404
QEMU_FLAGS=-serial mon:stdio -gdb tcp::$(GDB_REMOTE_DEBUG_PORT) \
	-d guest_errors -m 1024 -cpu qemu64 -net none

EFI_NAME=boot
EFI_C_FILE=$(SRC_DIR)$(EFI_NAME).c
EFI_O_FILE=$(BUILD_DIR)$(EFI_NAME).o
EFI_SO_FILE=$(BUILD_DIR)$(EFI_NAME).so

# Change these if you want a bigger disk image etc. It assumes that
# `parted` treats the sector size as 512 bytes. Since I don't need all
# the space that FAT32 requires as a minimum, and that `parted` does not
# support FAT12, I've gone with FAT16 for now so I don't waste your space.
FAT_SIZE=16
IMAGE_BLOCK_SIZE=512
IMAGE_SECTORS=6000

# The bootable EFI system partition's sector details.
ESP_SECTOR_START=50
ESP_SECTOR_END=5000
ESP_SECTOR_SIZE=4950

# Enable/uncomment this when debugging.
FORCE_RECOMPILE=-f -we

define echo
@echo -e "\033[4;96m          $1\033[0m"
endef

.DEFAULT_GOAL: all
.PHONY: all
all: $(HAVK_IMAGE)

$(BUILD_DIR):
	if [ ! -d "$@" ]; then mkdir $@; fi

$(EFI_O_FILE): $(EFI_C_FILE) | $(BUILD_DIR)
	$(call echo, "BUILDING BOOTLOADER TO $(HAVK_BOOTLOADER)") && \
	$(CC) $(EFI_C_FLAGS) -c $< -o $@

$(EFI_SO_FILE): $(EFI_O_FILE)
	$(LD) $(EFI_LD_FLAGS) $< -o $@ $(EFI_LD_LIB)

$(HAVK_BOOTLOADER): $(EFI_SO_FILE)
	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc  -j .debug_info -j .debug_abbrev \
		-j .debug_loc -j .debug_aranges -j .debug_line \
		-j .debug_macinfo -j .debug_str --target=efi-app-x86_64 \
		$< $@.debug

	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc --target=efi-app-x86_64 \
		$< $@

$(HAVK_KERNEL_ENTRY): $(SRC_DIR)entry.asm
	$(AS) -f elf64 -F dwarf $< -o $@

$(HAVK_KERNEL): $(HAVK_KERNEL_ENTRY)
	$(call echo, "BUILDING HAVK TO $@")

	gprbuild -P $(HAVK_PROJECT) -d -j$(shell nproc --all) -s \
		$(FORCE_RECOMPILE) -XBuild=$(BUILD) -o $@

$(HAVK_PARTITION): $(HAVK_BOOTLOADER) $(HAVK_KERNEL)
	$(call echo, "CREATING EFI SYSTEM PARTITION")

	dd if=/dev/zero of=$@ bs=$(IMAGE_BLOCK_SIZE) count=$(ESP_SECTOR_SIZE)
	mkfs.fat -v -F $(FAT_SIZE) -s 1 -S $(IMAGE_BLOCK_SIZE) $@

	mmd -i $@ ::/EFI
	mmd -i $@ ::/EFI/BOOT
	mmd -i $@ ::/$(NAME)

	mcopy -i $@ $(HAVK_BOOTLOADER) ::/EFI/BOOT/BOOTX64.EFI
	mcopy -i $@ $(HAVK_KERNEL) ::/$(NAME)/$(NAME).elf

$(HAVK_IMAGE): $(HAVK_PARTITION)
	$(call echo, "CREATING IMAGE TO $@")

	dd if=/dev/zero of=$@ bs=$(IMAGE_BLOCK_SIZE) count=$(IMAGE_SECTORS)

	parted $@ --align optimal --script \
		mktable gpt \
		mkpart primary fat$(FAT_SIZE) \
			$(ESP_SECTOR_START)s $(ESP_SECTOR_END)s \
		name 1 EFI \
		set 1 esp on

	dd if=$< of=$@ bs=$(IMAGE_BLOCK_SIZE) count=$(ESP_SECTOR_SIZE) \
		seek=$(ESP_SECTOR_START) conv=notrunc

# (CTRL + A, C) to exit serial and enter monitor, and vice versa.
.PHONY: qemu
qemu: $(HAVK_IMAGE)
	$(call echo, "LOADING QEMU WITH $<")

	qemu-system-x86_64 \
	-drive if=pflash,format=raw,unit=0,\
	file=$(OVMF_DIR)OVMF_CODE-pure-efi.fd,readonly=on \
	-drive if=pflash,format=raw,unit=1,\
	file=$(OVMF_DIR)OVMF_VARS-pure-efi.fd,readonly=off \
	-drive index=0,format=raw,media=disk,\
	file=$<,readonly=off \
	$(QEMU_FLAGS)

	@tput sgr0 # Corrected terminal after serial console usage.

.PHONY: gdb
gdb:
	-gdb $(HAVK_KERNEL) \
		-ex "target remote :$(GDB_REMOTE_DEBUG_PORT)" \
		-ex "continue"

.PHONY: clean
clean: $(BUILD_DIR)
	rm -vr $<
