###############################################################################
##################### HAVK - An Operating System & Kernel. ####################
###############################################################################
NAME:=HAVK

# Currently, there are two build levels: "Final" and "Debug". Case sensitive.
BUILD?=Debug

ifeq ("$(BUILD)", "Debug")
	VERSION=UPCOMING
else
	VERSION=$(shell git describe --abbrev=0 --tags)
endif

# This only has any effect when the build is Debug. As of now, this
# only controls the additional test functions added in to the UEFI bootloader,
# and there is two levels: 1 for compiler-related switches, 2 for code changes.
DEBUG_LEVEL?=1

GNAT_DIR:=./com/gnatgpl_linux/bin/
BUILD_DIR:=./build/
SRC_DIR:=./src/

# Not the best solution, but should stop anyone from running the Makefile
# outside its directory, as that could be dangerous. Looks for SRC_DIR's path.
ifeq ("$(wildcard $(SRC_DIR))", "")
	$(error Only run the Makefile from the root directory of HAVK's source)
endif

# Add the GNAT GPL bin folder to the beginning of the path for `gprbuild`.
# This "overrides" any current GNAT tools you may have for `gprbuild`.
# Eliminate this line if you want to use your own GNAT installation.
export PATH:=$(PATH):$(shell pwd)/$(GNAT_DIR)

# Disable the useless built-in implicit rules here. Speeds up `make` by a lot.
# Try making the kernel without this below line via `make -dB`, and then
# compare it with the line present in this file. Quite the difference.
MAKEFLAGS+=--no-builtin-rules --no-builtin-variables

# A normal x86-64 targeting GCC seems to be able to compile
# EFI applications, so I do not think a cross-compiler is necessary.
# Clang is also capable. You need both C and Ada enabled for GCC to build HAVK.
# GNAT GPL comes with a version of x86-64 targeting GCC, so that may be used
# as the system may not use the exact same target. The system's current
# linker (`ld`) will be used instead of the supplied one, as that's more
# common among installations (no matter bfd or gold).
CC:=gcc
LD:=ld
AS:=nasm

HAVK_PROJECT:=$(SRC_DIR)$(NAME).gpr
HAVK_RUNTIME:=$(SRC_DIR)$(NAME)_RTS.gpr

# Copy the source directory to the directory structure GNAT demands.
HAVK_RUNTIME_DIR:=$(SRC_DIR)runtime/
HAVK_ADAINCLUDE_DIR:=$(BUILD_DIR)adainclude/
HAVK_ADALIB_DIR:=$(BUILD_DIR)adalib/

HAVK_BOOTLOADER:=$(BUILD_DIR)$(NAME).efi
HAVK_LIBRARY:=$(BUILD_DIR)lib$(NAME).a
HAVK_KERNEL:=$(BUILD_DIR)$(NAME).elf

HAVK_PARTITION:=$(BUILD_DIR)$(NAME).part
HAVK_IMAGE:=$(BUILD_DIR)$(NAME).img

OVMF_DIR=./ext/ovmf-x64/
LIB_DIR=/usr/lib/

EFI_DIR=/usr/include/efi/
EFI_SRC_DIR=$(SRC_DIR)uefi/
EFI_CRT0=$(EFI_SRC_DIR)crt0-efi-x86_64.o
EFI_LINK=$(EFI_SRC_DIR)elf_x86_64_efi.lds

# Note that I create two EFI files, but one still has the debug symbols kept
# in it. For better analyzing, it's wise to use the same optimisation level
# for both of the bootloader application's files.
EFI_C_STD=-std=c11 # Intel ASM syntax does not work with GNU-EFI functionality.
EFI_C_WARN=-Wall -Wextra
EFI_C_OPT=-nostdlib -fpic -fno-stack-protector -fno-strict-aliasing \
	-fno-builtin -fshort-wchar -mno-red-zone
EFI_C_INC=-I $(EFI_DIR) -I $(EFI_DIR)x86_64 -I $(EFI_DIR)protocol
EFI_C_LIB=-l efi -l gnuefi
EFI_C_DEF=-D EFI_FUNCTION_WRAPPER -D HAVK_VERSION=L\"$(VERSION)\"
EFI_C_FLAGS=$(EFI_C_STD) $(EFI_C_WARN) $(EFI_C_OPT) $(EFI_C_INC) \
	$(EFI_C_LIB) $(EFI_C_DEF)

# The offset values below are estimations.
EFI_TEXT_SECTION_OFFSET=0x3000
EFI_DATA_SECTION_OFFSET=0xC000

ifeq ("$(DEBUG_LEVEL)", "2")
	EFI_C_DEF+= -D HAVK_GDB_DEBUG
	EFI_C_DEF+= -D TEXT_OFFSET=$(EFI_TEXT_SECTION_OFFSET)
	EFI_C_DEF+= -D DATA_OFFSET=$(EFI_DATA_SECTION_OFFSET)
endif

ifneq ("$(EFI_IMAGE_BASE)", "")
	EFI_TEXT_SECTION=$$(( $(EFI_IMAGE_BASE) + $(EFI_TEXT_SECTION_OFFSET)))
	EFI_DATA_SECTION=$$(($(EFI_IMAGE_BASE) + $(EFI_DATA_SECTION_OFFSET)))
endif

EFI_LD_OPT=-nostdlib -Bsymbolic -shared -no-undefined -znocombreloc
EFI_LD_INC=-L $(LIB_DIR) -T $(EFI_LINK)
EFI_LD_LIB=-l efi -l gnuefi
EFI_LD_FLAGS=$(EFI_LD_OPT) $(EFI_LD_INC) $(EFI_CRT0)

GDB_REMOTE_DEBUG_PORT?=40404
QEMU_FLAGS=-serial mon:stdio -gdb tcp::$(GDB_REMOTE_DEBUG_PORT) \
	-d guest_errors -m 1024 -cpu qemu64,check -net none \
	-no-reboot -no-shutdown

ifeq ("$(INT)", "1")
	QEMU_FLAGS+= -d int,cpu_reset
endif

EFI_NAME=boot
EFI_C_FILE=$(EFI_SRC_DIR)$(EFI_NAME).c
EFI_O_FILE=$(BUILD_DIR)$(EFI_NAME).o
EFI_SO_FILE=$(BUILD_DIR)$(EFI_NAME).so

# Change these if you want a bigger disk image etc. It assumes that
# `parted` treats the sector size as 512 bytes. Since I don't need all
# the space that FAT32 requires as a minimum, and that `parted` does not
# support FAT12, I've gone with FAT16 for now so I don't waste your space.
FAT_SIZE=16
IMAGE_BLOCK_SIZE=512
IMAGE_SECTORS=10000

# The bootable EFI system partition's sector details. I store the kernel
# in there as well, because why not.
ESP_SECTOR_START=50
ESP_SECTOR_END=9500
ESP_SECTOR_SIZE=9550

# When debugging, consider warnings as errors and be more verbose.
# Also control the optimisation here so it's easier to tweak.
ifeq ("$(BUILD)", "Debug")
	O?=g
	EFI_C_OPT+= -ggdb3 -O$(O)
	GPR_RTS_FLAGS=-we -k -d -O$(O)
	GPR_KERNEL_FLAGS=-we -k -d -O$(O)
else
	O?=2
	EFI_C_OPT+= -g0 -O$(O)
	GPR_RTS_FLAGS=-q -vP0 -O$(O)
	GPR_KERNEL_FLAGS=-q -vP0 -O$(O)
endif

ifneq ("$(PROVE_FILES)", "")
	override PROVE_FILES:=-u $(PROVE_FILES)
endif

define echo
	@echo -e "\033[4;96m          $1\033[0m"
endef

.DEFAULT_GOAL: all
.PHONY: all
all: $(HAVK_IMAGE)

$(BUILD_DIR):
	@if [ ! -d "$@" ]; then mkdir $@; fi
	@if [ ! -d "$(HAVK_ADALIB_DIR)" ]; then mkdir $(HAVK_ADALIB_DIR); fi

# TODO: If any one of the runtime system's files are changed, a force or clean
# build is necessary. This can be resolved.
$(HAVK_ADAINCLUDE_DIR): | $(BUILD_DIR)
	@if [ -d "$@" ]; then rm -r $@; fi
	@mkdir $@
	@cp $(HAVK_RUNTIME_DIR)* $@

$(EFI_O_FILE): $(EFI_C_FILE) | $(BUILD_DIR)
	$(call echo, "BUILDING BOOTLOADER TO $(HAVK_BOOTLOADER)")

	$(CC) $(EFI_C_FLAGS) -c $< -o $@

$(EFI_SO_FILE): $(EFI_O_FILE)
	$(call echo, "LINKING EFI BOOTLOADER APPLICATION")

	$(LD) $(EFI_LD_FLAGS) $< -o $@ $(EFI_LD_LIB)

$(HAVK_BOOTLOADER): $(EFI_SO_FILE)
	$(call echo, "FIXING GNU-EFI UTILIZING BOOTLOADER")

	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc  -j .debug_info -j .debug_abbrev \
		-j .debug_loc -j .debug_aranges -j .debug_line \
		-j .debug_macinfo -j .debug_str --target=efi-app-x86_64 \
		$< $@.debug;

	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc --target=efi-app-x86_64 \
		$< $@

$(HAVK_LIBRARY): | $(HAVK_ADAINCLUDE_DIR) $(BUILD_DIR)
	$(call echo, "BUILDING THE HAVK RUNTIME TO $@")

	gprbuild -P $(HAVK_RUNTIME) -XBuild=$(BUILD) -p -eL \
		$(GPR_RTS_FLAGS) -j0 -s -o ./../$@

$(HAVK_KERNEL): $(HAVK_LIBRARY) | $(BUILD_DIR)
	$(call echo, "BUILDING THE HAVK KERNEL TO $@")

	gprbuild -P $(HAVK_PROJECT) -XBuild=$(BUILD) -p -eL \
		$(GPR_KERNEL_FLAGS) -j0 -s -o ./../$@

$(HAVK_PARTITION): $(HAVK_BOOTLOADER) $(HAVK_KERNEL)
	$(call echo, "CREATING EFI SYSTEM PARTITION FILE")

	dd if=/dev/zero of=$@ bs=$(IMAGE_BLOCK_SIZE) count=$(ESP_SECTOR_SIZE)
	mkfs.fat -v -F $(FAT_SIZE) -s 1 -S $(IMAGE_BLOCK_SIZE) $@

	mmd -i $@ ::/EFI
	mmd -i $@ ::/EFI/BOOT
	mmd -i $@ ::/$(NAME)

	mcopy -i $@ $(HAVK_BOOTLOADER) ::/EFI/BOOT/BOOTX64.EFI
	mcopy -i $@ $(HAVK_KERNEL) ::/$(NAME)/$(NAME).elf

$(HAVK_IMAGE): $(HAVK_PARTITION)
	$(call echo, "CREATING IMAGE AT $@")

	dd if=/dev/zero of=$@ bs=$(IMAGE_BLOCK_SIZE) count=$(IMAGE_SECTORS)

	parted $@ --align minimal --script \
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
	-@gdb $(HAVK_KERNEL) -q \
		-ex "set confirm off" \
		-ex "set architecture i386:x86-64:intel" \
		-ex "set max-value-size 10485760" \
		-ex "set tcp auto-retry on" \
		-ex "set tcp connect-timeout 300" \
		-ex "target remote :$(GDB_REMOTE_DEBUG_PORT)" \
		-ex "continue"

.PHONY: uefi-gdb
uefi-gdb:
	@if [ -z "$(EFI_IMAGE_BASE)" ]; \
	then \
		echo "Set the 'UEFI_IMAGE_BASE' variable to"; \
		echo "the appropriate value when calling Make for"; \
		echo "the UEFI GDB commands to work as intended."; \
		exit 1; \
	fi

	-@gdb $(HAVK_BOOTLOADER) -q \
		-ex "set confirm off" \
		-ex "set architecture i386:x86-64:intel" \
		-ex "set max-value-size 10485760" \
		-ex "set tcp auto-retry on" \
		-ex "set tcp connect-timeout 300" \
		-ex "add-symbol-file $(HAVK_BOOTLOADER).debug \
			$(EFI_TEXT_SECTION) -s .data $(EFI_DATA_SECTION)" \
		-ex "target remote :$(GDB_REMOTE_DEBUG_PORT)" \
		-ex "set gdb_ready=1" \
		-ex "continue"

.PHONY: proof
proof: $(BUILD_DIR)
	$(call echo, "PROVING HAVK KERNEL\'S CORRECTNESS")

	@gnatprove -P $(HAVK_PROJECT) -XBuild=$(BUILD) $(PROVE_FILES) -j0 -k \
		--assumptions --pedantic --cwe --level=4 --mode=all

.PHONY: stats
stats:
	$(call echo, "HAVK KERNEL STATISTICS")

	@echo -n "Makefile kernel image size capacity: $(shell du -sh \
		$(HAVK_KERNEL) | awk -F '\t' '{print $$1}')"

	@echo -e " / $(shell du -sh $(HAVK_PARTITION) \
		| awk -F '\t' '{print $$1}')\n"

	@cd $(SRC_DIR) && gnatmetric -P HAVK.gpr -XBuild=$(BUILD) -U -eL \
		--contract-all --syntax-all --lines-all --lines-spark \
		--complexity-all

.PHONY: clean
clean: $(BUILD_DIR) $(HAVK_ADAINCLUDE_DIR)
	$(call echo, "CLEANING BUILD DIRECTORY $<")

	@gnatprove -P $(HAVK_PROJECT) -XBuild=$(BUILD) --clean
	@gprclean -P $(HAVK_RUNTIME) -XBuild=$(BUILD) -q
	@gprclean -P $(HAVK_PROJECT) -XBuild=$(BUILD) -q
	@rm -vr $<
