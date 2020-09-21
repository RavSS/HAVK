###############################################################################
## Program         -- HAVK                                                   ##
## Filename        -- Makefile                                               ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2019-2020                ##
###############################################################################

NAME=HAVK

# Currently, there are two build levels: "Final" and "Debug". Case sensitive.
BUILD?=Final

ifeq ("$(BUILD)", "Debug")
	VERSION=UPCOMING
else
	VERSION=$(shell git name-rev --tags --name-only --no-undefined HEAD \
		2> /dev/null | grep '^V[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}$$' \
		|| echo -n UNKNOWN)
endif

$(info Build: $(VERSION)-$(BUILD) at $(shell date).)

# This only has any effect when the build is Debug. As of now, this
# only controls the additional test functions added in to the UEFI bootloader,
# and there is two levels: 1 for compiler-related switches, 2 for code changes.
DEBUG_LEVEL?=1

GNAT_PATH?=./compilers/gnat_gpl_linux/bin/
BUILD_PATH=./build/
SOURCE_PATH=./source/

# Add the GNAT GPL bin folder to the beginning of the path for `gprbuild`.
# This "overrides" any current GNAT tools you may have for `gprbuild`.
# Eliminate this line if you want to use your own GNAT installation.
export PATH:=$(shell pwd)/$(GNAT_PATH):$(PATH)

# Disable the useless built-in implicit rules here. Speeds up `make` by a lot.
# Try making the kernel without this below line via `make -dB`, and then
# compare it with the line present in this file. Quite the difference.
MAKEFLAGS+=--no-builtin-rules --no-builtin-variables

# A normal x86-64 targeting GCC seems to be able to compile
# UEFI applications, so I do not think a cross-compiler is necessary.
# Clang is also capable. You need both C and Ada enabled for GCC to build HAVK.
# GNAT GPL comes with a version of x86-64 targeting GCC, so that may be used
# as the system may not use the exact same target. The system's current
# linker (`ld`) will be used instead of the supplied one, as that's more
# common among installations (no matter bfd or gold).
CC=gcc
LD=ld
AS=as

KERNEL_BUILD_PATH=$(BUILD_PATH)kernel/
KERNEL_SOURCE_PATH=$(SOURCE_PATH)kernel/
KERNEL_PROJECT=$(KERNEL_SOURCE_PATH)$(NAME)_Kernel.gpr
KERNEL_RUNTIME_PROJECT=$(KERNEL_SOURCE_PATH)$(NAME)_Kernel_Runtime.gpr

# Not the best solution, but should stop anyone from running the Makefile
# outside its directory, as that could be dangerous. Finds the kernel GPR file.
ifeq ("$(wildcard $(KERNEL_PROJECT))", "")
	$(error Only run Make from the root directory of HAVK's repository)
endif

# Copy the source directory to the directory structure GNAT demands.
KERNEL_RUNTIME_PATH=$(KERNEL_SOURCE_PATH)ada_runtime/
KERNEL_ADAINCLUDE_PATH=$(KERNEL_BUILD_PATH)adainclude/
KERNEL_ADALIB_PATH=$(KERNEL_BUILD_PATH)adalib/

UEFI_BOOTLOADER=$(BUILD_PATH)$(NAME).efi
KERNEL_LIBRARY=$(KERNEL_ADALIB_PATH)libhavk_kernel_runtime.a
KERNEL=$(KERNEL_BUILD_PATH)$(NAME).elf

IMAGE?=$(BUILD_PATH)$(NAME).img
VMDK=$(BUILD_PATH)$(NAME).vmdk

OVMF_PATH=./tools/ovmf-x64/
LIB_PATH=/usr/lib/

UEFI_PATH=/usr/include/efi/
UEFI_SOURCE_PATH=$(SOURCE_PATH)bootloader/
UEFI_CRT0=$(UEFI_SOURCE_PATH)crt0-efi-x86_64.o
UEFI_LINK=$(UEFI_SOURCE_PATH)elf_x86_64_efi.lds

# This controls the bootloader configuration file on our disk and the location
# in the generated image.
# TODO: The location to seek is found in the bootloader source code itself, as
# it's annoying to define the symbol from here due to backslashes.
UEFI_BOOTLOADER_CONFIGURATION=$(UEFI_SOURCE_PATH)boot.cfg
UEFI_BOOTLOADER_LOCATION=/EFI/BOOT/BOOT.CFG # Same UEFI naming style.

# Note that I create two UEFI files, but one still has the debug symbols kept
# in it. For better analyzing, it's wise to use the same optimisation level
# for both of the bootloader application's files. Global Intel assembly syntax
# does not work with the GNU-EFI library. Also note that UEFI and EFI refer to
# the same concept, with the latter being the old name kept for compatibility.
UEFI_CC_STD=-std=c17
UEFI_CC_WARN=-Wall -Wextra -Werror
UEFI_CC_OPT=-nostdlib -fpic -fno-stack-protector -fno-strict-aliasing \
	-fno-builtin -fshort-wchar -mno-red-zone -funsigned-char
UEFI_CC_INC=-I $(UEFI_PATH) -I $(UEFI_PATH)x86_64 -I $(UEFI_PATH)protocol \
	-I $(UEFI_SOURCE_PATH)
UEFI_CC_LIB=-l efi -l gnuefi
UEFI_CC_DEF=-D EFI_FUNCTION_WRAPPER -D HAVK_VERSION=u\"$(VERSION)\"
UEFI_CC_FLAGS=$(UEFI_CC_STD) $(UEFI_CC_WARN) $(UEFI_CC_OPT) $(UEFI_CC_INC) \
	$(UEFI_CC_LIB) $(UEFI_CC_DEF)

# The offset values below are estimations.
UEFI_TEXT_SECTION_OFFSET=0x3000
UEFI_DATA_SECTION_OFFSET=0xC000

ifeq ("$(DEBUG_LEVEL)", "2")
	UEFI_CC_DEF+= -D HAVK_GDB_DEBUG
	UEFI_CC_DEF+= -D TEXT_OFFSET=$(UEFI_TEXT_SECTION_OFFSET)
	UEFI_CC_DEF+= -D DATA_OFFSET=$(UEFI_DATA_SECTION_OFFSET)
endif

ifneq ("$(UEFI_IMAGE_BASE)", "")
	UEFI_TEXT_SECTION=\
		$$(($(UEFI_IMAGE_BASE) + $(UEFI_TEXT_SECTION_OFFSET)))
	UEFI_DATA_SECTION=\
		$$(($(UEFI_IMAGE_BASE) + $(UEFI_DATA_SECTION_OFFSET)))
endif

UEFI_LD_OPT=-nostdlib -Bsymbolic -shared -no-undefined -znocombreloc
UEFI_LD_INC=-L $(LIB_PATH) -T $(UEFI_LINK)
UEFI_LD_LIB=-l efi -l gnuefi
UEFI_LD_FLAGS=$(UEFI_LD_OPT) $(UEFI_LD_INC) $(UEFI_CRT0)

GDB_REMOTE_DEBUG_PORT?=40404
QEMU_MEMORY?=1024
QEMU_FLAGS=-serial mon:stdio -gdb tcp::$(GDB_REMOTE_DEBUG_PORT) -net none \
	-m $(QEMU_MEMORY) -no-reboot -no-shutdown -nic user,model=e1000e -smp 2

ifeq ("$(QEMU_DEBUG)", "1")
	# The interesting settings e.g. "int" doesn't work when the host CPU
	# is being utilised instead of the QEMU64 CPU.
	QEMU_FLAGS+= -d guest_errors,cpu_reset,pcall,page,int
else
	QEMU_FLAGS+= -d guest_errors
endif

# I've decided not to support xAPIC and am only supporting x2APIC. In reality,
# adding support for the former isn't difficult, but nearly every CPU at and
# after Sandy Bridge should support it. If it doesn't, then it's likely that
# the system itself doesn't even have UEFI firmware, so why bother with the
# backwards compatibility.
ifeq ("$(QEMU_SOFTWARE_CPU)", "1")
	QEMU_FLAGS+= -cpu qemu64,check
else
	QEMU_FLAGS+= -cpu host,+x2apic,enforce -enable-kvm
endif

BOOTLOADER_NAME=boot
UEFI_C_FILE=$(UEFI_SOURCE_PATH)$(BOOTLOADER_NAME).c
UEFI_O_FILE=$(BUILD_PATH)$(BOOTLOADER_NAME).o
UEFI_SO_FILE=$(BUILD_PATH)$(BOOTLOADER_NAME).so

# For now, I've also included some basic programs for the operating system
# itself in this Git repository. I may move them later on if they get too
# large and unwieldy for the current repository. Also avoid potential name
# collisions by putting the output in a subdirectory of the build path.
# I'll simply make all of the operating system programs and store any generated
# ELF files onto the EFI partition.
OPERATING_SYSTEM_BUILD_PATH=$(BUILD_PATH)operating_system/
OPERATING_SYSTEM_SOURCE_PATH=$(SOURCE_PATH)operating_system/

OPERATING_SYSTEM_RUNTIME_PATH=\
	$(OPERATING_SYSTEM_SOURCE_PATH)include/ada_runtime/
OPERATING_SYSTEM_ADAINCLUDE_PATH=$(OPERATING_SYSTEM_BUILD_PATH)adainclude/
OPERATING_SYSTEM_ADALIB_PATH=$(OPERATING_SYSTEM_BUILD_PATH)adalib/

OPERATING_SYSTEM_RUNTIME_PROJECT=\
	$(OPERATING_SYSTEM_SOURCE_PATH)$(NAME)_Operating_System_Runtime.gpr
OPERATING_SYSTEM_PROJECT=\
	$(OPERATING_SYSTEM_SOURCE_PATH)$(NAME)_Operating_System.gpr

OPERATING_SYSTEM_LIBRARY=\
	$(OPERATING_SYSTEM_ADALIB_PATH)libhavk_operating_system_runtime.a
OPERATING_SYSTEM_PROGRAMS=\
	$(wildcard $(OPERATING_SYSTEM_BUILD_PATH)programs/*)
OPERATING_SYSTEM=operating-system # A phony/dummy.

# Change these if you want a bigger disk image etc. It assumes that
# `parted` treats the sector size as 512 bytes. Since I don't need all
# the space that FAT32 requires as a minimum, and that `parted` does not
# support FAT12, I've gone with FAT16 for now so I don't waste your space.
# HAVK can only read files off a FAT16 formatted partition via 512-byte sectors
# for now, so these are sane values.
FAT_SIZE=16
IMAGE_BLOCK_SIZE=512
IMAGE_SECTORS=10000

# The bootable UEFI system partition's sector details. I store the kernel
# in there as well, because why not.
ESP_SECTOR_START=50
ESP_SECTOR_END=9500
ESP_SECTOR_SIZE=9550
ESP_PARTITION:=$(BUILD_PATH)ESP.partition

# Instead of creating a new drive image each time, we'll just modify the
# already created one (if one exists). You can provide your own "IMAGE"
# and "ESP_OFFSET" variables for a drive image not made by this Makefile.
ESP_OFFSET?=$$(($(IMAGE_BLOCK_SIZE) * $(ESP_SECTOR_START)))
IMAGE_ESP=$(IMAGE)@@$(ESP_OFFSET)

# When debugging, consider warnings as errors and be more verbose.
# Also control the optimisation here so it's easier to tweak.
ifeq ("$(BUILD)", "Debug")
	O?=g
	UEFI_CC_OPT+= -ggdb3 -O$(O)
	GPR_RUNTIME_FLAGS=-we -k -d -O$(O)
	GPR_FLAGS=-we -k -d -O$(O)
	GPR_VARIABLES=-XBuild=$(BUILD)
else
	O?=2
	UEFI_CC_OPT+= -g0 -O$(O)
	GPR_RUNTIME_FLAGS=-q -vP0 -O$(O)
	GPR_FLAGS=-q -vP0 -O$(O)
	GPR_VARIABLES=-XBuild=$(BUILD)
endif

ifneq ("$(PROVE_FILES)", "")
	override PROVE_FILES:=-u $(PROVE_FILES)
endif

# $1 => The string to echo.
define echo
	@echo -e "\033[4;96m          $1\033[0m"
endef

# $1 => The directory to create. Handles logic for when it exists.
define mtools_create
	@if ! mdir -i $(IMAGE_ESP) $1 > /dev/null 2>&1; then \
		mmd -i $(IMAGE_ESP) $1; \
	fi
endef

# $1 => The file(s) to copy. Mind spaces and quotes.
# $2 => The location of the copy/copies. Supply "::" as prefix for the path.
define mtools_copy
	@mcopy -D o -i $(IMAGE_ESP) $1 $2
	@echo -e "$2\r\n          $1"
endef

.DEFAULT_GOAL: all
.PHONY: all
all: $(UEFI_BOOTLOADER) $(KERNEL) $(OPERATING_SYSTEM)

######################## Build Structure Preparations #########################

$(BUILD_PATH):
	@if [ ! -d "$@" ]; then mkdir "$@"; fi

$(KERNEL_BUILD_PATH) $(OPERATING_SYSTEM_BUILD_PATH): | $(BUILD_PATH)
	@if [ ! -d "$@" ]; then mkdir "$@"; fi

$(KERNEL_ADALIB_PATH) $(OPERATING_SYSTEM_ADALIB_PATH): \
| $(KERNEL_BUILD_PATH) $(OPERATING_SYSTEM_BUILD_PATH)
	@if [ ! -d "$@" ]; then mkdir "$@"; fi

$(KERNEL_ADAINCLUDE_PATH): | $(KERNEL_BUILD_PATH)
	@if [ ! -d "$@" ]; then mkdir "$@"; fi
	@ln -f $(KERNEL_RUNTIME_PATH)/* "$@"

$(OPERATING_SYSTEM_ADAINCLUDE_PATH): | $(OPERATING_SYSTEM_BUILD_PATH)
	@if [ ! -d "$@" ]; then mkdir "$@"; fi
	@ln -f $(OPERATING_SYSTEM_RUNTIME_PATH)/* "$@"

############################# Bootloader Building #############################

$(UEFI_O_FILE): $(UEFI_C_FILE) | $(BUILD_PATH)
	$(call echo, "BUILDING BOOTLOADER TO $(UEFI_BOOTLOADER)")

	$(CC) $(UEFI_CC_FLAGS) -c "$<" -o "$@"

$(UEFI_SO_FILE): $(UEFI_O_FILE)
	$(call echo, "LINKING BOOTLOADER APPLICATION")

	$(LD) $(UEFI_LD_FLAGS) "$<" -o "$@" $(UEFI_LD_LIB)

$(UEFI_BOOTLOADER): $(UEFI_SO_FILE)
	$(call echo, "FIXING GNU-EFI BOOTLOADER\'S SECTIONS")

	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc  -j .debug_info -j .debug_abbrev \
		-j .debug_loc -j .debug_aranges -j .debug_line \
		-j .debug_macinfo -j .debug_str --target=efi-app-x86_64 \
		"$<" "$@.debug"

	objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym \
		-j .rel -j .rela -j .reloc --target=efi-app-x86_64 \
		"$<" "$@"

############################### Kernel Building ###############################

.PHONY: $(KERNEL_LIBRARY) # Let `gprbuild` handle change detection.
$(KERNEL_LIBRARY): | $(KERNEL_ADAINCLUDE_PATH) $(KERNEL_ADALIB_PATH)
	$(call echo, "BUILDING THE HAVK KERNEL\'S RUNTIME IN \
		$(KERNEL_BUILD_PATH)")

	@gprbuild -P $(KERNEL_RUNTIME_PROJECT) $(GPR_VARIABLES) -p -eL \
		--complete-output $(GPR_RUNTIME_FLAGS) -j0 -s -o "./../../$@"

.PHONY: $(KERNEL) # Let `gprbuild` handle change detection.
$(KERNEL): $(KERNEL_LIBRARY)
	$(call echo, "BUILDING THE HAVK KERNEL TO $@")

	@gprbuild -P $(KERNEL_PROJECT) $(GPR_VARIABLES) -p -eL \
		--complete-output $(GPR_FLAGS) -j0 -s -o "./../../$@"

########################## Operating System Building ##########################

.PHONY: $(OPERATING_SYSTEM_LIBRARY) # Let `gprbuild` handle change detection.
$(OPERATING_SYSTEM_LIBRARY): | $(OPERATING_SYSTEM_ADAINCLUDE_PATH) \
$(OPERATING_SYSTEM_ADALIB_PATH)
	$(call echo, "BUILDING THE HAVK OPERATING SYSTEM\'S RUNTIME IN \
		$(OPERATING_SYSTEM_BUILD_PATH)")

	@gprbuild -P $(OPERATING_SYSTEM_RUNTIME_PROJECT) $(GPR_VARIABLES) \
		-p -eL --complete-output $(GPR_RUNTIME_FLAGS) -j0 -s \
		-o "./../../$@"

.PHONY: $(OPERATING_SYSTEM)
$(OPERATING_SYSTEM): $(OPERATING_SYSTEM_LIBRARY)
	$(call echo, "BUILDING THE HAVK OPERATING SYSTEM IN \
		$(OPERATING_SYSTEM_BUILD_PATH)")

	@gprbuild -P $(OPERATING_SYSTEM_PROJECT) $(GPR_VARIABLES) -p -eL \
		--complete-output $(GPR_FLAGS) -j0 -s -nostdlib -nostdinc

################################ Image Building ###############################

$(ESP_PARTITION): | $(BUILD_PATH)
	$(call echo, "CREATING BLANK \"EFI\" SYSTEM PARTITION AT $@")

	dd if=/dev/zero of="$@" bs=$(IMAGE_BLOCK_SIZE) count=$(ESP_SECTOR_SIZE)
	mkfs.fat -v -F $(FAT_SIZE) -s 1 -S $(IMAGE_BLOCK_SIZE) "$@"

$(IMAGE): | $(ESP_PARTITION)
	$(call echo, "CREATING BOOTABLE IMAGE AT $@")

	dd if=/dev/zero of="$@" bs=$(IMAGE_BLOCK_SIZE) count=$(IMAGE_SECTORS)

	parted "$@" --align minimal --script \
		mktable gpt \
		mkpart primary fat$(FAT_SIZE) \
			$(ESP_SECTOR_START)s $(ESP_SECTOR_END)s \
		name 1 EFI \
		set 1 esp on \

	dd if="$(ESP_PARTITION)" of="$@" bs=$(IMAGE_BLOCK_SIZE) \
		count=$(ESP_SECTOR_SIZE) seek=$(ESP_SECTOR_START) conv=notrunc

.PHONY: image-configuration
image-configuration: $(UEFI_BOOTLOADER) $(KERNEL) $(OPERATING_SYSTEM) \
$(IMAGE)
	$(call echo, "CONFIGURING HAVK ON IMAGE AT $(IMAGE)")

	$(call mtools_create, '/EFI')
	$(call mtools_create, '/EFI/BOOT')
	$(call mtools_copy, "$(UEFI_BOOTLOADER)","::/EFI/BOOT/BOOTX64.EFI")

	$(call mtools_create, "/$(NAME)")
	$(call mtools_copy, "$(KERNEL)","::/$(NAME)/$(NAME).elf")
	$(call mtools_copy, \
	"$(UEFI_BOOTLOADER_CONFIGURATION)","::$(UEFI_BOOTLOADER_LOCATION)")

	$(call mtools_create, "/$(NAME)")
	$(call mtools_create, "/$(NAME)/system")
	$(call mtools_copy, \
		$(OPERATING_SYSTEM_PROGRAMS),"::/$(NAME)/system")

########################### Virtual Machine Testing ###########################

# (CTRL + A, C) to exit serial and enter monitor, and vice versa.
.PHONY: qemu
qemu: $(IMAGE) image-configuration
	$(call echo, "LOADING QEMU WITH $<")

	-qemu-system-x86_64 \
	-drive if=pflash,format=raw,unit=0,\
	file="$(OVMF_PATH)OVMF_CODE-pure-efi.fd",readonly=on \
	-drive if=pflash,format=raw,unit=1,\
	file="$(OVMF_PATH)OVMF_VARS-pure-efi.fd",readonly=on \
	-drive index=0,format=raw,media=disk,\
	file="$<",readonly=off \
	$(QEMU_FLAGS)

	@tput sgr0 # Corrected terminal after serial console usage.

$(HAVK_VMDK): $(IMAGE) image-configuration
	$(call echo, "CREATING VMDK FILE AT $@")

	@qemu-img convert -f raw -O vmdk "$<" "$@"

.PHONY: gdb
gdb:
	-@gdb "$(KERNEL)" -q \
		-ex "set confirm off" \
		-ex "set architecture i386:x86-64:intel" \
		-ex "set max-value-size 10485760" \
		-ex "set varsize-limit 10485760" \
		-ex "set tcp auto-retry on" \
		-ex "set tcp connect-timeout 300" \
		-ex "target remote :$(GDB_REMOTE_DEBUG_PORT)" \
		-ex "continue"

.PHONY: uefi-gdb
uefi-gdb:
	@if [ -z "$(UEFI_IMAGE_BASE)" ]; \
	then \
		echo "Set the 'UEFI_IMAGE_BASE' variable to"; \
		echo "the appropriate value when calling Make for"; \
		echo "the UEFI GDB commands to work as intended."; \
		exit 1; \
	fi

	-@gdb "$(UEFI_BOOTLOADER)" -q \
		-ex "set confirm off" \
		-ex "set architecture i386:x86-64:intel" \
		-ex "set max-value-size 10485760" \
		-ex "set tcp auto-retry on" \
		-ex "set tcp connect-timeout 300" \
		-ex "add-symbol-file \"$(UEFI_BOOTLOADER).debug\" \
			$(UEFI_TEXT_SECTION) -s .data $(UEFI_DATA_SECTION)" \
		-ex "target remote :$(GDB_REMOTE_DEBUG_PORT)" \
		-ex "set gdb_ready=1" \
		-ex "continue"

################################### Proving ###################################

.PHONY: proof
proof: $(KERNEL_BUILD_PATH)
	$(call echo, "PROVING HAVK KERNEL\'S CORRECTNESS")

	@gnatprove -P "$(KERNEL_PROJECT)" $(GPR_VARIABLES) $(PROVE_FILES)

.PHONY: stats
stats:
	$(call echo, "HAVK KERNEL STATISTICS")

	@echo -n "Makefile kernel image size capacity: $(shell du -sh \
		"$(KERNEL)" | awk -F '\t' '{print $$1}')"

	@echo -e " / $(shell du -sh "$(ESP_PARTITION)" \
		| awk -F '\t' '{print $$1}')\n"

	@cd "$(HAVK_SOURCE_PATH)" && gnatmetric -P $(NAME).gpr \
		$(GPR_VARIABLES) -U -eL

################################## Cleaning ###################################

.PHONY: clean-kernel
clean-kernel: | $(BUILD_PATH) $(HAVK_ADAINCLUDE_PATH) $(HAVK_ADALIB_PATH)
	$(call echo, "CLEANING ALL KERNEL FILES IN $(BUILD_PATH)")

	@gprclean -P "$(KERNEL_RUNTIME_PROJECT)" $(GPR_VARIABLES)
	@gprclean -P "$(KERNEL_PROJECT)" $(GPR_VARIABLES)
	@rm -vrf "$(KERNEL)"

.PHONY: clean-operating-system
clean-operating-system: | $(BUILD_PATH) $(OPERATING_SYSTEM_BUILD_PATH)
	$(call echo, "CLEANING ALL OPERATING SYSTEM FILES IN \
		$(OPERATING_SYSTEM_BUILD_PATH)")

	@rm -vrf $(OPERATING_SYSTEM_BUILD_PATH)*

.PHONY: clean-bootloader
clean-bootloader: | $(BUILD_PATH)
	$(call echo, "CLEANING ALL UEFI BOOTLOADER FILES IN $(BUILD_PATH)")

	@rm -vrf "$(UEFI_O_FILE)" "$(UEFI_SO_FILE)" "$(UEFI_BOOTLOADER)" \
		"$(UEFI_BOOTLOADER).debug"

.PHONY: clean-image
clean-image: | $(BUILD_PATH)
	$(call echo, "CLEANING ALL IMAGE FILES IN $(BUILD_PATH)")

	@rm -vrf "$(IMAGE)" "$(ESP_PARTITION)"

.PHONY: clean-proof
clean-proof: | $(BUILD_PATH) $(HAVK_ADAINCLUDE_PATH)
	$(call echo, "CLEANING ALL PROOF IN $(BUILD_PATH)")

	@gnatprove -P "$(KERNEL_PROJECT)" $(GPR_VARIABLES) --clean

.PHONY: clean-all
clean-all: clean-kernel clean-operating-system clean-bootloader clean-image \
clean-proof
	$(call echo, "DELETING BUILD DIRECTORY $(BUILD_PATH)")

	@rm -vrf "$(BUILD_PATH)"
