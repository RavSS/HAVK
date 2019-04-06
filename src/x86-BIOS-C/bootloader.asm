; This file is a bootloader for HAVK. It's completely custom, and is
; to be considered a heavy work in progress. I would suggest using GRUB
; for a long time being, especially with how GRUB supports AHCI instead of
; just the older ATA method, which often requires going into the BIOS of the
; machine to enable the older SATA operation mode.

HAVK_LOCATION EQU 0x100000 ; HAVK is physically located at 1 MiB.
HAVK_MAGIC_NUMBER EQU 0x0DEC0DED ; For the kernel to know what it booted with.
HAVK_MAGIC_NUMBER_ENTRY EQU 0xDEADC0DE ; For the bootloader to find the entry.

; TODO: This is not an efficient way of finding available sectors to load.
; A better method would be to query the ATA ports for maximum LBA values.
; A slightly less worse method would be to define this size as an argument.
HAVK_SIZE EQU 32880 ; Size of "HAVK.bin". Needs to be updated every time!

; A macro for breakpoints without it persisting in memory even when unused.
%MACRO BREAKPOINT 0
	CLI ; Opcode 0xFA. 1 byte size.
	HLT ; Opcode 0xF4. 1 byte size.
	JMP $ - 0x2 ; Jump back two bytes (word) for an infinite loop.
%ENDMACRO

BITS 16 ; BIOS loads us in real mode in nearly all cases.
GLOBAL _start ; The linker expects this label (ELF).
_start:
	XOR AX, AX ; Clear it just in case.
	MOV DS, AX
	MOV ES, AX
	MOV GS, AX
	MOV FS, AX
	MOV SS, AX
	MOV AX, 0x1337 ; Stack size doesn't really matter for now.
	MOV SP, AX ; Stack starts 0x1337 ahead.
	STI ; Enable interrupts.

	; TODO: While in real mode (16 bit), I can access BIOS functions
	; or interrupts. I should use this time to gain a memory map, which
	; is something that GRUB does, but this currently does not.

	; Clear the screen.
	XOR AH, AH ; Set to zero. Interrupts now control the video mode.
	MOV AL, 0x2 ; 80x25 mode.
	INT 0x10

	MOV AH, 0xE ; Print characters when interrupt 0x10 is called.
	MOV SI, boot_message ; Load the address of the string into "SI".
	CALL print

	; TODO: This is very lazy. QEMU and Bochs both enable
	; the A20 gate from the beginning anyway, which seems to be
	; quite unrealistic. The fast method of enabling the A20
	; gate should be supported on most PCs these days,
	; but this is still quite subpar, and should be checked by the code.
	CALL fast_A20

	; Now begin to leave real mode (16 bit) for protected mode (32 bit).
	CLI ; Disable interrupts, HAVK will set it up later on.
	LGDT [GDTR] ; Load the GDT. HAVK will create a "proper" one soon.

	MOV EAX, CR0
	OR EAX, 0x1 ; Flip the protected mode bit.
	MOV CR0, EAX

	; Took this from HAVK's "descriptors.c". Need to update the segments.
	MOV AX, 0x10 ; 0x10 is the data descriptor (third actual entry).
	MOV DS, AX
	MOV ES, AX
	MOV FS, AX
	MOV GS, AX
	MOV SS, AX
	JMP 0x8: protected_mode ; Reload the "CS" register via a far jump.

BITS 32
protected_mode:
	; Currently, HAVK is under 1 MiB. I am using LBA to load it to
	; memory as opposed to the older CHS address method. I am however
	; only using 22-bit LBA, which can only address around (I think) 4 GiB
	; of data. The operating system will obviously never go over that.

	.load_kernel:
		INC DWORD [current_LBA] ; Skips bootloader before first read!

		; Add a visual indicator (a dot) for each sector read.
		MOV EAX, 0xB8034
		TIMES 2 ADD EAX, [current_LBA]
		MOV DWORD [EAX], 0x72E

		CALL read_sector
		CMP DWORD [current_LBA], HAVK_SIZE / 512
		JBE .load_kernel

	; TODO: This is seriously not optimal, and is extremely prone
	; to breaking. I need to find the entry point for the ELF (or binary)
	; file of the kernel in a more subtle way.
	;JMP HAVK_ELF_ENTRY

	; Now to find the entry point, which is not the start of the kernel
	; because of other code before the entry function.
	MOV ECX, HAVK_LOCATION - 1

	.find_entry:
		INC ECX
		CMP DWORD [ECX], 0xDEADC0DE
		JNE .find_entry

	MOV EAX, HAVK_MAGIC_NUMBER
	MOV EBX, 0x1337 ; TODO: Pass a memory map, please.
	ADD ECX, 0x4 ; Jump to the instructions after the entry magic number.

	JMP ECX ; Finally exit the bootloader.

read_sector:
	; I'm going to load the kernel by utilizing ports.
	; The constants below are ports, they must be moved into the
	; "DX" register for use with the "OUT" instruction.
	; I am not going to be using CHS, but rather LBA, as CHS is old
	; and a lot more annoying to manage as opposed to the latter.
	; https://wiki.osdev.org/ATA_PIO_Mode
	; https://wiki.osdev.org/ATA_Command_Matrix
	; https://wiki.osdev.org/ATA_read/write_sectors#Read_in_LBA_mode
	; https://en.wikipedia.org/wiki/Logical_block_addressing
	DATA_REGISTER EQU 0x1F0 ; Where the data is read (or written).
	ERROR_REGISTER EQU 0x1F1
	SECTOR_COUNT EQU 0x1F2 ; The amount of sectors to read. Starts at 1.
	DRIVE_SELECT EQU 0x1F6
	COMMAND_REGISTER EQU 0x1F7 ; See the link above for all commands.

	; Since the logical block address value can be up to 22 bits long
	; and we can only send a byte (8 bits) at a time, I have to send it
	; to seperate ports. I will very likely not ever need the extended
	; LBA options. This is only intended to load the kernel, nothing
	; else. I don't think HAVK will even get bigger than an MB or MiB.
	LBA_0_7 EQU 0x1F3 ; Bits 0 to 7 of the address.
	LBA_8_15 EQU 0x1F4 ; Bits 8 to 15 of the address.
	LBA_16_23 EQU 0x1F5 ; Bits 16 to 23 of the address.

	MOV DWORD EBX, [current_LBA] ; Register access is faster.

	; Select the drive.
	MOV DX, DRIVE_SELECT
	MOV EAX, EBX
	SHR EAX, 24 ; Bits 24 to 32 of the address (starting at 1).
	OR AL, 11100000b ; Enable LBA mode. Select drive 0 (bit 4).
	OUT DX, AL

	MOV DX, SECTOR_COUNT
	MOV AL, 1 ; Read a single sector, of course.
	OUT DX, AL

	MOV DX, LBA_0_7
	MOV EAX, EBX
	OUT DX, AL ; Output lower 8 bits of EAX.

	MOV DX, LBA_8_15
	MOV EAX, EBX
	SHR EAX, 8
	OUT DX, AL ; Output lower 8 bits of (EAX >> 8).

	MOV DX, LBA_16_23
	MOV EAX, EBX
	SHR EAX, 16
	OUT DX, AL ; Output lower 8 bits of (EAX >> 16).

	MOV DX, COMMAND_REGISTER
	MOV AL, 0x20 ; Read and retry. Check the command list above for more.
	OUT DX, AL

	MOV DWORD EDI, [current_kernel_offset]
	MOV CX, 512 / 2 ; Read as words, as "IN" can input 16 bits.

	.read:
		CALL check_drive_read ; Check if we can read from the drive.
		CMP AL, 1 ; If "AL" is 1, then we currently cannot read.
		JE .read ; If "AL" is equal to 1, then check the drive again.

		MOV DX, DATA_REGISTER ; Data must be read from the data port.
		INSW ; Read a string word.
		DEC CX ; Decrease the counter.
		CMP CX, 0 ; Compare the counter to zero.
		JNE .read ; If "CX" isn't equal to zero, then read another word.

	MOV DWORD [current_kernel_offset], EDI
	RET

; This routine is needed to make sure I can actually read the data register
; when the hard drive is done preparing the data. Emulators like QEMU are
; very fast and won't need much time preparing data, but Bochs and real
; machines will verifiably need these checks, or else Bochs in particular
; will complain. In other words, since interrupts (IRQs) are inactive,
; we must poll the hard drive instead.
check_drive_read:
	MOV DX, COMMAND_REGISTER ; Command register when read is the HDD status.
	XOR AL, AL ; Clear it so the HDD is safe to read by default.
	IN AL, DX ; Get what's in the status (command) register.

	TEST AL, 0x8 ; Check if the drive is ready to transfer.
	JZ .busy ; Return 1 if bit 3 is not set (data not ready).
	TEST AL, 0x80 ; Check if the drive is busy.
	JNZ .busy ; Return 1 if bit 7 is set (drive is busy).

	XOR AL, AL ; No complaints detected, so clear "AL" again.
	RET ; Return, with "AL" as 0, indicating a read is safe.
	.busy:
		MOV AL, 1 ; "1" indicates the drive is not ready to be read.
		RET

BITS 16
; Requires "AH" to be 0xE and "SI" to be loaded with a string address.
print:
	MOV BYTE AL, [DS:SI] ; Move character into "AL".
	INC SI ; Move onto the next character's address in the string.
	TEST AL, AL ; Check for null-terminator.
	JZ .return ; Return if null/zero (string ended).
	INT 0x10 ; Print character in "AL".
	JMP print ; Do it again until null-terminated.
	.return: RET

fast_A20:
	; Got this odd method from OSDev Wiki. Saves a lot of work.
	IN AL, 0x92
	OR AL, 0x2
	OUT 0x92, AL
	RET

GDT:
	; Required for going into protected mode. Only a fluff one, though.
	; Basically cloned the ones from my "descriptors.c".
	.null_descriptor: ; First one always must be null.
		DQ 0 ; Descriptors take up 8 bytes.
	.code_descriptor:
		DW 0xFFFF ; Limit high.
		DW 0 ; Lower base.
		DB 0 ; Middle of base.
		DB 0x9A ; Access type.
		DB 0xCF ; Granularity.
		DB 0 ; Higher base.
	.data_descriptor:
		DW 0xFFFF
		DW 0
		DB 0
		DB 0x92
		DB 0xCF
		DB 0
	.end: ; The end of the GDT. Needed for size calculation.

GDTR:
	; This is what will be inside the GDT register via `LGDT`.
	DW GDT.end - GDT - 1 ; Things get odd if you forget that 1 byte.
	DD GDT

; The BIOS for x86 IBM PCs uses a form of ASCII that's a little different.
; There is no tabs from what I can see. Do not special escape in NASM strings.
; https://en.wikipedia.org/wiki/Code_page_437#Character_set
boot_message DB "Now booting HAVK ", VERSION, "...", 0x0

; These are pointers to memory where information is held, like where
; the kernel's end currently is, and what logical block address
; the loading routine last loaded into memory from the hard drive.
current_kernel_offset: DD HAVK_LOCATION
current_LBA: DD 0

TIMES (510 - ($ - $$)) DB 0 ; Fill in all 512 bytes (or boot sector).
DW 0xAA55 ; Magic number for the BIOS.
