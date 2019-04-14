## Changelog for the HAVK operating system
## Version - ISO 8601 Date (UTC+13:00)

## 00-05-00 - 2019-04-14
### Overall Changes
- A "video map" along with a memory map is now passed to the Ada program.
- The framebuffer is controllable through Ada, meaning that graphics
can technically be drawn; however, graphics programming is not my speciality.
- Setting the video mode is possible through the UEFI application, a future
addition may be to grant the user an option to set the resolution etc.
- Instead of using the GNAT FSF compiler, the GNAT GPL (Community) version
will be used instead. It simplifies installation and comes with extra tools.
- HAVK is now GPLv3 licensed due to the GNAT compiler change.
- The build system has been refactored yet again. The old x86-BIOS-C version
has been moved into a seperate branch, and now a single Makefile builds
the x86_64-UEFI-Ada version of HAVK. The tool `gprbuild` is now used for
building the Ada portion. Originally, I decided against it, but it seems
to be more coherent to use than GNU Make and it comes with GNAT Community.
- The run-time system (RTS) is changed to the provided zero footprint profile
that comes with GNAT Community.
- I have decided against using exception handlers, as I would like HAVK to
not rely upon them at all. The configuration file has been updated to reflect
that. A last chance handler nearly changes all x86-64 registers to a
mnemonic value for quick lazy error detection in QEMU's monitor console.
- The style guide for Ada is still being decided, but I've gone with all
uppercase for keywords, mixed-case for variables, and lowercase for types.

## 00-04-00 - 2019-04-07
### Platform Shift
- HAVK will now target x86-64. For the sake of security, it will
utilize Ada as its programming language for critical safety over C. 
In an attempt to modernize it, HAVK will boot from its own UEFI application,
as BIOS is slated to not be included in x86(-64) Intel PCs from 2020 forward.
Changes below to the kernel reflect the x86 and x86-64 versions.
### Overall Changes
- A custom UEFI bootloader can now boot HAVK. It loads the kernel's ELF file
properly from what I can infer, and jumps to the ELF header entry point after
all loadable segments are copied into memory successfully.
- The build system for the HAVK's Ada version is now functional.
It was created via knowledge from the OSDev Wiki's Ada bare bones tutorial.
That version is for x86 BIOS. Maybe I should write a UEFI 64-bit 
bare bones Ada example after I begin to comprehend Ada more.
- The run-time system (RTS) is using minimal Ada features, but it can build
a freestanding ELF program. From what I can see, it mimics the RavenScar
RTS profile, which is seemingly not available for the FSF version of GNAT.
- A custom BIOS bootloader can now boot HAVK, making it independent from GRUB.
The bootloader lacks disk sector count detection. It also uses ATA PIO mode
to load the kernel into memory, and it relies on a magic number, as it does
not interpret the ELF header's entry point. The BIOS bootloader is unreliable.
- A PIC bug was partially resolved by minimalizing initialization.
HAVK can now utilize keyboard input on all (real) systems, not just QEMU.
- New script for compiling GCC. Has been tested with i686-elf successfully.
- Another restructure of Makefile system, now with two more Makefiles that
target the legacy version and the now current platform for HAVK.
It is not very stable right now, so the Makefiles will have to be cleaned up.
### Depreciation
- The x86 version of HAVK that was made with C and utilized BIOS to boot
has been depreciated. The last thing that was being worked on was a frame
allocator for the memory manager. A language translation from C to Ada
is now the next goal for the kernel as it is now.

## 00-03-00 - 2019-02-15
### Overall Changes
- GDT (with TSS) set up, Ring 3 entrance is technically possible.
- IDT set up (with PIC remappings to avoid clashes).
- PIT set up. Counting seconds is now possible, as with `sleep()`.
- Keyboard is now responsive and the user can type.
Only Scan code set 2 support only as of now.
- Terminal scrolling, but no scrollback buffer (for now).
- Memory map and multiboot information obtained from GRUB.
- 64-bit types now actually available due to previous LibGCC linkage failure.
- `printf()` now available for e.g. inline debugging.
- VGA (text) mode 3 cursor control.
- Redid "README.md".
- Makefile tweaks. Bochs RC file included.

## 00-02-00 - 2019-01-23
### Overall Changes
- Enabled paging.
- Further developed terminal operations and features.
- Much better and complex Makefile, more coherent build system overall.

## 00-01-00 - 2019-01-18
### Overall Changes
- Completely redid terminal functions.
- Restructured build directory and Makefile.

## 00-00-00 - 2019-01-18
### Initialized
- Based on OSDev Wiki's basic i386 bare-bones example with my Makefile.
