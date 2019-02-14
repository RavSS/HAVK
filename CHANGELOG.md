## Changelog for the HAVK operating system

## 0.03-00 - 2019/02/15
### Overall Changes
- GDT (with TSS) set up, Ring 3 entrance is technically possible.
- IDT set up (with PIC remappings to avoid clashes).
- PIT set up. Counting seconds is now possible, as with `sleep()`.
- Keyboard is now responsive (can type). Scan code set 2 support only (for now).
- Terminal scrolling, but no scrollback buffer (for now).
- Memory map and multiboot information obtained from GRUB.
- 64-bit types now actually available due to previous LibGCC linkage failure.
- `printf()` now available for e.g. inline debugging.
- VGA (text) mode 3 cursor control.
- Redid "README.md".
- Makefile tweaks. Bochs RC file included.

## 0.02-00 - 2019/01/23
### Overall Changes
- Enabled paging.
- Further developed terminal operations and features.
- Much better and complex Makefile, more coherent build system overall.

## 0.01-00 - 2019/01/18
### Overall Changes
- Completely redid terminal functions.
- Restructured build directory and Makefile.

## 0.00-00 - 2019/01/18
### Initialized
- Based on OSDev Wiki's basic i386 bare-bones example with my Makefile.
