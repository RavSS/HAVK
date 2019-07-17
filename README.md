# HAVK
HAVK is a 64-bit operating system and kernel created with Ada
that focuses on security (and only that) via writing minimal code.
It is influenced by Unix, but it is not necessarily a Unix clone.

### Software requirements
There are a few mandatory software requirements:
1. GNAT Community. GCC can compile Ada, and the package contains the
GNAT Project Manager tools. There's a script inside the "ext" folder which
you can utilize.
2. GNU Make. This is pretty obvious.
3. GNU-EFI. The bootloader I created uses UEFI to boot HAVK, not BIOS.
4. GNU Mtools & GNU Parted. Used for creating a hard drive image.

Clone this repository, install those requirements, and enter `make`
to create a hard drive image inside "build" called "HAVK.img". Then, simply
`dd` it to a USB flash drive or install QEMU (`qemu-x86-64`) to emulate
HAVK in a VM by performing `make qemu`.

### Hardware requirements
There's two hardware requirements, but they are both critical right now:
1. UEFI firmware that isn't bugged and acts according to the specification.
2. A PS/2 controller that is emulated by the system properly, as a USB
controller would take forever to program (see the GNU Hurd microkernel).

Having a serial port (COM1, preferably) on your hardware would help in
debugging, so you can receive messages from the kernel about its progress.

### Why can it do right now?
You can type and count seconds.

Expect it to do nothing until I decide to release the first major version.

### What is next?
The next big step is to manage paging and memory.

The end goal is to get a text-based web browser working at the very least.

### Important development documents
These two links are the best resource for x86-64 architecture development:

https://software.intel.com/en-us/articles/intel-sdm#combined

https://developer.amd.com/resources/developer-guides-manuals/

The OSDev Wiki helps greatly with general overviews:

https://wiki.osdev.org

### License
GPLv3. Applies to everything unless stated otherwise or is external software.
