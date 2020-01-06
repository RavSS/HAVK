# HAVK
HAVK is a 64-bit operating system and kernel created with Ada (SPARK subset).
It focuses on security (and only that) via writing minimal code and using
formal verification techniques to help achieve program correctness.
It is influenced by Unix, but it is not necessarily a Unix clone.

### What can it do right now and what is next?
You can type and count seconds.

Expect it to do nothing until I decide to release the first major version, as
it is still under heavy development.

The next big step is to implement multi-tasking and user mode.

The end goal is to get a text-based web browser working at the very least.

### Software requirements
There are a few mandatory software requirements:
1. GNAT Community. GCC can compile Ada, and the package contains the
GNAT Project Manager tools, along with GNATprove for SPARK.
There's a script inside the "ext" folder which you can utilise
to obtain GNAT Community 2019.
2. GNU Make. This is pretty obvious.
3. GNU-EFI. The bootloader I created uses UEFI to boot HAVK, not BIOS.
4. GNU Mtools & GNU Parted. Used for creating a hard drive image.

Clone this repository, install those requirements, and enter `make`
to create a hard drive image inside "build" called "HAVK.img". Then, simply
`dd` it to a USB flash drive or install QEMU (`qemu-x86-64`) to emulate
HAVK in a VM by performing `make qemu`.

Note that HAVK can only be (easily) compiled on GNU/Linux. The Windows version
of GNAT Community 2019 unfortunately lacks x86-64 ELF support for its linker,
as it was not compiled with support for it. You will have to use WSL or
Cygwin if you wish to compile this on Windows.

### Hardware requirements
There's a few hardware requirements, but they are both critical right now:
1. An x86-64 system that has a working display/monitor.
2. UEFI firmware that isn't bugged and acts according to the specification.
3. A PS/2 controller that is emulated by the system properly, as a USB
controller would take forever to program (see the GNU Hurd microkernel).

Having a serial port (COM1, preferably) on your hardware would help in
debugging, so you can receive messages from the kernel about its progress.
You can boot HAVK from BIOS by emulating the UEFI services, but that is
not explained in detail here and should be avoided.

### Useful development resources
These two links are the best resources for development on the x86-64
architecture:

https://software.intel.com/en-us/articles/intel-sdm#combined

https://developer.amd.com/resources/developer-guides-manuals/

The OSDev Wiki helps greatly with general overviews:

https://wiki.osdev.org

These links are helpful for understanding Ada, SPARK, and GNAT:

https://en.wikibooks.org/wiki/Ada_Programming

http://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/index.html

http://docs.adacore.com/live/wave/spark2014/html/spark2014_rm/index.html

http://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/gnat_rm.html

### License
GNU GPLv3. Applies to everything unless stated otherwise.
This repository also contains OVMF compiled firmware and GNU-EFI objects.
