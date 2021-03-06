# HAVK
HAVK is an x86-64 operating system and kernel created with Ada (SPARK subset).
It focuses on security (and only that) via writing minimal code and using
formal verification techniques to help achieve program correctness.
It is influenced by Unix, but it is not necessarily a Unix clone.

### What can it do right now?
It currently has a primitive task scheduler and a functional user mode where
you can load your own x86-64 programs. What remains is to create more system
calls and expand what tasks can do so they can be meaningful.

Expect it to do nothing until I decide to release the first major version, as
it is still under heavy development.

### Goals

This exists so I can experiment with intriguing security concepts such as
capabilities and play around with how programs in ring 3 interact with
anything in ring 0, with the main goal being how much I can harden and
lockdown the entire operating system itself for fun.

I am more interested in any unique ideas you may have to do with operating
system security as opposed to code contributions; however, issues and pull
requests are very welcome, especially bug fixes.

The end goal is to get a text-based web browser working at the very least, so
one could consider this a nearly featureless but usable OS. There are no goals
to port this to other platforms/architectures at this stage.

### Software requirements
There are a few mandatory software requirements:
1. GNAT Community. GCC can compile Ada and the package contains the GNAT
   Project Manager tools, along with GNATprove for SPARK. There's a script
   inside the "tools" folder which you can utilise to obtain GNAT Community
   2021. You can also modify the Makefile.
2. GNU Make. This is pretty obvious.
3. GNU-EFI. The bootloader I created uses UEFI to boot HAVK, not BIOS.
4. GNU Mtools & GNU Parted. Used for creating a hard drive image.

Clone this repository, install those requirements, and enter `make`
to create a hard drive image inside "build" called "HAVK.img". Then, simply
`dd` it to a USB flash drive or install QEMU (`qemu-system-x86_64`) to emulate
HAVK in a VM by performing `make qemu`.

You can also recreate the proof by executing `make proof`.

### Hardware requirements
There's a few hardware requirements, but they are all critical right now:
1. An x86-64 system that has a working display/monitor.
2. UEFI firmware that isn't bugged and acts according to the specification,
   along with supporting ACPI 2.0+.
3. A PS/2 controller that is emulated/implemented by the system properly, as a
   USB controller would take forever to program (see the GNU Hurd microkernel).
4. A CPU that supports x2APIC.

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

I started from here and have progressed beyond it at this stage:

https://wiki.osdev.org/Ada_Bare_bones

These links are helpful for understanding Ada, SPARK, and GNAT:

https://en.wikibooks.org/wiki/Ada_Programming

http://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/index.html

http://docs.adacore.com/live/wave/spark2014/html/spark2014_rm/index.html

http://docs.adacore.com/live/wave/gnat_rm/html/gnat_rm/gnat_rm.html

Disclaimer: This is the first program I have made with either Ada or SPARK, and
that includes any Hello World examples. A lot of this OS may not be modelled
correctly as I am new to the concept of formal verification as well.

There is currently no specification of what the kernel should do, so I'm making
it up as I go along. `gnatprove` is used until it no longer brings up any
warnings or unproven checks by being as careful as possible and placing
restrictive contracts, with the purpose being to avoid runtime errors, not
proving 100% correctness. There's still SPARK mode exclusions and assumptions
made, like for when raw memory manipulation is necessary to continue.

### License
GNU GPLv3. Applies to everything unless stated otherwise.
This repository also contains OVMF compiled firmware, GNU-EFI objects, and Ada
runtime files provided by AdaCore as a part of GNAT GPL.
