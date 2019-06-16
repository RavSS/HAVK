# HAVK
HAVK is a 64-bit operating system and kernel created in Ada
that focuses on security experimentation.

### How do I run it?
There are a few requirements:
1. GNAT Community. GCC can compile Ada, and the package contains the
GNU Project Manager tools. There's a script inside the "ext" folder which you
can utilize.
2. GNU Make. This is pretty obvious.
3. GNU-EFI. The bootloader uses UEFI to boot HAVK.
4. GNU Mtools & GNU Parted. Used for creating a hard drive image.

Clone this repository, install those requirements, and enter `make`
to create a hard drive image inside "build" called "HAVK.img". Then, simply
`dd` it to a USB flash drive or install QEMU (`qemu-x86-64`) to emulate
HAVK in a VM by performing `make qemu`.

### Why is it called "HAVK"?
https://en.wikipedia.org/wiki/Caesar_cipher

https://en.wikipedia.org/wiki/ROT13

It takes a lot of inspiration and design philosophy from another
_particular_ operating system, but will deviate from it. HAVK is very simple.
This does not mean HAVK will necessary be like one, though, and might not
end up like one at all.

### Why does it exist?
The real reason is so I can learn about operating system security fundamentals
on a practical level. That's things like protection via virtual memory,
hierarchical protection domains (privilege rings), and operating system
architecture in general.

I mostly plan to heavily experiment with things like
how dynamic memory (or the heap) is managed, for good or for bad.
Things that require drivers e.g. networking are not to be so hugely focused
upon, or at least until the kernel is at a stage where it can
survive a stack smash from 1996 locally or over TCP/IP.

I have not decided whether it is a monolithic kernel or a microkernel yet,
but HAVK will very likely be a microkernel due to its security focus,
so that way, I guarantee no one will use it and I will have to write
minimal code.

~~It would have been extremely wise to have wrote this in Rust or Ada,
but I'm far too comfortable with C, and now it might
be too late.~~ Writing safe software in C is a challenge. After a literal
coin-flip, HAVK now uses Ada instead of C. If I manage to grasp
formal verification, then I'll slowly shift it to Ada's subset SPARK.

### How did you make this?
Intel's gigantic "Intel® 64 and IA-32 Architectures Software Developer
Manuals" help when comprehending all the capabilities of x86(-64):

https://software.intel.com/en-us/articles/intel-sdm#combined

Other than that, the excellent OSDev Wiki helps with simplified explanations
for early on in development:

https://wiki.osdev.org

I started with this example:

https://wiki.osdev.org/Bare_Bones

From there on, I kept building upon the 32-bit version.
There is not much point in creating a new operating system other than learning
or for hobbyist enjoyment. Creating an operating system is no easy task.

After wanting to switch languages to something more exotic and maybe even
reasonable, I came across this example:

https://wiki.osdev.org/Ada_Bare_bones

Now, HAVK mainly uses Ada. The difference between that example and HAVK
is that HAVK uses UEFI to boot and it is 64-bit instead of being 32-bit
and using BIOS to boot. Now, it also handles a framebuffer and interrupts.

From searching online, HAVK may be the first x86-64 Ada kernel
on the internet, as the rest seem to be for i386, embedded systems, or
rewrites of existing kernels.
