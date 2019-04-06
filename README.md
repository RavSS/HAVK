# HAVK
HAVK is a 64-bit operating system and kernel created in Ada
that focuses on security experimentation.

### Why is it called "HAVK"?
https://en.wikipedia.org/wiki/Caesar_cipher

https://en.wikipedia.org/wiki/ROT13

It takes a lot of inspiration and design philosophy from another
_particular_ operating system, but may deviate from it. HAVK is very simple.

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
coin-flip, HAVK now uses Ada instead of C.

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
and using BIOS to boot.

From searching online, HAVK may be the first x86-64 Ada kernel
on the internet, as the rest seem to be for i386 and/or embedded systems.

### How do you run it?
#### IA-32
I discourage using the 32-bit version done in C, but it shows a minimal
working example. A cross compiler targeting ix86 like `i686-elf-gcc`
is required, along with `nasm` for some assembly parts.
Then it uses Grub to boot, but there is a very terrible custom bootloader
that does work half the time for BIOS boot in Qemu.

#### AMD64
While hardly anything at the moment, it uses the inner workings of `gnatmake`
or rather GNAT FSF to compile the freestanding Ada application. To compile
the UEFI application, GNU EFI is used and your standard x86-64 targeting
`gcc` compiles both the UEFI application and the Ada program.
