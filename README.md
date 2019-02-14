# HAVK, an operating system and a kernel.
HAVK is a 32-bit (i386/IA-32) operating system and kernel that focuses on security experimentation.

### Why is it called "HAVK"?
https://en.wikipedia.org/wiki/Caesar_cipher
https://en.wikipedia.org/wiki/ROT13

It takes a lot of inspiration and design philosophy from another _particular_ operating system. HAVK is very simple.

### Why does it exist?
The real reason is so I can learn about operating system security fundamentals on a practical level. That's things like protection 
via virtual memory, hierarchical protection domains (privilege rings), and operating system architecture in general. I mostly plan 
to heavily experiment with things like how dynamic memory (or the heap) is managed, for good or for bad. Things that require drivers 
e.g. networking are not to be so hugely focused upon, or at least until the kernel is at a stage where it can survive a stack smash 
from 1996. It would have been extremely wise to have wrote this in Rust or Ada, but I'm far too comfortable with C, and now it might 
be too late. Writing safe software in C is a challenge.

### How did you make this?
By reading Intel's "Intel® 64 and IA-32 Architectures Software Developer Manuals" gigantic books: 
https://software.intel.com/en-us/articles/intel-sdm#combined 

Other than that, the excellent OSDev Wiki helps with simplified explanations for early on in development: 
https://wiki.osdev.org

I started with this example:
https://wiki.osdev.org/Bare_Bones 

From there on, I kept building upon it. There is not much point in creating a new operating system other than 
learning or for hobbyist enjoyment. Creating an operating system is no easy task.

### How do you run it?
A IA-32 cross-compiler is absolutely required. Use GCC and GNU Binutils, compile them from the source. After that, edit the Makefile 
to point to `i686-elf-gcc` (your new cross-compiler). You also require `nasm`, as I personally prefer Intel assembly syntax over 
AT&Tl $assembly (%syntax). The kernel is bootloaded via GRUB (`grub-mkrescue`), so get that too, along with `xorriso` for creating 
the ISO file. Once the prerequisites are set up, simply run `make HAVK.iso` and `dd` it onto a bootable device if you think it will 
run on a real bare-metal machine. I develop HAVK via QEMU and Bochs (emulation). I recommend installing `qemu-system-i386` and 
running `make qemu-kernel` (booting the kernel binary), then connecting to it via VNC.
