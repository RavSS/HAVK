# Tasklist for the HAVK operating system
### Last Updated: 2020-08-03
#### High priority
- The kernel crashes on AMD systems (but not Intel systems) during
  some point in which my descriptor tables are prepared and loaded, which
  points to the idea that they're not completely free of formatting errors.
- Add dynamic priority functionality to the round-robin scheduler, as it
  currently gives all tasks an identical time slice.
- Create a package for better concurrency support e.g. mutexes.
- Start implementing various system calls so user space can do something.
- Create a virtual filesystem task.
- Implement a user-space thread library using `setjmp()` and `longjmp()` or
  bring back the kernel-space threading from a few commits ago, as I cannot
  think of a way to do preemptive threading in user-space.

#### Low priority
- Take the PS/2 driver out of the kernel and into a ring 3 task.
- Reorganise the bootloader arguments structure and provide
  a consistent format that does away with some of UEFI's oddities.
- Properly calibrate the LAPIC timer so it's more accurate than the PIT.
- Implement FAT12 and FAT32 support, along with a VFAT driver.
- Improve the capabilities of the ATA PIO operations, particularly error
  checking.
- Trampoline the APs so we can use more than one core. I'd like to avoid
  concurrency issues and performance is not a demand, so leave this out until
  the tasking system is solid.
- Finally start engaging with security concepts centred around user space.
- Rework the physical frame allocator.
- Extend the capabilities of the Ada and C runtime files for operating system
  programs/tasks.
