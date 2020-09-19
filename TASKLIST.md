# Tasklist for the HAVK operating system
### Last Updated: 2020-09-20
#### High priority
- The kernel crashes on AMD systems (but not Intel systems) during
  some point in which my descriptor tables are prepared and loaded, which
  points to the idea that they're not completely free of formatting errors.
- Add dynamic priority functionality to the round-robin scheduler, as it
  currently gives all tasks an identical time slice.
- Create a package for better concurrency support e.g. mutexes.
- Let the kernel run its own tasks and let user tasks order system operations
  using IPC instead of relying upon doing system calls immediately.
- Create a virtual filesystem task.
- Implement user-space signal handling so user tasks can handle interrupts.
  Currently needed for better PS/2 operability.
- Rework the message passing system. Instead of having a single queue which
  drops the oldest messages, do it the L4 way and give each task a limited
  number of message slots i.e. 64.

#### Low priority
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
  programs/tasks. Also turn all of them into libraries as opposed to compiling
  them separately for each program.
- Make the build system better and more convenient for proving operating
  system tasks programmed in SPARK.
