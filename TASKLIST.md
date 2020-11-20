# Tasklist for the HAVK operating system and kernel
### Last Updated: 2020-11-21
#### High priority
- The kernel crashes on AMD systems (but not Intel systems) during
  some point in which my descriptor tables are prepared and loaded, which
  points to the idea that they're not completely free of formatting errors.
- Implement a high-priority task queue which lets tasks switch context to a
  registered task "service". This will alleviate the shortcomings of the
  round robin scheduler.
- Create a package for better concurrency support e.g. mutexes.
- Create a model for storage-based I/O. Preferably something like the
  following: (S)ATA driver for drive I/O -> drive and partition manager ->
  filesystem driver(s) -> virtual filesystem. Perhaps run each one as its
  own task. This includes reworking the ATA PIO driver to separate the GPT
  and FAT16 parsers from it.
- Redo the ATA PIO package so it's efficient enough to be used with the
  current IPC mechanism, as it's currently extremely slow to a comical level
  due to repeated file lookups. Shared memory would fix this issue without
  needing to change the package itself, but that has other implications.
- Implement user-space signal handling so user tasks can handle interrupts.
  Currently needed for better PS/2 operability. Could work as a part of the
  high-priority task queue.
- Rework the message passing system. Instead of having a single queue which
  drops the oldest messages, let tasks have a connection state with other tasks
  and make message passing reliable i.e. messages won't get lost.

#### Low priority
- Properly calibrate the LAPIC timer so it's more accurate than the PIT.
- Implement FAT12 and FAT32 support, along with a VFAT driver.
- Improve the capabilities of the ATA PIO operations, particularly error
  checking.
- Trampoline the APs so we can use more than one core. I'd like to avoid
  concurrency issues and performance is not a demand, so leave this out until
  the tasking system is solid.
- Finally start engaging with security concepts centred around user space.
- Rework the physical frame allocator.
- Make the build system better and more convenient for proving operating
  system tasks programmed in SPARK.
- Let the bootloader configuration file exert more control over boot-time
  options i.e. the display resolution.
- Handle the CPU exceptions which currently aren't handled by the tasking
  functionality, as they occasionally are raised by tasks due to errors.
