# Tasklist for the HAVK operating system and kernel
### Last Updated: 2020-12-17
#### High priority
- The kernel crashes on AMD systems (but not Intel systems) during
  some point in which my descriptor tables are prepared and loaded, which
  points to the idea that they're not completely free of formatting errors.
  Note that this crash has only been observed on a AMD Ryzen 7 1700X processor.
- Implement a high-priority task queue which lets tasks switch context to a
  registered task "service". This will alleviate the shortcomings of the
  round robin scheduler.
- Create a package for better concurrency support e.g. mutexes.
- Implement user-space signal handling so user tasks can handle interrupts.
  Currently needed for better PS/2 operability. Could work as a part of the
  high-priority task queue.
- Rework the system call for yielding so that it doesn't just zero out the
  time slice and instead does a proper context switch. Same goes for the exit
  task operation. This will lead to a performance gain due to how the current
  scheduler is implemented.
- Allow interrupts (including context switches) during system calls. An
  extremely noticeable performance penalty is currently present due to some
  tasks starving out other tasks by being heavily dependent on system calls.

#### Low priority
- Set the LAPIC timer to one-shot mode whenever a task is switched. This
  might be better than an interrupt every specific period due to less
  pointless interruptions.
- Implement FAT12 and FAT32 support, along with a VFAT driver.
- Improve the capabilities of the ATA PIO operations, particularly error
  checking. In regards to tasking, the tasks which depend on the ATA driver
  task expect no sudden task terminations and cannot reset themselves
  accordingly.
- Trampoline the APs so we can use more than one core. I'd like to avoid
  concurrency issues and performance is not a demand, so leave this out until
  the tasking system is solid.
- Finally start engaging with security concepts centred around user space.
- Rework the physical frame allocator.
- Make the build system better and more convenient for proving operating
  system tasks programmed in SPARK.
- Handle the CPU exceptions which currently aren't handled by the tasking
  functionality, as they occasionally are raised by tasks due to errors.
- Come up with a way to make IPC interfaces cleaner from a programming
  perspective. I have been implementing them on an ad hoc basis.
- Clean up the entire operating system runtime overall, as I have been rushing
  it in comparison to the care I have been taking with the kernel.