# Tasklist for the HAVK operating system and kernel
### Last Updated: 2020-12-30
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
- Start thinking of a way to do memory-mapped I/O for user space drivers, as
  tasks will need to interact with hardware. PCI(e) enumeration and access may
  be done inside the kernel.

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
  the tasking system is solid. This includes pointing the GS register's base
  address to a CPU-specific structure/record which includes tasking
  information etc.
- Finally start engaging with security concepts centred around user space.
  This includes handling x86 IOPBs properly for each task.
- Rework the physical frame allocator.
- Make the build system better and more convenient for proving operating
  system tasks programmed in SPARK.
- Handle the CPU exceptions which currently aren't handled by the tasking
  functionality, as they occasionally are raised by tasks due to errors.
- Come up with a way to make IPC interfaces cleaner from a programming
  perspective. I have been implementing them on an ad hoc basis.
- Clean up the entire operating system runtime overall, as I have been rushing
  it in comparison to the care I have been taking with the kernel.
- Redo the IPC handling for some of the user tasks that have been ignored
  due to the now bygone IPC performance issues.
- Slightly rework the tasking package's contracts, as there are too many
  dead task checks that raise a panic if reached. This should not actually be
  necessary if modelled more carefully.
- Move the PIT driver out of the kernel, as it has no reason to be inside the
  kernel any more aside from initially calibrating the LAPIC timer.
- Make IPC more fair and give tasks more control over context switching,
  along with what tasks are actually active so an expensive system call does
  not have to be used in order for every task in order to check if they've
  received a message. This can also potentially lead to taking the scheduler
  out of the kernel; thus, further pushing it towards a microkernel.