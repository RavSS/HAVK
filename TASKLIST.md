# Tasklist for the HAVK operating system
### Last Updated: 2020-04-06
#### High priority
- The kernel crashes on AMD systems (but not Intel systems) during
  some point in which my descriptor tables are prepared and loaded, which
  points to the idea that they're not completely free of formatting errors.
- Get rid of the awful pre-emptive round robin scheduler and replace it
  with anything less laggy which can properly terminate tasks.
- Move the task scheduler to the runtime so we can use Ada's task syntax.
  A general refactor of the entire RTS would be good.
- Create a package for better concurrency support e.g. mutexes and make
  numerous subprograms safer for ring 0 tasks.
- Get access to the main disk, either via DMA or PIO. The former requires
  PCI enumeration, but the latter is much slower.

#### Low priority
- Begin to create a solution for PS/2 mouse capabilities.
  The caps-lock state and shift-state for the keyboard can still sometimes
  be incorrect.
- Very specific resolutions like 1366x768 are bugged and unusable.
- Reorganise the bootloader arguments structure and provide
  a consistent format that does away with some of UEFI's oddities.
- Properly calibrate the LAPIC timer so it's more accurate than the PIT.
- Use more abstract states for SPARK to help indicate external logic.
