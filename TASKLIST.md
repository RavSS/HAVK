# Tasklist for the HAVK operating system
### Last Updated: 2020-01-07
#### High priority
- Finally begin work on scheduling/multi-tasking and user mode.
- Create a CPUID package so things like 1-GiB pages can be used without fear.
- After implementing more OS functionality, implement LAPIC and IOAPIC
features by parsing some of the ACPI tables. Required for SMT support.

#### Low priority
- Begin to create a solution for PS/2 mouse capabilities.
The caps-lock state and shift-state for the keyboard can still sometimes
be incorrect.
- Very specific resolutions like 1366x768 are bugged and unusable.
- Reorganise the bootloader arguments structure and provide
a consistent format that does away with some of UEFI's oddities.
- Potentially stop using NASM and transition to GAS, as it is included
with GNAT and is a GNU tool. Less dependencies are usually nicer to have.
- Figure out why GDB acts erratic when debugging with HAVK that is
compiled with GCC's `-mcmodel=kernel` but not `-mcmodel=large`. For now,
the "Final" build is compiled with the former, as debug functionality is
irrelevant to that build's purpose.
