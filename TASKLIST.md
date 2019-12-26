# Tasklist for the HAVK operating system
### 2019-12-25
- Very specific resolutions like 1366x768 are bugged and unusable.
- Unoptimised `Object.Screen(Index, Pixel)` function as I cannot figure out
how to add a non-local address aliased array into a tagged record, or
any other optimal solution. Making it non-primitive doesn't help at
removing the overhead.
- Reorganise the bootloader arguments structure and provide
a consistent format that does away with some of UEFI's oddities.
- Potentially stop using NASM and transition to GAS, as it is included
with GNAT and is a GNU tool. Less dependencies are usually nicer to have.
- Create a CPUID package so things like 1-GiB pages can be used without fear.
- Quickly refresh the GPR file for HAVK's runtime, as the main project file
has deviated somewhat.
- Begin to create a solution for PS/2 mouse capabilities. Low priority.
- Figure out why GDB acts erratic when debugging with HAVK that is
compiled with GCC's `-mcmodel=kernel` but not `-mcmodel=large`. For now,
the "Final" build is compiled with the former, as debug functionality is
irrelevant to that build's purpose.
- After implementing more OS functionality, implement LAPIC and IOAPIC
features by parsing some of the ACPI tables. Required for SMT support.
- Switch from a statically allocated page structure to a dynamic one,
as it's unfortunately too limiting and we have plenty of heap space.
