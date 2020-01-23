# Tasklist for the HAVK operating system
### Last Updated: 2020-01-24
#### High priority
- Create a CPUID package so things like 1-GiB pages can be used without fear.
- Get rid of the awful pre-emptive round robin scheduler and replace it
with anything less laggy which can properly terminate tasks.
- Start work on a real memory manager and not a static buffer like the one
for the kernel's heap as of now.
- Move the task scheduler to the runtime so we can use Ada's task syntax.
- Configure and calibrate either the LAPIC timer or the HPET. The latter
seems to be a stability mess, so prefer the former.

#### Low priority
- Begin to create a solution for PS/2 mouse capabilities.
The caps-lock state and shift-state for the keyboard can still sometimes
be incorrect.
- Very specific resolutions like 1366x768 are bugged and unusable.
- Reorganise the bootloader arguments structure and provide
a consistent format that does away with some of UEFI's oddities.
- Figure out why GDB acts erratic when debugging with HAVK that is
compiled with GCC's `-mcmodel=kernel` but not `-mcmodel=large`. For now,
the "Final" build is compiled with the former, as debug functionality is
irrelevant to that build's purpose.
