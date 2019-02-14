#ifndef PIT_H
#define PIT_H

#include <havk.h>
#include <havk/io.h>

/* This channel is connected to the PIC's IRQ 0. Creates interruptions. */
#define PIT_CHANNEL_0 0x40

/* I think this is just a historical leftover. Apparently it once used to
/  refresh DRAM. The OSDev wiki says it can't be used on modern PCs. */
#define PIT_CHANNEL_1 0x41

/* PC speaker control, the real reason as to why I'm making this OS. :^) */
#define PIT_CHANNEL_2 0x42

/* Port used to select channels, access modes, and operations modes. */
#define PIT_CONTROL_REGISTER 0x43

/* TODO: Make some preprocessor definitions for these. */
/* Taken from the OSDev wiki:
/  https://wiki.osdev.org/Programmable_Interval_Timer#I.2FO_Ports
/  Bits		Usage
/  6 - 7	Select channel:
/			0 0 = Channel 0.
/			0 1 = Channel 1.
/			1 0 = Channel 2.
/			1 1 = Read-back command (8254 only).
/  4 - 5	Access mode:
/			0 0 = Latch count value command.
/			0 1 = Access mode: low byte only.
/			1 0 = Access mode: high byte only.
/			1 1 = Access mode: low byte/high byte.
/  1 - 3	Operating mode:
/			0 0 0 = Mode 0 (interrupt on terminal count).
/			0 0 1 = Mode 1 (hardware re-triggerable one-shot).
/			0 1 0 = Mode 2 (rate generator).
/			0 1 1 = Mode 3 (square wave generator).
/			1 0 0 = Mode 4 (software triggered strobe).
/			1 0 1 = Mode 5 (hardware triggered strobe).
/			1 1 0 = Mode 2 (rate generator, same as 010b).
/			1 1 1 = Mode 3 (square wave generator, same as 011b).
/      0	BCD/Binary mode: 0 = 16-bit binary, 1 = four-digit BCD.
/  More details: https://en.wikipedia.org/wiki/Intel_8253 */
struct pit_control
{
	uint8_t binary_mode : 1;
	uint8_t operation : 3;
	uint8_t access_mode : 2;
	uint8_t channel : 2;
};

extern uint_fast64_t tick;
extern uint32_t tick_frequency;
extern char_ht tick_string[];

/* Base frequency is approximately 1.193181666... MHz. */
#define BASE_FREQUENCY 1193180

uint8_t pit_create_command(struct pit_control byte);
bool pit_initialize_timer(uint32_t frequency);

#endif
