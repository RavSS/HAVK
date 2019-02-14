#include <havk/PIT.h>

uint_fast64_t tick = 0;
uint32_t tick_frequency = 0;
char_ht tick_string[UINT64_MAX_DIGITS + 1];

uint8_t pit_create_command(struct pit_control byte)
{
	uint8_t command;

	command = 0;
	command |= byte.channel << 6;
	command |= byte.access_mode << 4;
	command |= byte.operation << 1;
	command |= byte.binary_mode;

	return command;
}

bool pit_initialize_timer(uint32_t frequency)
{
	struct pit_control setup_timer;
	uint32_t reload_value;
	uint8_t reload_value_low;
	uint8_t reload_value_high;

	/* The OSDev wiki recommends only even numbers for the reload value.
	/  Operating mode 2 can use odd numbers (and is more accurate), but
	/  it ticks twice as much (compared to mode 3). I don't think
	/  the slight accuracy is worth the additional interruptions. */
	if (frequency % 2 != 0)
		return false;

	/* So other functions know the hertz of the timer too. */
	tick_frequency = frequency;

	reload_value = BASE_FREQUENCY / frequency;
	reload_value_low = reload_value & 0xFF;
	reload_value_high = (reload_value >> 8) & 0xFF;

	/* Configure the timer with 00110110 or 0x36 or 54.
	/  Channel: 00 = Channel 0.
	/  Access mode: 11 = Low byte, then high byte.
	/  Operation: 011 = Generate square waves.
	/  Binary mode: 0 = Use 16-bit binary. */
	setup_timer.channel = 0;
	setup_timer.access_mode = 3;
	setup_timer.operation = 3;
	setup_timer.binary_mode = 0;

	out_byte(pit_create_command(setup_timer), PIT_CONTROL_REGISTER);
	out_byte(reload_value_low, PIT_CHANNEL_0);
	out_byte(reload_value_high, PIT_CHANNEL_0);

	return true;
}