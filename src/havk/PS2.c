#include <havk/PS2.h>

char_ht ps2_key = 0;

/* TODO: Ignores breaks. Will need to redo this for the
/  "pause" key and perhaps some other keys as well. */
void ps2_update_key(void)
{
	uint_fast8_t ps2_scan_code;
	char_ht ps2_key_last;

	ps2_key_last = ps2_key;
	ps2_scan_code = in_byte(PS2_DATA);
	ps2_key = ps2_scs2_get_key(ps2_scan_code);

	if (!ps2_key)
		ps2_key = ps2_key_last;
}

uint8_t ps2_status_byte(struct ps2_status_register status)
{
	uint8_t byte;

	byte = 0;
	byte |= status.parity_error << 7;
	byte |= status.timeout_error << 6;
	byte |= status.unspecific_2 << 5;
	byte |= status.unspecific_1 << 4;
	byte |= status.data << 3;
	byte |= status.system << 2;
	byte |= status.input_buffer << 1;
	byte |= status.output_buffer;

	return byte;
}

uint8_t ps2_command_byte(struct ps2_command_register command)
{
	uint8_t byte;

	/* Bytes are read in complete reverse, but they are written
	/  like the OSDev wiki says (or as these next statements. */
	byte = 0;
	byte |= command.null << 7;
	byte |= command.second_port_clock << 6;
	byte |= command.first_port_clock << 5;
	byte |= command.zero << 4;
	byte |= command.first_port_clock << 3;
	byte |= command.system << 2;
	byte |= command.second_port << 1;
	byte |= command.first_port;

	return byte;
}

/* TODO: This entire function has no status checking. It just assumes that
/  the 8042 "chip" is done writing whenever I output a byte due to it
/  being fast enough, since it's 2019 and not 1989. Implement the status
/  register and integrate it into the initialization of the controller.
/  Maybe split this function up as well. */
bool ps2_initialize_keyboard(uint8_t scan_code_set)
{
	uint_fast8_t read, write;
	struct ps2_command_register command;

	command.first_port = 1; /* Only configure first port. */
	command.second_port = 0; /* TODO: Implement the mouse port. */
	command.system = 1; /* Must be 1 for indicating the system booted. */
	command.zero = 0;
	command.first_port_clock = 0;
	command.second_port_clock = 0;
	command.first_port_translation = 0; /* Disable scan code translation. */
	command.null = 0;

	read = 0;
	write = ps2_command_byte(command);

	/* Disable both PS/2 ports. */
	out_byte(PS2_PORT_1_DISABLE, PS2_COMMAND_PORT);
	out_byte(PS2_PORT_2_DISABLE, PS2_COMMAND_PORT);
	in_byte(PS2_DATA); /* Flush output. */

	/* Send an echo and see if we get an echo back. */
	out_byte(PS2_TEST_ECHO, PS2_DATA);
	read = in_byte(PS2_DATA);
	if (read == PS2_TEST_ECHO)
		hprintf("Initializing PS/2 controller and devices.", 0);
	else
	{
		hprintf("Unable to initialize PS/2 controller (0x%H).", read);
		return false;
	}

	/* Disable keyboard scan codes, so they don't interfere. */
	out_byte(PS2_KEYBOARD_SCAN_CODE_DISABLE, PS2_DATA);
	read = in_byte(PS2_DATA);
	if (read == ACKNOWLEDGED)
		hprintf("PS/2 device scan codes disabled.", 0);
	else
	{
		hprintf("Could not disable PS/2 device scan codes.", 0);
		return false;
	}

	/* Test the PS/2 controller. */
	out_byte(CONTROLLER_TEST, PS2_COMMAND_PORT);
	read = in_byte(PS2_DATA);
	if (read != CONTROLLER_TEST_PASS)
	{
		hprintf("The PS/2 controller reports failure (0x%H).", read);
		return false;
	}

	/* Test PS/2 port 1. */
	out_byte(PS2_TEST_PORT_1, PS2_COMMAND_PORT);
	read = in_byte(PS2_DATA);
	if (read != PS2_TEST_PORT_PASS)
	{
		hprintf("PS/2 port 1 reports failure (0x%H).", read);
		return false;
	}

	/* Test PS/2 port 2. */
	out_byte(PS2_TEST_PORT_2, PS2_COMMAND_PORT);
	read = in_byte(PS2_DATA);
	if (read != PS2_TEST_PORT_PASS)
	{
		hprintf("PS/2 port 2 reports failure (0x%H)."
			"Assuming dual PS/2 ports unsupported.", read);
	}

	/* Write our own configuration. */
	out_byte(CONTROLLER_CONFIGURATION_WRITE, PS2_COMMAND_PORT);
	out_byte(write, PS2_DATA);

	/* Check if it wrote correctly. */
	out_byte(CONTROLLER_CONFIGURATION_READ, PS2_COMMAND_PORT);
	read = in_byte(PS2_DATA);
	if (read == write)
		hprintf("PS/2 controller configuration written.", 0);
	else
		hprintf("Failed to write configuration to PS/2 controller.", 0);

	/* Configure scan code set. */
	out_byte(PS2_KEYBOARD_SCAN_CODE_SET_CONFIGURE, PS2_DATA);
	in_byte(PS2_DATA);
	out_byte(scan_code_set, PS2_DATA);
	if (in_byte(PS2_DATA) == ACKNOWLEDGED)
		hprintf("Keyboard now using scan code set %D.", scan_code_set);
	else
	{
		hprintf("Scan code set %D has failed to set.", scan_code_set);
		return false;
	}

	/* Enable keyboard scan codes. */
	out_byte(PS2_KEYBOARD_SCAN_CODE_ENABLE, PS2_DATA);
	if (in_byte(PS2_DATA) == ACKNOWLEDGED)
		hprintf("PS/2 device scan codes enabled.", 0);
	else
	{
		hprintf("Could not enable PS/2 device scan codes.", 0);
		return false;
	}

	/* TODO: Mouse support is currently not implemented. Doubt it will. */
	/* Enable the first PS/2 port. */
	out_byte(PS2_PORT_1_ENABLE, PS2_COMMAND_PORT);

	return true;
}

char_ht ps2_scs2_get_key(uint_fast8_t scan_code)
{
	switch (scan_code)
	{
		case SCAN_CODE_SET_2_A: return 'A';
		case SCAN_CODE_SET_2_B: return 'B';
		case SCAN_CODE_SET_2_C: return 'C';
		case SCAN_CODE_SET_2_D: return 'D';
		case SCAN_CODE_SET_2_E: return 'E';
		case SCAN_CODE_SET_2_F: return 'F';
		case SCAN_CODE_SET_2_G: return 'G';
		case SCAN_CODE_SET_2_H: return 'H';
		case SCAN_CODE_SET_2_I: return 'I';
		case SCAN_CODE_SET_2_J: return 'J';
		case SCAN_CODE_SET_2_K: return 'K';
		case SCAN_CODE_SET_2_L: return 'L';
		case SCAN_CODE_SET_2_M: return 'M';
		case SCAN_CODE_SET_2_N: return 'N';
		case SCAN_CODE_SET_2_O: return 'O';
		case SCAN_CODE_SET_2_P: return 'P';
		case SCAN_CODE_SET_2_Q: return 'Q';
		case SCAN_CODE_SET_2_R: return 'R';
		case SCAN_CODE_SET_2_S: return 'S';
		case SCAN_CODE_SET_2_T: return 'T';
		case SCAN_CODE_SET_2_U: return 'U';
		case SCAN_CODE_SET_2_V: return 'V';
		case SCAN_CODE_SET_2_W: return 'W';
		case SCAN_CODE_SET_2_X: return 'X';
		case SCAN_CODE_SET_2_Y: return 'Y';
		case SCAN_CODE_SET_2_Z: return 'Z';
		case SCAN_CODE_SET_2_0: return '0';
		case SCAN_CODE_SET_2_1: return '1';
		case SCAN_CODE_SET_2_2: return '2';
		case SCAN_CODE_SET_2_3: return '3';
		case SCAN_CODE_SET_2_4: return '4';
		case SCAN_CODE_SET_2_5: return '5';
		case SCAN_CODE_SET_2_6: return '6';
		case SCAN_CODE_SET_2_7: return '7';
		case SCAN_CODE_SET_2_8: return '8';
		case SCAN_CODE_SET_2_9: return '9';
		case SCAN_CODE_SET_2_SPACE: return '\x20';
		case SCAN_CODE_SET_2_BACKSPACE: return '\b';
		case SCAN_CODE_SET_2_TAB: return '\t';
		case SCAN_CODE_SET_2_ENTER: return '\r';
		case SCAN_CODE_SET_2_DOT: return '.';
		case SCAN_CODE_SET_2_SEMICOLON: return ';';
		case SCAN_CODE_SET_2_APOSTROPHE: return '\x27';
		case SCAN_CODE_SET_2_COMMA: return ',';
		case SCAN_CODE_SET_2_TILDE: return '`';
		case SCAN_CODE_SET_2_LEFT_BRACKET: return '[';
		case SCAN_CODE_SET_2_RIGHT_BRACKET: return ']';
		case SCAN_CODE_SET_2_EQUAL: return '=';
		case SCAN_CODE_SET_2_MINUS: return '-';
		case SCAN_CODE_SET_2_FORWARD_SLASH: return '/';
		case SCAN_CODE_SET_2_BACKSLASH: return '\\';
		default: return 0;
	}
}
