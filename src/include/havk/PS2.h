#ifndef PS2_H
#define PS2_H

#include <havk.h>
#include <havk/io.h>
#include <havk/standard.h>

/* A lot of redundant definitions etc. are going to be here for the sake
/  of comprehending the PS/2 controller, as it seems pretty complex.
/  https://wiki.osdev.org/%228042%22_PS/2_Controller */

#define PS2_DATA 0x60
#define PS2_STATUS_REGISTER 0x64
#define PS2_COMMAND_PORT 0x64

#define ACKNOWLEDGED 0xFA
#define RESEND 0xFE

#define CONTROLLER_TEST 0xAA
#define CONTROLLER_TEST_PASS 0x55
#define CONTROLLER_TEST_FAIL 0xFC
#define CONTROLLER_CONFIGURATION_WRITE 0x60
#define CONTROLLER_CONFIGURATION_READ 0x20

#define PS2_TEST_ECHO 0xEE
#define PS2_TEST_PORT_1 0xAB
#define PS2_TEST_PORT_2 0xA9
#define PS2_TEST_PORT_PASS 0x00
#define PS2_TEST_PORT_FAIL_CLK_LOW 0x01
#define PS2_TEST_PORT_FAIL_CLK_HIGH 0x02
#define PS2_TEST_PORT_FAIL_DATA_LOW 0x03
#define PS2_TEST_PORT_FAIL_DATA_HIGH 0x04

#define PS2_PORT_1_ENABLE 0xAE
#define PS2_PORT_1_DISABLE 0xAD
#define PS2_PORT_2_ENABLE 0xA8
#define PS2_PORT_2_DISABLE 0xA7

#define PS2_KEYBOARD_SCAN_CODE_SET_CONFIGURE 0xF0
#define PS2_KEYBOARD_SCAN_CODE_ENABLE 0xF4
#define PS2_KEYBOARD_SCAN_CODE_DISABLE 0xF5

/* https://wiki.osdev.org/%228042%22_PS/2_Controller#Status_Register */
struct ps2_status_register
{
	uint8_t output_buffer : 1;
	uint8_t input_buffer : 1;
	uint8_t system : 1;
	uint8_t data : 1;
	uint8_t unspecific_1 : 1;
	uint8_t unspecific_2 : 1;
	uint8_t timeout_error : 1;
	uint8_t parity_error : 1;
};

/* https://wiki.osdev.org/%228042%22_PS/2_Controller
/  	#PS.2F2_Controller_Configuration_Byte */
struct ps2_command_register
{
	uint8_t first_port : 1;
	uint8_t second_port : 1;
	uint8_t system : 1;
	uint8_t zero : 1;
	uint8_t first_port_clock : 1;
	uint8_t second_port_clock : 1;
	uint8_t first_port_translation : 1;
	uint8_t null : 1;
};

extern char_ht ps2_key;

/* The basics for scan code set 2, which seems to be the de facto default.
/  TODO: Include the keys that require multiple reads when bothered to do so.
/  https://webdocs.cs.ualberta.ca/~amaral/courses/329/labs/scancodes.html */
#define SCAN_CODE_SET_2_A 0x1C
#define SCAN_CODE_SET_2_B 0x32
#define SCAN_CODE_SET_2_C 0x21
#define SCAN_CODE_SET_2_D 0x23
#define SCAN_CODE_SET_2_E 0x24
#define SCAN_CODE_SET_2_F 0x2B
#define SCAN_CODE_SET_2_G 0x34
#define SCAN_CODE_SET_2_H 0x33
#define SCAN_CODE_SET_2_I 0x43
#define SCAN_CODE_SET_2_J 0x3B
#define SCAN_CODE_SET_2_K 0x42
#define SCAN_CODE_SET_2_L 0x4B
#define SCAN_CODE_SET_2_M 0x3A
#define SCAN_CODE_SET_2_N 0x31
#define SCAN_CODE_SET_2_O 0x44
#define SCAN_CODE_SET_2_P 0x4D
#define SCAN_CODE_SET_2_Q 0x15
#define SCAN_CODE_SET_2_R 0x2D
#define SCAN_CODE_SET_2_S 0x1B
#define SCAN_CODE_SET_2_T 0x2C
#define SCAN_CODE_SET_2_U 0x3C
#define SCAN_CODE_SET_2_V 0x2A
#define SCAN_CODE_SET_2_W 0x1D
#define SCAN_CODE_SET_2_X 0x22
#define SCAN_CODE_SET_2_Y 0x35
#define SCAN_CODE_SET_2_Z 0x1A
#define SCAN_CODE_SET_2_0 0x45
#define SCAN_CODE_SET_2_1 0x16
#define SCAN_CODE_SET_2_2 0x1E
#define SCAN_CODE_SET_2_3 0x26
#define SCAN_CODE_SET_2_4 0x25
#define SCAN_CODE_SET_2_5 0x2E
#define SCAN_CODE_SET_2_6 0x36
#define SCAN_CODE_SET_2_7 0x3D
#define SCAN_CODE_SET_2_8 0x3E
#define SCAN_CODE_SET_2_9 0x46
#define SCAN_CODE_SET_2_SPACE 0x29
#define SCAN_CODE_SET_2_CAPS_LOCK 0x58
#define SCAN_CODE_SET_2_LEFT_SHIFT 0x12
#define SCAN_CODE_SET_2_LEFT_CONTROL 0x14
#define SCAN_CODE_SET_2_LEFT_ALT 0x11
#define SCAN_CODE_SET_2_ESCAPE 0x76
#define SCAN_CODE_SET_2_BACKSPACE 0x66
#define SCAN_CODE_SET_2_TAB 0x0D
#define SCAN_CODE_SET_2_ENTER 0x5A
#define SCAN_CODE_SET_2_DOT 0x49
#define SCAN_CODE_SET_2_SEMICOLON 0x4C
#define SCAN_CODE_SET_2_APOSTROPHE 0x52
#define SCAN_CODE_SET_2_COMMA 0x41
#define SCAN_CODE_SET_2_TILDE 0x0E
#define SCAN_CODE_SET_2_LEFT_BRACKET 0x54
#define SCAN_CODE_SET_2_RIGHT_BRACKET 0x5B
#define SCAN_CODE_SET_2_EQUAL 0x55
#define SCAN_CODE_SET_2_MINUS 0x4E
#define SCAN_CODE_SET_2_NUM_LOCK 0x77
#define SCAN_CODE_SET_2_SCROLL_LOCK 0x7E
#define SCAN_CODE_SET_2_FORWARD_SLASH 0x4A
#define SCAN_CODE_SET_2_BACKSLASH 0x5D

/* Manages the variable that keeps update of the last key pressed. */
void ps2_update_key(void);

/* Functions for helping form bytes for the 0x64 port. */
uint8_t ps2_status_byte(struct ps2_status_register status);
uint8_t ps2_command_byte(struct ps2_command_register command);

bool ps2_initialize_keyboard(uint8_t scan_set);

/* Scan code set 2 jump table. Does not handle control keys, only
/  printable ones. */
char_ht ps2_scs2_get_key(uint_fast8_t scan_code);


#endif
