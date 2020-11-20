###############################################################################
## Program         -- HAVK Operating System                                  ##
## Filename        -- utilities.s                                            ##
## License         -- GNU General Public License version 3.0                 ##
## Original Author -- Ravjot Singh Samra, Copyright 2020                     ##
###############################################################################

.SECTION .text

.GLOBAL assembly__output_byte
.TYPE assembly__output_byte, @function
# (RDI => port, RSI => value)
assembly__output_byte:
	MOV DX, DI
	MOV AL, SIL
	OUT DX, AL
	RET

.GLOBAL assembly__output_word
.TYPE assembly__output_word, @function
# (RDI => port, RSI => value)
assembly__output_word:
	MOV DX, DI
	MOV AX, SI
	OUT DX, AX
	RET

.GLOBAL assembly__input_byte
.TYPE assembly__input_byte, @function
# (RDI => port) >> (RAX => inputted byte)
assembly__input_byte:
	XOR RAX, RAX
	MOV DX, DI
	IN AL, DX
	RET

.GLOBAL assembly__input_word
.TYPE assembly__input_word, @function
# (RDI => port) >> (RAX => inputted word)
assembly__input_word:
	XOR RAX, RAX
	MOV DX, DI
	IN AX, DX
	RET
