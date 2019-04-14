WITH System.Machine_Code;
WITH Interfaces;

USE System.Machine_Code;
USE Interfaces;

PACKAGE BODY HAVK_Kernel.Exceptions
IS
	PROCEDURE Last_Chance_Handler
	IS
		Magic : CONSTANT unsigned_64 := 16#0ADAC0DEDEADC0DE#;
	BEGIN
		LOOP
			Asm( -- Works for now as a quick indicator.
				"MOV RAX, %0;" &
				"MOV RBX, %0;" &
				"MOV RCX, %0;" &
				"MOV RDX, %0;" &
				"MOV RSI, %0;" &
				"MOV RDI, %0;" &
				"MOV RSP, %0;" &
				"MOV R8,  %0;" &
				"MOV R9,  %0;" &
				"MOV R10, %0;" &
				"MOV R11, %0;" &
				"MOV R12, %0;" &
				"MOV R13, %0;" &
				"MOV R14, %0;" &
				"MOV R15, %0;" &
				"CLI; HLT;",
				Outputs => No_Output_Operands,
				Inputs => unsigned_64'Asm_Input("g", Magic),
				Clobber => "rax, rbx, rcx, rdx, rsi, rdi," &
					"rsp, r8, r9, r10, r11, r12, r13," &
					"r14, r15",
				Volatile => True);
		END LOOP;
	END Last_Chance_Handler;

	-- Just a wrapper for the last chance handler for now, as a
	-- symbol is created for the stack check fail if stack protection
	-- is enabled during compilation.
	PROCEDURE Stack_Smash_Handler
	IS
	BEGIN
		Last_Chance_Handler;
	END Stack_Smash_Handler;
END HAVK_Kernel.Exceptions;
