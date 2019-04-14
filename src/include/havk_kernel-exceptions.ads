PACKAGE HAVK_Kernel.Exceptions
IS
	PROCEDURE Last_Chance_Handler;
	PRAGMA Export(C, Last_Chance_Handler, "__gnat_last_chance_handler");

	PROCEDURE Stack_Smash_Handler;
	PRAGMA Export(C, Stack_Smash_Handler, "__stack_chk_fail");
END HAVK_Kernel.Exceptions;
