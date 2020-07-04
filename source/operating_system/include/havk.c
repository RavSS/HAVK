#include <havk.h>

syserr_ht syscall(syscall_ht operation, sysargs_ht *arguments)
{
	register syserr_ht error_value;

	__asm__ volatile
	(
		"MOV EDX, %5;"
		"MOV RSI, %1;"
		"MOV RDX, %2;"
		"MOV R8, %3;"
		"MOV R9, %4;"
		"SYSCALL;"
		"MOV %0, EAX;"
		"MOV %1, RSI;"
		"MOV %2, RDX;"
		"MOV %3, R8;"
		"MOV %4, R9;"
		: "=r" (error_value), "+rm" (arguments->argument_1),
			"+rm" (arguments->argument_2),
			"+rm" (arguments->argument_3),
			"+rm" (arguments->argument_4)
		: "g" (operation)
		: "rax", "rcx", "rdi", "rdx", "rsi", "r8", "r9", "r10", "r11",
			"memory", "cc"
	);

	return error_value;
}


