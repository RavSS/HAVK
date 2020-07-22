#include <havk.h>

// TODO: Implement this in a separate assembly file, outside a function scope,
// or in a function marked with the "naked" attribute (in hopes that it will be
// inlined), as GCC seems to do a lot of extensive register juggling regardless
// of the optimisation level to fit the extended inline assembly statement in.
// Perhaps my inline assembly's constraints are not specified in an optimised
// manner. I could handle the entire sequence in non-extended inline assembly
// ("Basic Asm") i.e. accessing the structure pointer directly through RDI and
// `RET`ing in it as well.
syserr_ht syscall(sysargs_ht *arguments)
{
	register syserr_ht error_value;

	__asm__ volatile
	(
		"MOV RDI, %[operation];"
		"MOV RSI, %[argument_1];"
		"MOV RDX, %[argument_2];"
		"MOV R8, %[argument_3];"
		"MOV R9, %[argument_4];"
		"MOV R10, %[argument_5];"
		"SYSCALL;"
		"MOV %[error], EAX;"
		"MOV %[argument_1], RSI;"
		"MOV %[argument_2], RDX;"
		"MOV %[argument_3], R8;"
		"MOV %[argument_4], R9;"
		"MOV %[argument_5], R10;"
		: [error] "=r" (error_value),
			[argument_1] "+rm" (arguments->argument_1),
			[argument_2] "+rm" (arguments->argument_2),
			[argument_3] "+rm" (arguments->argument_3),
			[argument_4] "+rm" (arguments->argument_4),
			[argument_5] "+rm" (arguments->argument_5)
		: [operation] "g" ((uint64_t) arguments->operation)
		: "rax", "rcx", "rdi", "rdx", "rsi", "r8", "r9", "r10", "r11",
			"memory", "cc"
	);

	return error_value;
}

void memset(void *area, int value, size_t bytes)
{
	for (register unsigned char *i = area; bytes--; *i++ = value);
}
