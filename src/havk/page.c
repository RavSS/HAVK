#include <havk/page.h>

void tlb_flush_all(void)
{
	__asm__ volatile
	(
		"MOV ECX, CR3;"
		"MOV CR3, ECX;"
		::: "ecx"
	);
}