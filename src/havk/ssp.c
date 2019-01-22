#include <havk/string.h>
#include <havk/terminal.h>

void __stack_chk_fail(void)
{
	while (1);
}
