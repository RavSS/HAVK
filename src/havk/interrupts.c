#include <havk/interrupts.h>

/* I've decided to give each interrupt its own handler for specific
/  operations. I could have used a bunch of wrappers to a common interrupt
/  handling function, but that may give me less flexibility with a specific
/  interrupt's control flow. This file is going to be very bloated. */

/* WARNING: I'm not sure if I have used the interrupt attribute provided
/  by GCC properly. It's documented sparsely with no examples. Seems to work
/  so far. All I know is that it pushes all registers at the start, pops all
/  registers at the end, and then does an interrupt return or "IRET".
/  https://software.intel.com/en-us/node/734271 */

/***************************** CPU Interrupts ********************************/
__attribute__ ((interrupt))
void isr_handler_0(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 0 - '%s' - CS:%H - EIP:%H",
		INT_0_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_1(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 1 - '%c' - CS:%H - EIP:%H",
		INT_1_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_2(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 2 - '%s' - CS:%H - EIP:%H",
		INT_2_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_3(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 3 - '%s' - CS:%H - EIP:%H",
		INT_3_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_4(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 4 - '%s' - CS:%H - EIP:%H",
		INT_4_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_5(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 5 - '%s' - CS:%H - EIP:%H",
		INT_5_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_6(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 6 - '%s' - CS:%H - EIP:%H",
		INT_6_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_7(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 7 - '%s' - CS:%H - EIP:%H",
		INT_7_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_8(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 8 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_8_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_9(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 9 - '%s' - CS:%H - EIP:%H",
		INT_9_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_10(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 10 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_10_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_11(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 11 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_11_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_12(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 12 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_12_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_13(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 13 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_13_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_14(struct interrupt *interrupted, uint32_t error)
{
	uint32_t cr2;

	__asm__ volatile ("MOV %0, CR2" : "=r" (cr2));
	printf("\r[INTERRUPT]: 14 (ERROR:%d) - '%s' - CS:%H - EIP:%H\r"
		"\tCR2 (FAULT ADDRESS):%H",
		error, INT_14_DESC, interrupted->cs, interrupted->eip, cr2);
}

__attribute__ ((interrupt))
void isr_handler_15(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 15 - '%s' - CS:%H - EIP:%H",
		INT_15_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_16(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 16 - '%s' - CS:%H - EIP:%H",
		INT_16_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_17(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 17 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_17_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_18(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 18 - '%s' - CS:%H - EIP:%H",
		INT_18_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_19(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 19 - '%s' - CS:%H - EIP:%H",
		INT_19_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_20(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 20 - '%s' - CS:%H - EIP:%H",
		INT_20_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_21(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 21 - '%s' - CS:%H - EIP:%H",
		INT_21_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_22(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 22 - '%s' - CS:%H - EIP:%H",
		INT_22_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_23(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 23 - '%s' - CS:%H - EIP:%H",
		INT_23_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_24(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 24 - '%s' - CS:%H - EIP:%H",
		INT_24_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_25(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 25 - '%s' - CS:%H - EIP:%H",
		INT_25_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_26(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 26 - '%s' - CS:%H - EIP:%H",
		INT_26_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_27(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 27 - '%s' - CS:%H - EIP:%H",
		INT_27_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_28(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 28 - '%s' - CS:%H - EIP:%H",
		INT_28_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_29(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 29 - '%s' - CS:%H - EIP:%H",
		INT_29_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_30(struct interrupt *interrupted, uint32_t error)
{
	printf("\r[INTERRUPT]: 30 (ERROR:%d) - '%s' - CS:%H - EIP:%H",
		error, INT_30_DESC, interrupted->cs, interrupted->eip);
}

__attribute__ ((interrupt))
void isr_handler_31(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 31 - '%s' - CS:%H - EIP:%H",
		INT_31_DESC, interrupted->cs, interrupted->eip);
}

/***************************** IRQ Interrupts ********************************/
__attribute__ ((interrupt))
void irq_handler_0(__attribute__ ((unused)) struct interrupt *interrupted)
{
	++tick;
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_1(__attribute__ ((unused)) struct interrupt *interrupted)
{
	ps2_update_key();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_2(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 2) %d - '%s' - CS:%H - EIP:%H",
		IRQ_2, IRQ_2_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_3(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 3) %d - '%s' - CS:%H - EIP:%H",
		IRQ_3, IRQ_3_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_4(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 4) %d - '%s' - CS:%H - EIP:%H",
		IRQ_4, IRQ_4_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_5(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 5) %d - '%s' - CS:%H - EIP:%H",
		IRQ_5, IRQ_5_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_6(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 6) %d - '%s' - CS:%H - EIP:%H",
		IRQ_6, IRQ_6_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_7(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 7) %d - '%s' - CS:%H - EIP:%H",
		IRQ_7, IRQ_7_DESC, interrupted->cs, interrupted->eip);

	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_8(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 8) %d - '%s' - CS:%H - EIP:%H",
		IRQ_8, IRQ_8_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_9(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 9) %d - '%s' - CS:%H - EIP:%H",
		IRQ_9, IRQ_9_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_10(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 10) %d - '%s' - CS:%H - EIP:%H",
		IRQ_10, IRQ_10_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_11(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 11) %d - '%s' - CS:%H - EIP:%H",
		IRQ_11, IRQ_11_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_12(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 12) %d - '%s' - CS:%H - EIP:%H",
		IRQ_12, IRQ_12_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_13(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 13) %d - '%s' - CS:%H - EIP:%H",
		IRQ_13, IRQ_13_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_14(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 14) %d - '%s' - CS:%H - EIP:%H",
		IRQ_14, IRQ_14_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

__attribute__ ((interrupt))
void irq_handler_15(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: (IRQ 15) %d - '%s' - CS:%H - EIP:%H",
		IRQ_15, IRQ_15_DESC, interrupted->cs, interrupted->eip);

	pic_slave_reset();
	pic_master_reset();
}

/***************************** HAVK Interrupts *******************************/
__attribute__ ((interrupt))
void isr_handler_153(struct interrupt *interrupted)
{
	printf("\r[INTERRUPT]: 153 - '%s' - CS:%H - EIP:%H",
		INT_153_DESC, interrupted->cs, interrupted->eip);
}
