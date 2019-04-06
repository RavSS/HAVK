#include <havk/descriptors.h>

/* Most of the code revolving around descriptors and interrupts for
/  HAVK is devolved from the information at:
/  https://www.intel.com/content/www/us/en/architecture-and-technology
/	/64-ia-32-architectures-software-developer-vol-3a-part-1-manual.html */

void gdt_initialize(void)
{
	/* https://wiki.osdev.org/TSS#TSS_in_software_multitasking */
	tss.ss0 = 0x10; /* Kernel data descriptor is at GDT entry 3. */
	/* TODO: This needs to point at the kernel stack pointer, I think? */
	tss.esp0 = 0xDEADC0DE; /* Updated every time Ring 0 is reached? */
	tss.iopb = sizeof(tss); /* Don't think I'll need this. Disabled. */

	/* First segment descriptor must be a null descriptor. */
	gdt_encode_entry(0, 0, 0, 0, 0);
	/* Code segment descriptor. */
	gdt_encode_entry(1, 0xFFFFFFFF, 0, 0x9A, 0xCF);
	/* Data segment descriptor. */
	gdt_encode_entry(2, 0xFFFFFFFF, 0, 0x92, 0xCF);
	/* User-level code segment descriptor. */
	gdt_encode_entry(3, 0xFFFFFFFF, 0, 0xFA, 0xCF);
	/* User-level data segment descriptor. */
	gdt_encode_entry(4, 0xFFFFFFFF, 0, 0xF2, 0xCF);
	/* Task state segment descriptor. I will need this, as HAVK will
	/  incorporate privilege ring levels for security. */
	gdt_encode_entry(5, sizeof(tss), (uint32_t) &tss, 0x89, 0xCF);

	gdt.limit = sizeof(struct gdt_entry) * GDT_ENTRY_LIMIT;
	gdt.base = (uintptr_t) &gdt_entries;

	gdt_load((uintptr_t) &gdt);
	tss_load(0x28); /* May as well shove this in here. TSS at entry 6. */
}

void idt_initialize(void)
{
	#define interrupt_setup(x, y, z) idt_encode_gate((x), (uintptr_t) y,\
		0x08, IDT_GATE_32BIT_INTERRUPT, (z))
	#define trap_setup(x, y) idt_encode_gate((x), (uintptr_t) y,\
		0x08, IDT_GATE_32BIT_TRAP, 0)

	/* Setup CPU interrupts. */
	interrupt_setup(0, isr_handler_0, 0);
	interrupt_setup(1, isr_handler_1, 0);
	interrupt_setup(2, isr_handler_2, 0);

	/* Interrupts 3 and 4 are traps, the rest are fault interrupts. */
	trap_setup(3, isr_handler_3);
	trap_setup(4, isr_handler_4);
	#undef trap_setup

	interrupt_setup(5, isr_handler_5, 0);
	interrupt_setup(6, isr_handler_6, 0);
	interrupt_setup(7, isr_handler_7, 0);
	interrupt_setup(8, isr_handler_8, 0);
	interrupt_setup(9, isr_handler_9, 0);
	interrupt_setup(10, isr_handler_10, 0);
	interrupt_setup(11, isr_handler_11, 0);
	interrupt_setup(12, isr_handler_12, 0);
	interrupt_setup(13, isr_handler_13, 0);
	interrupt_setup(14, isr_handler_14, 0);
	interrupt_setup(15, isr_handler_15, 0);
	interrupt_setup(16, isr_handler_16, 0);
	interrupt_setup(17, isr_handler_17, 0);
	interrupt_setup(18, isr_handler_18, 0);
	interrupt_setup(19, isr_handler_19, 0);
	interrupt_setup(20, isr_handler_20, 0);
	interrupt_setup(21, isr_handler_21, 0);
	interrupt_setup(22, isr_handler_22, 0);
	interrupt_setup(23, isr_handler_23, 0);
	interrupt_setup(24, isr_handler_24, 0);
	interrupt_setup(25, isr_handler_25, 0);
	interrupt_setup(26, isr_handler_26, 0);
	interrupt_setup(27, isr_handler_27, 0);
	interrupt_setup(28, isr_handler_28, 0);
	interrupt_setup(29, isr_handler_29, 0);
	interrupt_setup(30, isr_handler_30, 0);
	interrupt_setup(31, isr_handler_31, 0);

	/* IRQ setup now begins. */
	interrupt_setup(IRQ_0, irq_handler_0, 3);
	interrupt_setup(IRQ_1, irq_handler_1, 3);
	interrupt_setup(IRQ_2, irq_handler_2, 3);
	interrupt_setup(IRQ_3, irq_handler_3, 3);
	interrupt_setup(IRQ_4, irq_handler_4, 3);
	interrupt_setup(IRQ_5, irq_handler_5, 3);
	interrupt_setup(IRQ_6, irq_handler_6, 3);
	interrupt_setup(IRQ_7, irq_handler_7, 3);
	interrupt_setup(IRQ_8, irq_handler_8, 3);
	interrupt_setup(IRQ_9, irq_handler_9, 3);
	interrupt_setup(IRQ_10, irq_handler_10, 3);
	interrupt_setup(IRQ_11, irq_handler_11, 3);
	interrupt_setup(IRQ_12, irq_handler_12, 3);
	interrupt_setup(IRQ_13, irq_handler_13, 3);
	interrupt_setup(IRQ_14, irq_handler_14, 3);
	interrupt_setup(IRQ_15, irq_handler_15, 3);

	/* Setup HAVK interrupts. */
	interrupt_setup(153, isr_handler_153, 3);

	#undef interrupt_setup

	idt.limit = sizeof(struct idt_gate) * IDT_GATE_LIMIT;
	idt.base = (uintptr_t) &idt_gates;

	idt_load((uintptr_t) &idt);
}

void gdt_encode_entry(uint16_t index, uint32_t limit, uint32_t base,
	uint8_t access_type, uint8_t granularity)
{
	gdt_entries[index].limit_low = (limit & 0xFFFF);

	gdt_entries[index].base_low = (base & 0xFFFF);
	gdt_entries[index].base_middle = (base >> 16) & 0xFF;
	gdt_entries[index].base_high = (base >> 24) & 0xFF;

	gdt_entries[index].access_type = access_type;

	gdt_entries[index].granularity = (limit >> 16) & 0x0F;
	gdt_entries[index].granularity |= granularity & 0xF0;
}

void idt_encode_gate(uint16_t index, uintptr_t offset, uint16_t selector,
	uint8_t type, uint8_t privilege_level)
{
	idt_gates[index].base_low = offset & 0xFFFF;

	idt_gates[index].selector = selector;

	idt_gates[index].null = 0;

	idt_gates[index].type = type;
	idt_gates[index].storage_segment = 0;
	idt_gates[index].privilege_level = privilege_level;
	idt_gates[index].present = 1;

	idt_gates[index].base_high = (offset >> 16) & 0xFFFF;
}

void gdt_load(uintptr_t gdt_pointer)
{
	/* Disable interrupts just in case. */
	__asm__ volatile("CLI; LGDT [%0];" :: "r" (gdt_pointer));

	/* Now reload the memory segments. */
	__asm__ volatile
	(
		".reload_code_segment:"
			"JMP 0x8: .reload_other_segments;"
		".reload_other_segments:"
			"MOV AX, 0x10;"
			"MOV DS, AX;"
			"MOV ES, AX;"
			"MOV FS, AX;"
			"MOV GS, AX;"
			"MOV SS, AX;"
		::: "ax"
	);
}

void tss_load(uint16_t tss_descriptor)
{
	__asm__ volatile("LTR %0" :: "r" (tss_descriptor));
}

void idt_load(uintptr_t idt_pointer)
{
	__asm__ volatile("LIDT [%0]" :: "r" (idt_pointer));
}
