#ifndef INTERRUPTS_H
#define INTERRUPTS_H

#include <havk.h>
#include <havk/terminal.h>
#include <havk/io.h>
#include <havk/PIC.h>
#include <havk/PIT.h>
#include <havk/PS2.h>

/* GCC demands this structure for an interrupt attribute function. */
struct interrupt
{
	/* Passed by the CPU. I think it's the right order? */
	uint32_t eip;
	uint32_t cs;
	uint32_t eflags;
	uint32_t esp;
	uint32_t ss;
} __attribute__((packed));

#define interrupt_enable() __asm__ volatile ("STI")
#define interrupt_disable() __asm__ volatile ("CLI")
#define interrupt_wait() __asm__ volatile ("HLT")

/* May as well put these in definitions as opposed to variable storage.
/  The link below shows all CPU interrupts (except we don't use an IVT).
/  https://wiki.osdev.org/Interrupt_Vector_Table */
#define INT_0_DESC "Division by Zero"
#define INT_1_DESC "Reserved"
#define INT_2_DESC "NMI Interruption"
#define INT_3_DESC "Breakpoint"
#define INT_4_DESC "Overflow"
#define INT_5_DESC "Out-of-Bounds"
#define INT_6_DESC "Invalid Opcode"
#define INT_7_DESC "Device Unavailable"
#define INT_8_DESC "Double Fault"
#define INT_9_DESC "Math Coprocessor Page or Segment Violation"
#define INT_10_DESC "Invalid TSS"
#define INT_11_DESC "Segment Not Present"
#define INT_12_DESC "Stack-Segment Fault"
#define INT_13_DESC "General Protection Fault"
#define INT_14_DESC "Page Fault"
#define INT_15_DESC "Reserved"
#define INT_16_DESC "Math Coprocessor FPU Error"
#define INT_17_DESC "Alignment Check"
#define INT_18_DESC "Machine Check"
#define INT_19_DESC "SIMD Floating-Point Error"
#define INT_20_DESC "Reserved"
#define INT_21_DESC "Reserved"
#define INT_22_DESC "Reserved"
#define INT_23_DESC "Reserved"
#define INT_24_DESC "Reserved"
#define INT_25_DESC "Reserved"
#define INT_26_DESC "Reserved"
#define INT_27_DESC "Reserved"
#define INT_28_DESC "Reserved"
#define INT_29_DESC "Reserved"
#define INT_30_DESC "Reserved"
#define INT_31_DESC "Reserved"

/* 32 to 255 is all user-defined; however, due to the dumb design of IRQs
/  clashing with CPU exceptions (8 to 15), they must be remapped later on.
/  https://wiki.osdev.org/Interrupts
/	#General_IBM-PC_Compatible_Interrupt_Information */
/* First PIC (master). */
#define INT_32_DESC "PIT"
#define INT_33_DESC "Keyboard"
#define INT_34_DESC "Cascade To Another PIC"
#define INT_35_DESC "COM2"
#define INT_36_DESC "COM1"
#define INT_37_DESC "LPT2"
#define INT_38_DESC "Floppy Disk"
#define INT_39_DESC "LPT1 or Spurious Interrupt"
/* Second PIC (slave). */
#define INT_40_DESC "CMOS RTC"
#define INT_41_DESC "Peripheral"
#define INT_42_DESC "Peripheral"
#define INT_43_DESC "Peripheral"
#define INT_44_DESC "PS/2 Mouse"
#define INT_45_DESC "Coprocessor"
#define INT_46_DESC "Primary ATA HDD"
#define INT_47_DESC "Secondary ATA HDD"

/* Just in case I need to remap them somewhere else for whatever reason. */
/* First PIC (master). */
#define IRQ_0 32
#define IRQ_0_DESC INT_32_DESC
#define IRQ_1 33
#define IRQ_1_DESC INT_33_DESC
#define IRQ_2 34
#define IRQ_2_DESC INT_34_DESC
#define IRQ_3 35
#define IRQ_3_DESC INT_35_DESC
#define IRQ_4 36
#define IRQ_4_DESC INT_36_DESC
#define IRQ_5 37
#define IRQ_5_DESC INT_37_DESC
#define IRQ_6 38
#define IRQ_6_DESC INT_38_DESC
#define IRQ_7 39
#define IRQ_7_DESC INT_39_DESC
/* Second PIC (slave). */
#define IRQ_8 40
#define IRQ_8_DESC INT_40_DESC
#define IRQ_9 41
#define IRQ_9_DESC INT_41_DESC
#define IRQ_10 42
#define IRQ_10_DESC INT_42_DESC
#define IRQ_11 43
#define IRQ_11_DESC INT_43_DESC
#define IRQ_12 44
#define IRQ_12_DESC INT_44_DESC
#define IRQ_13 45
#define IRQ_13_DESC INT_45_DESC
#define IRQ_14 46
#define IRQ_14_DESC INT_46_DESC
#define IRQ_15 47
#define IRQ_15_DESC INT_47_DESC

/* Now my own interrupts begin. */
#define INT_153_DESC "Ravjot's Interrupt"

/* CPU interrupt handlers. According to Table 6-1 in Intel's IA-32 Vol-3A
/  manual (6-2), interrupts 8, 10 to 14, 17, and 30 pass errors. */
void isr_handler_0(struct interrupt *interrupted);
void isr_handler_1(struct interrupt *interrupted);
void isr_handler_2(struct interrupt *interrupted);
void isr_handler_3(struct interrupt *interrupted);
void isr_handler_4(struct interrupt *interrupted);
void isr_handler_5(struct interrupt *interrupted);
void isr_handler_6(struct interrupt *interrupted);
void isr_handler_7(struct interrupt *interrupted);
void isr_handler_8(struct interrupt *interrupted, uint32_t error);
void isr_handler_9(struct interrupt *interrupted);
void isr_handler_10(struct interrupt *interrupted, uint32_t error);
void isr_handler_11(struct interrupt *interrupted, uint32_t error);
void isr_handler_12(struct interrupt *interrupted, uint32_t error);
void isr_handler_13(struct interrupt *interrupted, uint32_t error);
void isr_handler_14(struct interrupt *interrupted, uint32_t error);
void isr_handler_15(struct interrupt *interrupted);
void isr_handler_16(struct interrupt *interrupted);
void isr_handler_17(struct interrupt *interrupted, uint32_t error);
void isr_handler_18(struct interrupt *interrupted);
void isr_handler_19(struct interrupt *interrupted);
void isr_handler_20(struct interrupt *interrupted);
void isr_handler_21(struct interrupt *interrupted);
void isr_handler_22(struct interrupt *interrupted);
void isr_handler_23(struct interrupt *interrupted);
void isr_handler_24(struct interrupt *interrupted);
void isr_handler_25(struct interrupt *interrupted);
void isr_handler_26(struct interrupt *interrupted);
void isr_handler_27(struct interrupt *interrupted);
void isr_handler_28(struct interrupt *interrupted);
void isr_handler_29(struct interrupt *interrupted);
void isr_handler_30(struct interrupt *interrupted, uint32_t error);
void isr_handler_31(struct interrupt *interrupted);

/* IRQ interrupt handlers. */
void irq_handler_0(struct interrupt *interrupted);
void irq_handler_1(struct interrupt *interrupted);
void irq_handler_2(struct interrupt *interrupted);
void irq_handler_3(struct interrupt *interrupted);
void irq_handler_4(struct interrupt *interrupted);
void irq_handler_5(struct interrupt *interrupted);
void irq_handler_6(struct interrupt *interrupted);
void irq_handler_7(struct interrupt *interrupted);
void irq_handler_8(struct interrupt *interrupted);
void irq_handler_9(struct interrupt *interrupted);
void irq_handler_10(struct interrupt *interrupted);
void irq_handler_11(struct interrupt *interrupted);
void irq_handler_12(struct interrupt *interrupted);
void irq_handler_13(struct interrupt *interrupted);
void irq_handler_14(struct interrupt *interrupted);
void irq_handler_15(struct interrupt *interrupted);

/* My personal (or rather HAVK's) interrupt handlers. */
void isr_handler_153(struct interrupt *interrupted);

#endif