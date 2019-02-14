#ifndef DESCRIPTORS_H
#define DESCRIPTORS_H

#include <havk.h>
#include <havk/terminal.h>
#include <havk/interrupts.h>
//#include <havk/PIT.h>

/* TODO: Implement bit fields in GDT entry structure when bothered to do so.
/  The reason for that is so we have more configuration choice over entries.
/  I've merged them for the time being.
/  https://wiki.osdev.org/Global_Descriptor_Table#Structure
/  https://wiki.osdev.org/Getting_to_Ring_3#GDT */
struct gdt_entry
{
	uint16_t limit_low;
	uint16_t base_low;
	uint8_t base_middle;
	uint8_t access_type;
	uint8_t granularity;
	uint8_t base_high;
} __attribute__((packed));

/* I have decided to use bit fields for this one. Needs to be
/  double-checked against the different structure layout indicated in the wiki.
/  So far, it seems to be valid.
/  https://wiki.osdev.org/IDT#Structure_IA-32 */
struct idt_gate
{
	uint16_t base_low;
	uint16_t selector; /* Points to GDT descriptor. */
	uint8_t null; /* Must be 0. Unused and empty. */
	uint8_t type : 4; /* Attribute begins. Bits: 40, 41, 42, 43. */
	uint8_t storage_segment : 1; /* Bit: 44. */
	uint8_t privilege_level : 2; /* Bits: 45, 46. */
	uint8_t present : 1; /* Bit: 47. Attribute ends, full 8 bits. */
	uint16_t base_high;
} __attribute__((packed));

/* Types of IDT gates. Mostly just going to use 32-bit interrupts. */
#define IDT_GATE_32BIT_TASK 0x5
#define IDT_GATE_16BIT_INTERRUPT 0x6
#define IDT_GATE_16BIT_TRAP 0x7
#define IDT_GATE_32BIT_INTERRUPT 0xE
#define IDT_GATE_32BIT_TRAP 0xF

struct descriptor_table
{
	uint16_t limit;
	uint32_t base;
} __attribute__((packed));

/* The link below explains the layout, hopefully I've done it correct:
/  https://wiki.osdev.org/Task_State_Segment */
struct tss32bit
{
	uint16_t link, link_reserved;
	uint32_t esp0;
	uint16_t ss0, ss0_reserved;
	uint32_t esp1;
	uint16_t ss1, ss1_reserved;
	uint32_t esp2;
	uint16_t ss2, ss2_reserved;
	uint32_t cr3;
	uint32_t eip;
	uint32_t eflags;
	uint32_t eax;
	uint32_t ecx;
	uint32_t edx;
	uint32_t ebx;
	uint32_t esp;
	uint32_t ebp;
	uint32_t esi;
	uint32_t edi;
	uint16_t es, es_reserved;
	uint16_t cs, cs_reserved;
	uint16_t ss, ss_reserved;
	uint16_t ds, ds_reserved;
	uint16_t fs, fs_reserved;
	uint16_t gs, gs_reserved;
	uint16_t ldtr, ldtr_reserved;
	uint16_t iopb_reserved, iopb;
} __attribute__((packed));

/* Increase GDT entry limit if anymore are needed. IDT gate max is 0xFF. */
#define GDT_ENTRY_LIMIT 6
#define IDT_GATE_LIMIT 255

struct descriptor_table gdt;
struct gdt_entry gdt_entries[GDT_ENTRY_LIMIT];
struct tss32bit tss;

struct descriptor_table idt;
struct idt_gate idt_gates[IDT_GATE_LIMIT];

void gdt_initialize(void);
void idt_initialize(void);

void gdt_encode_entry(uint16_t index, uint32_t limit, uint32_t base,
	uint8_t access, uint8_t granularity);

void idt_encode_gate(uint16_t index, uintptr_t offset, uint16_t selector,
	uint8_t type, uint8_t privilege_level);

void idt_set_gate(uint16_t num, uintptr_t base, uint16_t sel, uint8_t flags);

void gdt_load(uintptr_t gdt_pointer);
void tss_load(uint16_t tss_descriptor);
void idt_load(uintptr_t idt_pointer);

#endif
