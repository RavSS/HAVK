#ifndef PIC_H
#define PIC_H

#include <havk.h>
#include <havk/io.h>
#include <havk/interrupts.h>

/* Information for manipulating the progammable interrupt controller (PIC):
/  https://wiki.osdev.org/8259_PIC */

#define PIC_MASTER_COMMAND 0x20
#define PIC_MASTER_DATA 0x21
#define PIC_SLAVE_COMMAND 0xA0
#define PIC_SLAVE_DATA 0XA1
#define PIC_END_OF_INTERRUPT 0x20
#define PIC_REQUEST_REGISTER 0x0A
#define PIC_SERVICE_REGISTER 0x0B

/* Will be of use if I decide to use the APIC instead later on. */
#define pic_disable() do\
{\
	out_byte(0xFF, PIC_MASTER_DATA);\
	out_byte(0xFF, PIC_SLAVE_DATA);\
} while(0)

/* For resetting the PICs after an interrupt has been dealt with. */
#define pic_master_reset() out_byte(PIC_END_OF_INTERRUPT, PIC_MASTER_COMMAND)
#define pic_slave_reset() out_byte(PIC_END_OF_INTERRUPT, PIC_SLAVE_COMMAND)

void pic_initialize_remap(void);

/* Used for reading the IS and IR registers (ISR and IRR). Currently not
/  being utilized, but may come very useful, so I've made them anyway.
/  The command ports for both PICs can be combined if you wish to return
/  both PIC's registers as one. */
uint16_t pic_retrieve(uint8_t read_request, uint8_t pic);

#endif
