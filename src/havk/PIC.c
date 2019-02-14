#include <havk/PIC.h>

/* TODO: I call this from an assembly file, so it's probably smarter to just
/  turn this into an assembly file too, seeing as how it uses a single
/  function that only utilizes inline assembly anyway. */
void pic_initialize_remap(void)
{
	/* Initialize the PICs. */
	out_byte(0x11, PIC_MASTER_COMMAND);
	out_byte(0x11, PIC_SLAVE_COMMAND);

	/* Start the IRQ vector where IRQ_0 begins at according to us.
	/  The reasoning for this is because IRQ 0 to IRQ 7 are clashed with
	/  the CPU exception interrupts (8 to 15). */
	out_byte(IRQ_0, PIC_MASTER_DATA);

	/* We don't need to remap the slave PICs interrupt vector, as it goes
	/  from 112 to 120. I'm going to do it anyway so all IRQs line up. */
	out_byte(IRQ_8, PIC_SLAVE_DATA);

	/* Need to cascade interrupts.
	/      0 - 1 - 2 - 3 - 4 - 5 - 6 - 7
	/          |
	/  0 - 1 - 2 - 3 - 4 - 5 - 6 - 7 - 8
	/  IRQ 2 cascades to IRQ 1 (second PIC IRQ 1 becomes IRQ 8).
	/  New layout and priority:
	/  0 > 1 > 8 > 9 > 10 > 11 > 12 > 13 > 14 > 15 > 3 > 4 > 5 > 6 > 7 */

	/* Inform master PIC that there's another (slave) PIC at IRQ 2.
	/  (1 << [IRQ] 2) = [Binary Form] 100 = [Hex Form] 4. */
	out_byte(0x4, PIC_MASTER_DATA);

	/* Notify the slave PIC that interrupts will cascade
	/  from the master PIC's IRQ 2.
	/  (1 << [IRQ] 1) = [Binary Form] 10 = [Hex Form] 2. */
	out_byte(0x2, PIC_SLAVE_DATA);

	/* Put PICs into 8086 mode. */
	out_byte(0x1, PIC_MASTER_DATA);
	out_byte(0x1, PIC_SLAVE_DATA);

	/* Clear any interrupt masks. */
	out_byte(0x0, PIC_MASTER_DATA);
	out_byte(0x0, PIC_SLAVE_DATA);
}

uint16_t pic_retrieve(uint8_t read_request, uint8_t pic)
{
	/* Validate the read request. */
	if ((read_request != PIC_REQUEST_REGISTER)
		|| (read_request != PIC_SERVICE_REGISTER))
		return 0;
	else if (pic == PIC_MASTER_COMMAND || pic == PIC_SLAVE_COMMAND)
	{
		out_byte(read_request, pic);
		return in_byte(pic);
	}
	else if (pic == (PIC_MASTER_COMMAND + PIC_SLAVE_COMMAND))
	{
		out_byte(read_request, PIC_SLAVE_COMMAND);
		out_byte(read_request, PIC_MASTER_COMMAND);
		return (in_byte(PIC_SLAVE_COMMAND) << 8)
			| in_byte(PIC_MASTER_COMMAND);
	}
	else return 0;
}
