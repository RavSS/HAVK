SECTION .text

; Tight function to send EOIs to both PICs.
GLOBAL pic_eoi:function (pic_eoi.end - pic_eoi)
pic_eoi:
	MOV AL, 0x20 ; EOI command byte.
	OUT 0x20, AL ; Send EOI to master PIC.
	OUT 0xA0, AL ; Send EOI to slave PIC.
	RETQ
	.end: