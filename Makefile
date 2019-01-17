CC=crosscc/bin/i686-elf-gcc
NAME=HAVK
VER=\"1.00-00\"
OPTIM=-O2
WARN=-Wall -Wextra -pedantic
LIB=-lgcc
DEF=-D VERSION=$(VER)
STD=-std=c99
SRCDIR=./src/
BUILDDIR=./build/
IMGDIR=./img/

textos: $(SRCDIR)kernel.c
	if [ ! -d "$(BUILDDIR)" ]; then mkdir $(BUILDDIR); fi
	nasm -f elf32 $(SRCDIR)boot.asm -o $(BUILDDIR)boot.o
	$(CC) -c $(SRCDIR)kernel.c -o $(BUILDDIR)kernel.o $(STD) $(OPTIM) $(WARN) -ffreestanding
	$(CC) -T $(SRCDIR)linker.ld -o $(IMGDIR)boot/$(NAME).bin $(OPTIM) $(BUILDDIR)boot.o $(BUILDDIR)kernel.o -nostdlib -ffreestanding
	grub-mkrescue -o $(NAME).iso $(IMGDIR)
clean:
	rm -v ./$(NAME).iso
	rm -vr $(BUILDDIR)
