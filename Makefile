CC=crosscc/bin/i686-elf-gcc
NAME=HAVK
VER=\"1.00-00 (TESTING)\"
OPTIM=-O2
WARN=-Wall -Wextra -pedantic
LIB=-lgcc
DEF=-D VERSION=$(VER)
STD=-std=c89 -ansi
SRCDIR=./src/
BUILDDIR=./build/
INCLUDEDIR=-I ./src/include/
IMGDIR=./img/

CFILES=$(wildcard src/*.c)
OBJFILES=$(wildcard build/*.o)

all: $(CFILES)
	if [ ! -d "$(BUILDDIR)" ]; then mkdir $(BUILDDIR); fi
	$(CC) -c $^ $(INCLUDEDIR) $(STD) $(OPTIM) $(WARN) -ffreestanding
	mv *.o $(BUILDDIR)
HAVK: $(CFILES)
	nasm -f elf32 $(SRCDIR)boot.asm -o $(BUILDDIR)boot.o
	$(CC) -T $(SRCDIR)linker.ld -o $(IMGDIR)boot/$(NAME).bin $(OPTIM) $(WARN) $(OBJFILES) -nostdlib -ffreestanding
iso:
	grub-mkrescue -o $(NAME).iso $(IMGDIR)
clean:
	rm -v ./$(NAME).iso
	rm -vr $(BUILDDIR)
