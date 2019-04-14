#!/bin/sh

# This shell script helps compiles a cross-compiler for x86.
# An IA-32 C cross compiler is needed to compile the depreciated 32-bit HAVK OS.
# For that, the "i686-elf target" can be used for GCC.
set -e

# These point to source locations for BinUtils and GCC. Update and check them!
BINUTILS_SRC=https://ftp.gnu.org/gnu/binutils/binutils-2.32.tar.gz
GCC_SRC=https://ftp.gnu.org/gnu/gcc/gcc-8.3.0/gcc-8.3.0.tar.gz

# For Make related commands.
CORES=-j`nproc --all`

CUR_DIR=`basename $PWD`

if [ "$CUR_DIR" = "ext" ]
then
	cd ../
elif [ "$CUR_DIR" != "HAVK" -o "$CUR_DIR" != "havk" ]
then
	echo "You ran this script from basename '$CUR_DIR' at '$PWD'"
	echo "The basename directory is not 'HAVK' or 'ext'."
	echo "Run this script from one of those directories."
	echo "Modify the script if you surely want to continue."
	echo "You can also rename your current directory to the default names."
	exit 1
fi

if [ \
"$1" = "i686-elf" -o \
"$1" = "x86_64-w64-mingw32" \
]
then
	echo "Now compiling $1-gcc."
else
	if [ "$2" = "-O" ]
	then
		echo "Overridden. Now trying to compile $1-gcc."
		exit 1
	else
		echo "An invalid target is selected. Override this with '-O'."
		exit 1
	fi
fi

if [ ! -d "./com" ]
then
	mkdir ./com
fi

export TARGET=$1
export PREFIX="`pwd`/com/$TARGET"
export PATH=$PREFIX/bin:$PATH

if [ ! -d "./build" ]
then
	mkdir ./build
fi

cd ./build

if ls ../ext/binutils-*.**.tar.gz 1> /dev/null 2>&1
then
	echo "BinUtils source file already exists."
else
	wget $BINUTILS_SRC -P ../ext/
fi

if ls ../ext/gcc-*.*.*.tar.gz 1> /dev/null 2>&1
then
	echo "GCC source file already exists."
else
	wget $GCC_SRC -P ../ext/
fi

if [ ! -d "./binutils-*.**" ]
then
	tar -C ./ -xvzf ../ext/binutils-*.**.tar.gz
fi

if [ ! -d "./gcc-*.*.*" ]
then
	tar -C ./ -xvzf ../ext/gcc-*.*.*.tar.gz
fi

if [ ! -d "./binutils_$TARGET" ]
then
	mkdir ./binutils_$TARGET
else
	rm -rfv ./binutils_$TARGET
	mkdir ./binutils_$TARGET
fi

cd ./binutils_$TARGET

../binutils-*.**/configure --target=$TARGET --prefix="$PREFIX" \
--with-sysroot --disable-nls --disable-werror

make $CORES
make $CORES install

if [ ! -d "../gcc_$TARGET" ]
then
	mkdir ../gcc_$TARGET
else
	rm -rfv ../gcc_$TARGET
	mkdir ../gcc_$TARGET
fi

cd ../gcc_$TARGET

../gcc-*.*.*/configure --target=$TARGET --prefix="$PREFIX" \
--disable-nls --enable-languages=c --without-headers

make $CORES all-gcc
make $CORES all-target-libgcc
make $CORES install-gcc
make $CORES install-target-libgcc
