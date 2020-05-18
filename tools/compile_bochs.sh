#!/bin/sh
set -e

if [ -z "$1" -o -z "$2" -o "-h" = "$1" -o "--help" = "$1" ]
then
	echo "Usage: $0 <Bochs source directory> <Full output path>"
	echo 'Outputs Bochs binaries, one with GDB support and one without.'
	exit 1
else
	set -x
fi

JOBS=`nproc --all`

export CFLAGS='-O2'
export CXXFLAGS='-O2'
export MAKEFLAGS="-j$JOBS"

define()
{
	sed -i "s/#define $1 ./#define $1 $2/g" config.h
}

configure_defines()
{
	# Without this, a compilation error occurs when networking is enabled.
	define 'BX_NETMOD_FBSD' 0
}

# Compiling with "--enable-avx" doesn't seem to work with version 2.6.10.
#### /usr/bin/ld: cpu/libcpu.a(init.o): in function `BX_CPU_C::initialize()':
#### init.cc:(.text+0x506): undefined reference to
#### 	`create_corei7_icelake_u_cpuid(BX_CPU_C*)'
BXFLAGS='--enable-3dnow --enable-all-optimizations --enable-clgd54xx
	--enable-e1000 --enable-gameport --enable-long-phy-address
	--enable-ne2000 --enable-pci --enable-pnic --enable-sb16 --enable-svm
	--enable-usb --enable-usb-ehci --enable-usb-ohci --enable-usb-xhci
	--enable-vmx --enable-x86-64 --enable-x86-debugger --disable-docbook
	--enable-cpu-level=6 --enable-debugger-gui --with-x --with-x11'

cd $1

# "--enable-smp" doesn't seem to work. Hangs on loading OVMF.
make dist-clean || true
./configure $BXFLAGS --enable-debugger && configure_defines
make
mv bochs "$2/bochs"

make dist-clean || true
./configure $BXFLAGS --enable-gdb-stub && configure_defines
make
mv bochs "$2/bochs_gdb"
