#!/bin/sh
set -e

if [ -z "$1" -o "-h" = "$1" -o "--help" = "$1" ]
then
	echo "Usage: $0 <Bochs source directory>"
	exit 1
fi

cd $1

./configure --enable-debugger --enable-smp --enable-3dnow \
	--enable-x86-64 --enable-x86-debugger --enable-avx \
	--enable-svm --enable-long-phy-address \
	--enable-sb16 --enable-gameport \
	--enable-clgd54xx --enable-pci --enable-usb \
	--enable-idle-hack --enable-all-optimizations \
	--enable-usb-ohci

make -j`nproc --all`

