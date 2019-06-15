#!/bin/sh

# This shell script downloads and helps sets up GNAT Community or GNAT GPL
# for x86-64, which compiles the main version of HAVK.
set -e

# This points to the source location for GNAT GPL.
GNAT_SRC=http://mirrors.cdn.adacore.com/art/5cdffc5409dcd015aaf82626
GNAT_SHA1=0cd3e2a668332613b522d9612ffa27ef3eb0815b
GNAT_BIN=gnat-community.bin
GNAT_CHECKSUM_LINE="$GNAT_SHA1 *./ext/$GNAT_BIN"
GNAT_DIR=com/gnatgpl

CUR_DIR=`basename $PWD`

if [ "$CUR_DIR" = "ext" ]
then
	cd ../
elif [ "$CUR_DIR" != "HAVK" ]
then
	echo "You ran this script from basename '$CUR_DIR' at '$PWD'"
	echo "The basename directory is not 'HAVK' or 'ext'."
	echo "Run this script from one of those directories."
	echo "Modify the script if you surely want to continue."
	echo "You can also rename your current directory to the default names."
	exit 1
fi

if [ -d "./$GNAT_DIR" ]
then
	echo "GNAT Community already detected in './$GNAT_DIR', exiting."
	exit 1
fi

if [ ! -d "./com" ]
then
	mkdir ./com
fi

if [ ! -d "./build" ]
then
	mkdir ./build
fi

if ls ./ext/$GNAT_BIN 1> /dev/null 2>&1
then
	echo "GNAT Community installation file ($GNAT_BIN) already exists."
else
	wget $GNAT_SRC -nc -O ./ext/$GNAT_BIN
	chmod +x ./ext/$GNAT_BIN
fi

if echo $GNAT_CHECKSUM_LINE | sha1sum -c -
then
	echo "SHA1 sum is correct."
else
	echo "SHA1 sum is incorrect."
	echo "Check the hardcoded sum in the script or update it."
	exit 1
fi

# TODO: Integrate AdaCore's installation scripts into this file so the user
# doesn't need to use the graphical interface.
echo "Please install GNAT Community to '$PWD/$GNAT_DIR'. GPS IDE is optional."
./ext/$GNAT_BIN
