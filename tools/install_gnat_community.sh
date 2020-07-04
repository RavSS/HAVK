#!/bin/sh

# This shell script downloads and helps sets up GNAT Community or GNAT GPL
# for x86-64, which compiles the main version of HAVK.
set -e

# This points to the source location for GNAT GPL.
if [ "$1" = "windows" ] # Windows Subsystem for Linux support.
then
	echo "Obtaining GNAT for Windows to use with WSL."

	if [ "$(id -u)" -ne 0 ]
	then
		echo "This script must be ran as root for WSL. Try again."
		exit 1
	fi

	GNAT_SRC=https://community.download.adacore.com/v1/
	GNAT_SHA1=966801764ae160828c97d2c33000e9feb08d4cce
	GNAT_FILE=gnat-community.exe
	GNAT_DIR=com/gnat_gpl_windows
elif [ "$1" = "linux" ]
then
	echo "Obtaining GNAT for Linux."
	GNAT_SRC=https://community.download.adacore.com/v1/
	GNAT_SHA1=4d99b7b2f212c8efdab2ba8ede474bb9fa15888d
	GNAT_FILE=gnat-community.bin
	GNAT_DIR=com/gnat_gpl_linux
else
	echo "Usage: $0 <linux/windows>"
	exit 1
fi

GNAT_SRC="$GNAT_SRC""$GNAT_SHA1""?filename=gnat" # Rename it later.
GNAT_CHECKSUM_LINE="$GNAT_SHA1 *./tools/$GNAT_FILE"

CUR_DIR=`basename $PWD`

if [ "$CUR_DIR" = "tools" ]
then
	cd ../
elif [ "$CUR_DIR" != "HAVK" ]
then
	echo "You ran this script from basename '$CUR_DIR' at '$PWD'"
	echo "The basename directory is not 'HAVK' or 'tools'."
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

if [ ! -d "./compilers" ]
then
	mkdir ./compilers
fi

if [ ! -d "./build" ]
then
	mkdir ./build
fi

if ls ./tools/$GNAT_FILE 1> /dev/null 2>&1
then
	echo "GNAT Community installation file ($GNAT_FILE) already exists."
else
	wget $GNAT_SRC -nc -O ./tools/$GNAT_FILE
	chmod +x ./tools/$GNAT_FILE
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
echo "Please install GNAT Community to '$PWD/$GNAT_DIR'."
./tools/$GNAT_FILE