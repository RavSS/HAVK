#!/bin/sh
set -e

if [ "$1" = "-h" -o "$1" = "--help" ]; then
	echo "./ascii_art_to_ada.sh <file containing bitmap font character>"
	echo -e "\nThis script converts 8x8 ASCII art of a character"
	echo -e "to an array index initialization for HAVK in Ada.\n"
	echo "The file must be made up of 'X' (1) and spaces (0)."
	echo "It must also be 8 lines with 8 characters on each line."
	exit 1
fi;

FILE=$1

# Check if the file exists, isn't empty, and is readable. Error otherwise.
if [ ! -s $FILE ]; then
	echo "The file either doesn't exist or is empty."
	exit 1
elif [ ! -r $FILE ]; then
	echo "The file is unreadable by this shell script or your user."
	exit 1
fi;

# File must be 8 characters by 8 characters for a 8x8 font.
FILE_LINES=`wc -l $FILE | awk '{ print $1 }'`
FILE_COLUMNS=`wc -L $FILE | awk '{ print $1 }'`

PIXEL="X"
BLANK=" "
ASCII_DECIMAL_VALUE=`basename -s .ASCII $FILE`

# Now check if the file is to our standard.
if file $FILE | grep CR; then
	echo "The file '$FILE' contains Windows/DOS (CRLF) lines."
	echo "It must only contain Unix (LF) lines."
	exit 1
elif [ $FILE_LINES -ne 8 ]; then
	echo "The file is $FILE_LINES lines long, it must only be 8 lines."
	if [ $FILE_LINES -eq 9 ]; then
		echo "Your file may have a blank line at the end."
	fi;
	exit 1
elif [ $FILE_COLUMNS -ne 8 ]; then
	echo "A line(s) in the file extends beyond 8 characters or below it."
	echo "Remember that each line handles a byte (8 bits)."
	echo "$FILE_COLUMNS characters were detected in a line somewhere."
	exit 1
fi;

cat $FILE |
	# Replace all the places specifying pixels with 1.
	sed "s/$PIXEL/1/g" |
	# Replace all the places that specify nothing with 0.
	sed "s/$BLANK/0/g" |
	# Add the base indicating that it"s a binary number.
	sed "s/^/2#/" |
	# Add the suffix indicating the end of the numeric literal.
	sed "s/$/#/" |
	# Add a comma to each line"s end except the last one.
	sed "$ ! s/$/,/" |
	# Add a comment to each line end (extra space for the last line).
	sed "$ ! s/$/   -- /; $ s/$/    -- /" |
	# Merge the Ada code with the ASCII art.
	paste -d "" - $FILE |
	# Indent every line.
	sed "s/^/   /" |
	# Add the ASCII value (decimal) and first bracket.
	sed "1s/^/$ASCII_DECIMAL_VALUE =>\n(\n/" |
	# Finally, add the last bracket and a comma.
	sed "$ s/$/\n),/"

exit 0
