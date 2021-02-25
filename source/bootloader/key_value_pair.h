/*****************************************************************************/
/* Program         -- Key-Value Pair Parser                                  */
/* Filename        -- key_value_pair.h                                       */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020-2021                */
/*****************************************************************************/

/* This is a small library I made to parse a string into key-value pair
/  entries. I've barely checked it through, so expect potential errors. It
/  requires no library or even a libc, and it's compatible with C89 and above.
/  Usage is as simple as calling `kvp_count()` to check how many pairs there
/  are in the string (for the purposes of memory allocation) and then calling
/  `kvp_parse()` to fill in the buffer with key-value pair entries. The
/  original pointer to the characters and the characters themselves are not
/  modified at all. */
#ifndef KEY_VALUE_PAIR_H
#define KEY_VALUE_PAIR_H

/* Should be an unsigned type that can hold index values. */
#ifndef KVP_INDEX_TYPE
	#define KVP_INDEX_TYPE unsigned long
#endif

#ifndef KVP_ENTRY_ATTRIBUTES
	#define KVP_ENTRY_ATTRIBUTES
#endif

/* Semi-colon terminated list of fields to add to the entry structure. */
#ifndef KVP_ENTRY_DATA_FIELDS
	#define KVP_ENTRY_DATA_FIELDS
#endif

/* For now, only tokens that can fit inside a byte (char) are supported. */

#ifndef KVP_ASSIGNER
	#define KVP_ASSIGNER '='
#endif

#ifndef KVP_DELIMITER
	#define KVP_DELIMITER '\n'
#endif

#ifndef KVP_LINE_COMMENT
	#define KVP_LINE_COMMENT '#'
#endif

typedef KVP_INDEX_TYPE kvp_uint;
typedef struct kvp_entry
{
	kvp_uint key_start, key_end;
	kvp_uint value_start, value_end;
	KVP_ENTRY_DATA_FIELDS
} KVP_ENTRY_ATTRIBUTES kvp_entry;

/* This can be changed after the header itself is included, if need be. */
static char kvp_delimiter = KVP_DELIMITER;

/* It's just `memset()` for the entry structure. Used so we can clear the
/  custom field as well without strictly understanding it. */
static void kvp_clear_entry(kvp_entry *const entry)
{
	kvp_uint i;
	char *entry_byte = (char *)entry;

	for (i = sizeof(kvp_entry); i--; *entry_byte++ = 0);
}

/* The main parser logic. If the latter two arguments are zero, then it just
/  counts as many pairs as it can without filling in the entries array. */
static kvp_uint kvp_parser(const char *const data, const kvp_uint size,
	kvp_entry *const entries, const kvp_uint max_count)
{
	kvp_uint line_start, line_assigner, line_end, count;

	for (line_start = 0, count = 0; line_start < size;)
	{
		if (data[line_start] == kvp_delimiter) /* Empty line check. */
		{
			++line_start;
			continue;
		}

		for (line_assigner = 0, line_end = line_start;; ++line_end)
		{
			if (line_end >= size)
			{
				/* If there was no assignment, then exit from
				/  here early. */
				if (!line_assigner)
				{
					return count;
				}

				/* No empty newline after the final line in the
				/  file was found, but we still need to parse
				/  the final line itself. */
				--line_end; /* Don't go over the file's end. */
				break;
			}
			else if (data[line_end] == KVP_ASSIGNER)
			{
				/* If two or more assignment tokens appear,
				/  then the last assignment token will be used
				/  and the rest will appear as part of the key
				/  for the pair itself. */
				line_assigner = line_end;
			}
			else if (data[line_end] == kvp_delimiter)
			{
				--line_end; /* Don't include the delimiter. */
				break;
			}
			else if (!data[line_end])
			{
				/* A null byte in the buffer is erroneous. */
				return 0;
			}
		}

		/* Must not be a comment and must have both a key and a value
		/  of some length for it to be considered as a valid pair. */
		if (data[line_start] != KVP_LINE_COMMENT
			&& line_assigner != 0 /* Must have an assignment. */
			&& line_assigner != line_start
			&& line_assigner != line_end)
		{
			if (entries && count < max_count)
			{
				kvp_clear_entry(&entries[count]);
				entries[count].key_start = line_start;
				entries[count].key_end = line_assigner - 1;
				entries[count].value_start = line_assigner + 1;
				entries[count].value_end = line_end;
			}

			++count;
		}

		line_start = line_end + 1;
	}

	return count;
}

/* Counts the number of entries in the key-value-pair file. */
#define kvp_count(x, y) kvp_parser((x), (y), 0, 0)

/* Parses the key-value-pair file and puts pairs into entries. Requires a
/  memory area which can hold up to "a" number of entries. */
#define kvp_parse(x, y, z, a) kvp_parser((x), (y), (z), (a))

#endif
