/*****************************************************************************/
/* Program         -- Key-Value Pair Parser                                  */
/* Filename        -- key_value_pair.h                                       */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020                     */
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

#ifdef KVP_NO_STDDEF
	typedef unsigned long kvp_uint;
#else
	#include <stddef.h> /* Only "size_t" is used. Conforms to ANSI C. */
	typedef size_t kvp_uint;
#endif

#ifndef KVP_ENTRY_ATTRIBUTES
	#define KVP_ENTRY_ATTRIBUTES
#endif

/* Semi-colon terminated list of fields to add to the entry structure. */
#ifndef KVP_ENTRY_DATA_FIELDS
	#define KVP_ENTRY_DATA_FIELDS
#endif

#ifndef KVP_ASSIGNER
	#define KVP_ASSIGNER '='
#endif

#ifndef KVP_DELIMITER
	#define KVP_DELIMITER '\n'
#endif

#ifndef KVP_LINE_COMMENT
	#define KVP_LINE_COMMENT '#'
#endif

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

/* Returns the index of the next line, with the return value being +1 after the
/  delimiter itself. Returns zero if no more lines could be interpreted. */
static kvp_uint kvp_next_line(const char *const data, kvp_uint start,
	const kvp_uint size)
{
	while (start++ < size && data[start] != kvp_delimiter);
	return ++start < size && data[start] ? start : 0;
}

/* Returns one on success and zero on failure, depending on if the line could
/  be parsed properly. Also outputs the starting index for the value string.
/  Expects the data pointer to point towards the start of the line, unlike
/  other functions here. That also means the value string's start will be
/  relative to the start of the data pointer. */
static char kvp_read_line(const char *const data,
	kvp_uint *const value_start_offset)
{
	kvp_uint i;
	char key_set, value_set;

	for (i = 0, key_set = 0, value_set = 0; data[i] != kvp_delimiter; ++i)
	{
		if (data[i] == KVP_ASSIGNER)
		{
			if (!key_set)
			{
				key_set = 1;
			}
			else
			{
				return 0; /* Double usage of the assigner. */
			}
		}
		else if (key_set && !value_set)
		{
			value_set = 1;
			*value_start_offset = i;
		}
	}

	return key_set && value_set;
}

static kvp_uint kvp_count(const char *const data, const kvp_uint size)
{
	kvp_uint i, count, next_line, ignored;

	for (i = 0, count = 0; i < size; i = next_line)
	{
		next_line = kvp_next_line(data, i, size);

		if (data[i] == KVP_LINE_COMMENT)
		{
			if (!next_line)
			{
				break;
			}

			continue;
		}

		/* Ignore the value index. */
		count += kvp_read_line(&data[i], &ignored);

		if (!next_line)
		{
			break;
		}
	}

	return count;
}

static void kvp_parse(const char *const data, const kvp_uint size,
	kvp_entry *const entries, const kvp_uint pair_count)
{
	kvp_uint i, pair, next_line, value_start_offset;

	for (i = 0, pair = 0; i < size && pair < pair_count; i = next_line)
	{
		next_line = kvp_next_line(data, i, size);

		if (data[i] == KVP_LINE_COMMENT)
		{
			if (!next_line)
			{
				break;
			}

			continue;
		}

		value_start_offset = 0; /* Silence the unused warning. */
		if (kvp_read_line(&data[i], &value_start_offset))
		{
			kvp_clear_entry(&entries[pair]);
			value_start_offset += i; // Relative to absolute.

			entries[pair].key_start = i;
			entries[pair].key_end = value_start_offset - 2;
			entries[pair].value_start = value_start_offset;
			while (data[++value_start_offset] != kvp_delimiter);
			entries[pair++].value_end = value_start_offset - 1;
		}

		if (!next_line)
		{
			break;
		}
	}
}

#endif
