/*****************************************************************************/
/* Program         -- HAVK Libc                                              */
/* Filename        -- stdio.h                                               */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020-2021                */
/*****************************************************************************/

#ifndef STDIO_H
#define STDIO_H

#include <havk/havk.h>

typedef struct FILE_record
{
	char file_path[128];
	char file_mode[3];
	size_t file_size;
	size_t file_offset;
	syserr_ht file_error;
	bool write_request;
	char buffer[91];
	size_t buffer_length;
} FILE;

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* Standard libc `fopen()` (C89). */
FILE *fopen(const char *file_path, const char *file_mode);

/* Standard libc `fread()` (C89). */
size_t fread(void *data_buffer, size_t element_size, size_t element_count,
	FILE *open_file);

/* Standard libc `fclose()` (C89). */
int fclose(FILE *open_file);

/* Standard libc `fseek()` (C89). */
int fseek(FILE *open_file, long file_offset, int origin);

/* Standard libc `ftell()` (C89). */
int ftell(FILE *open_file);

/* TODO: `rewind()` is not meaningful without also having meaningful error
/  indicators for `fseek()` etc. For now, it's entirely synonymous to it. */
#define rewind(x) ((void) fseek((x), 0, SEEK_SET))

#endif
