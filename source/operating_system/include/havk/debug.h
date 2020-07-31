///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System                                  //
// Filename        -- debug.h                                                //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#ifndef HAVK_DEBUG_H
#define HAVK_DEBUG_H

#include "havk.h"

// Requires a buffer going up to at least 65 character elements.
char *uitoa(uintmax_t value, char *buffer);

// Takes in a null-terminated string of up to 256 characters (including the
// null character at the end).
void log_string(const char *text);

#endif
