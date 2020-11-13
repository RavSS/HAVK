///////////////////////////////////////////////////////////////////////////////
// Program         -- HAVK Operating System                                  //
// Filename        -- havk.c                                                 //
// License         -- GNU General Public License version 3.0                 //
// Original Author -- Ravjot Singh Samra, Copyright 2020                     //
///////////////////////////////////////////////////////////////////////////////

#include "havk.h"

extern inline syserr_ht syscall(sysargs_ht *arguments);
extern inline syserr_ht syscall_data(sysargs_ht *arguments, void *data);
extern inline void output_byte(uint16_t port, uint8_t value);
extern inline void output_word(uint16_t port, uint16_t value);
extern inline uint8_t input_byte(uint16_t port);
extern inline uint16_t input_word(uint16_t port);
