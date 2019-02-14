#ifndef PAGE_H
#define PAGE_H

#include <havk.h>

/* TODO: Paging is enabled, but it's not set up properly. */

/* Flushes the whole TLB. Slow. */
void tlb_flush_all(void);

#endif