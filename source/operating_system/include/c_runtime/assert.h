/*****************************************************************************/
/* Program         -- HAVK Libc                                              */
/* Filename        -- assert.h                                               */
/* License         -- GNU General Public License version 3.0                 */
/* Original Author -- Ravjot Singh Samra, Copyright 2020                     */
/*****************************************************************************/

#ifndef ASSERT_H
#define ASSERT_H

#include <havk/debug.h>

/* A non-standard assertion macro implementation. I'm reusing the last chance
/  handler intended for Ada code. */
#if defined(NDEBUG) || !defined(DEBUG)
	#define assert(x)
#else
	#define assert(x) \
	MACRO_BEGIN\
		if (!(x))\
		{\
			__gnat_last_chance_handler(__FUNCTION__, __LINE__);\
		}\
	MACRO_END
#endif

#endif
