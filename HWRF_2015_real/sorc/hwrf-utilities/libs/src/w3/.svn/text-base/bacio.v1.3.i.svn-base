/* Fortran-callable routines to read and write characther (bacio) and */ 
/*   numeric (banio) data byte addressably                            */ 
/* Robert Grumbine  16 March 1998 */ 
/*  v1.1: Put diagnostic output under control of define VERBOSE or QUIET */ 
/*        Add option of non-seeking read/write                           */ 
/*        Return code for fewer data read/written than requested */ 
/*  v1.2: Add cray compatibility  20 April 1998                  */ 
/* Define ISO C stdio on top of C++ iostreams.
   Copyright (C) 1991, 1994-2007, 2008 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.19 Input/output	<stdio.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* These are defined by the user (or the compiler)
   to specify the desired environment:

   __STRICT_ANSI__	ISO Standard C.
   _ISOC99_SOURCE	Extensions to ISO C89 from ISO C99.
   _POSIX_SOURCE	IEEE Std 1003.1.
   _POSIX_C_SOURCE	If ==1, like _POSIX_SOURCE; if >=2 add IEEE Std 1003.2;
			if >=199309L, add IEEE Std 1003.1b-1993;
			if >=199506L, add IEEE Std 1003.1c-1995;
			if >=200112L, all of IEEE 1003.1-2004
   _XOPEN_SOURCE	Includes POSIX and XPG things.  Set to 500 if
			Single Unix conformance is wanted, to 600 for the
			upcoming sixth revision.
   _XOPEN_SOURCE_EXTENDED XPG things and X/Open Unix extensions.
   _LARGEFILE_SOURCE	Some more functions for correct standard I/O.
   _LARGEFILE64_SOURCE	Additional functionality from LFS for large files.
   _FILE_OFFSET_BITS=N	Select default filesystem interface.
   _BSD_SOURCE		ISO C, POSIX, and 4.3BSD things.
   _SVID_SOURCE		ISO C, POSIX, and SVID things.
   _ATFILE_SOURCE	Additional *at interfaces.
   _GNU_SOURCE		All of the above, plus GNU extensions.
   _REENTRANT		Select additionally reentrant object.
   _THREAD_SAFE		Same as _REENTRANT, often used by other systems.
   _FORTIFY_SOURCE	If set to numeric value > 0 additional security
			measures are defined, according to level.

   The `-ansi' switch to the GNU C compiler defines __STRICT_ANSI__.
   If none of these are defined, the default is to have _SVID_SOURCE,
   _BSD_SOURCE, and _POSIX_SOURCE set to one and _POSIX_C_SOURCE set to
   200112L.  If more than one of these are defined, they accumulate.
   For example __STRICT_ANSI__, _POSIX_SOURCE and _POSIX_C_SOURCE
   together give you ISO C, 1003.1, and 1003.2, but nothing else.

   These are defined by this file and are used by the
   header files to decide what to declare or define:

   __USE_ISOC99		Define ISO C99 things.
   __USE_ISOC95		Define ISO C90 AMD1 (C95) things.
   __USE_POSIX		Define IEEE Std 1003.1 things.
   __USE_POSIX2		Define IEEE Std 1003.2 things.
   __USE_POSIX199309	Define IEEE Std 1003.1, and .1b things.
   __USE_POSIX199506	Define IEEE Std 1003.1, .1b, .1c and .1i things.
   __USE_XOPEN		Define XPG things.
   __USE_XOPEN_EXTENDED	Define X/Open Unix things.
   __USE_UNIX98		Define Single Unix V2 things.
   __USE_XOPEN2K        Define XPG6 things.
   __USE_LARGEFILE	Define correct standard I/O things.
   __USE_LARGEFILE64	Define LFS things with separate names.
   __USE_FILE_OFFSET64	Define 64bit interface as default.
   __USE_BSD		Define 4.3BSD things.
   __USE_SVID		Define SVID things.
   __USE_MISC		Define things common to BSD and System V Unix.
   __USE_ATFILE		Define *at interfaces and AT_* constants for them.
   __USE_GNU		Define GNU extensions.
   __USE_REENTRANT	Define reentrant/thread-safe *_r functions.
   __USE_FORTIFY_LEVEL	Additional security measures used, according to level.
   __FAVOR_BSD		Favor 4.3BSD things in cases of conflict.

   The macros `__GNU_LIBRARY__', `__GLIBC__', and `__GLIBC_MINOR__' are
   defined by this file unconditionally.  `__GNU_LIBRARY__' is provided
   only for compatibility.  All new code should use the other symbols
   to test for features.

   All macros listed above as possibly being defined by this file are
   explicitly undefined if they are not explicitly defined.
   Feature-test macros that are not defined by the user or compiler
   but are implied by the other feature-test macros defined (or by the
   lack of any definitions) are defined by the file.  */ 
/* Undefine everything, so we get a clean slate.  */ 
/* Suppress kernel-name space pollution unless user expressedly asks
   for it.  */ 
/* Always use ISO C things.  */ 
/* Convenience macros to test the versions of glibc and gcc.
   Use them like this:
   #if __GNUC_PREREQ (2,8)
   ... code requiring gcc 2.8 or later ...
   #endif
   Note - they won't work for gcc1 or glibc1, since the _MINOR macros
   were not defined then.  */ 
/* If _BSD_SOURCE was defined by the user, favor BSD over POSIX.  */ 
/* If _GNU_SOURCE was defined by the user, turn on all the other features.  */ 
/* If nothing (other than _GNU_SOURCE) is defined,
   define _BSD_SOURCE and _SVID_SOURCE.  */ 
/* This is to enable the ISO C99 extension.  Also recognize the old macro
   which was used prior to the standard acceptance.  This macro will
   eventually go away and the features enabled by default once the ISO C99
   standard is widely adopted.  */ 
/* This is to enable the ISO C90 Amendment 1:1995 extension.  */ 
/* If none of the ANSI/POSIX macros are defined, use POSIX.1 and POSIX.2
   (and IEEE Std 1003.1b-1993 unless _XOPEN_SOURCE is defined).  */ 
/* We do support the IEC 559 math functionality, real and complex.  */ 
/* wchar_t uses ISO 10646-1 (2nd ed., published 2000-09-15) / Unicode 3.1.  */ 
/* This macro indicates that the installed library is the GNU C Library.
   For historic reasons the value now is 6 and this will stay from now
   on.  The use of this variable is deprecated.  Use __GLIBC__ and
   __GLIBC_MINOR__ now (see below) when you want to test for a specific
   GNU C library version and use the values in <gnu/lib-names.h> to get
   the sonames of the shared libraries.  */ 
/* Major and minor version number of the GNU C library package.  Use
   these macros to test for features in specific releases.  */ 
/* Decide whether a compiler supports the long long datatypes.  */ 
/* This is here only because every header file already includes this one.  */ 
/*
 *
 *      Copyright 1995-2000, The Portland Group, Incorporated.
 *      Copyright 2000-2005, STMicroelectronics, Incorporated.
 *      All rights reserved.
 *
 *        THE PORTLAND GROUP, INCORPORATED PROPRIETARY INFORMATION
 * This software is supplied under the terms of a license agreement
 * or nondisclosure agreement with The Portland Group and may not be
 * copied or disclosed except in accordance with the terms of that
 * agreement.
 */ 
/* Copyright (C) 1992-2001, 2002, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* We are almost always included from features.h. */ 
/* The GNU libc does not support any K&R compilers or the traditional mode
   of ISO C compilers anymore.  Check for some of the combinations not
   anymore supported.  */ 
/* Some user header file might have defined this before.  */ 
/* These two macros are not used in glibc anymore.  They are kept here
   only because some other projects expect the macros to be defined.  */ 
/* For these things, GCC behaves the ANSI way normally,
   and the non-ANSI way under -traditional.  */ 
/* This is not a typedef so `const __ptr_t' does the right thing.  */ 
/* C++ needs to know that types and declarations are C, not C++.  */ 
/* The standard library needs the functions from the ISO C90 standard
   in the std namespace.  At the same time we want to be safe for
   future changes and we include the ISO C99 code in the non-standard
   namespace __c99.  The C++ wrapper header take case of adding the
   definitions to the global namespace.  */ 
/* For compatibility we do not add the declarations into any
   namespace.  They will end up in the global namespace which is what
   old code expects.  */ 
/* Support for bounded pointers.  */ 
/* Fortify support.  */ 
/* Support for flexible arrays.  */ 
/* __asm__ ("xyz") is used throughout the headers to rename functions
   at the assembly language level.  This is wrapped by the __REDIRECT
   macro, in order to support compilers that can do this some other
   way.  When compilers don't support asm-names at all, we have to do
   preprocessor tricks instead (which don't have exactly the right
   semantics, but it's the best we can do).

   Example:
   int __REDIRECT(setpgrp, (__pid_t pid, __pid_t pgrp), setpgid); */ 
/* GCC has various useful declarations that can be made with the
   `__attribute__' syntax.  All of the ways we use this do fine if
   they are omitted for compilers that don't understand it. */ 
/* At some point during the gcc 2.96 development the `malloc' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */ 
/* At some point during the gcc 2.96 development the `pure' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */ 
/* At some point during the gcc 3.1 development the `used' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.  */ 
/* gcc allows marking deprecated functions.  */ 
/* At some point during the gcc 2.8 development the `format_arg' attribute
   for functions was introduced.  We don't want to use it unconditionally
   (although this would be possible) since it generates warnings.
   If several `format_arg' attributes are given for the same function, in
   gcc-3.0 and older, all but the last one are ignored.  In newer gccs,
   all designated arguments are considered.  */ 
/* At some point during the gcc 2.97 development the `strfmon' format
   attribute for functions was introduced.  We don't want to use it
   unconditionally (although this would be possible) since it
   generates warnings.  */ 
/* The nonull function attribute allows to mark pointer parameters which
   must not be NULL.  */ 
/* If fortification mode, we warn about unused results of certain
   function calls which can lead to problems.  */ 
/* Forces a function to be always inlined.  */ 
/* GCC 4.3 and above with -std=c99 or -std=gnu99 implements ISO C99
   inline semantics, unless -fgnu89-inline is used.  */ 
/* GCC 4.3 and above allow passing all anonymous arguments of an
   __extern_always_inline function to some other vararg function.  */ 
/* It is possible to compile containing GCC extensions even if GCC is
   run in pedantic mode if the uses are carefully marked using the
   `__extension__' keyword.  But this is not generally available before
   version 2.8.  */ 
/* __restrict is known in EGCS 1.2 and above. */ 
/* ISO C99 also allows to declare arrays as non-overlapping.  The syntax is
     array_name[restrict]
   GCC 3.1 supports this.  */ 
/* Determine the wordsize from the preprocessor defines.  */ 
/* for PGI compiler make sure the __attribute__ keyword has not been defined as:
 * #define __attribute__(xyz)
 */ 
/* implement __restrict */ 
/* If we don't have __REDIRECT, prototypes will be missing if
   __USE_FILE_OFFSET64 but not __USE_LARGEFILE[64]. */ 
/* Decide whether we can define 'extern inline' functions in headers.  */ 
/* This is here only because every header file already includes this one.
   Get the definitions of all the appropriate `__stub_FUNCTION' symbols.
   <gnu/stubs.h> contains `#define __stub_FUNCTION' when FUNCTION is a stub
   that will always return failure (and set errno to ENOSYS).  */ 
/* This file selects the right generated file of `__stub_FUNCTION' macros
   based on the architecture being compiled for.  */ 
/* Determine the wordsize from the preprocessor defines.  */ 
/* This file is automatically generated.
   It defines a symbol `__stub_FUNCTION' for each function
   in the C library which is a stub, meaning it will fail
   every time called, usually setting errno to ENOSYS.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
typedef unsigned long int size_t ;
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Determine the wordsize from the preprocessor defines.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Convenience types.  */ 
typedef unsigned char __u_char ;
typedef unsigned short int __u_short ;
typedef unsigned int __u_int ;
typedef unsigned long int __u_long ;
/* Fixed-size types, underlying types depend on word size and compiler.  */ 
typedef signed char __int8_t ;
typedef unsigned char __uint8_t ;
typedef signed short int __int16_t ;
typedef unsigned short int __uint16_t ;
typedef signed int __int32_t ;
typedef unsigned int __uint32_t ;
typedef signed long int __int64_t ;
typedef unsigned long int __uint64_t ;
/* #elif defined __GLIBC_HAVE_LONG_LONG   */ 
/* quad_t is also 64 bits.  */ 
typedef long int __quad_t ;
typedef unsigned long int __u_quad_t ;
/* #elif defined __GLIBC_HAVE_LONG_LONG */ 
/* The machine-dependent file <bits/typesizes.h> defines __*_T_TYPE
   macros for each of the OS types we define below.  The definitions
   of those macros must use the following macros for underlying types.
   We define __S<SIZE>_TYPE and __U<SIZE>_TYPE for the signed and unsigned
   variants of each of the following integer types on this machine.

	16		-- "natural" 16-bit type (always short)
	32		-- "natural" 32-bit type (always int)
	64		-- "natural" 64-bit type (long or long long)
	LONG32		-- 32-bit type, traditionally long
	QUAD		-- 64-bit type, always long long
	WORD		-- natural type of __WORDSIZE bits (int or long)
	LONGWORD	-- type of __WORDSIZE bits, traditionally long

   We distinguish WORD/LONGWORD, 32/LONG32, and 64/QUAD so that the
   conventional uses of `long' or `long long' type modifiers match the
   types we define, even when a less-adorned type would be the same size.
   This matters for (somewhat) portably writing printf/scanf formats for
   these types, where using the appropriate l or ll format modifiers can
   make the typedefs and the formats match up across all GNU platforms.  If
   we used `long' when it's 64 bits where `long long' is expected, then the
   compiler would warn about the formats not matching the argument types,
   and the programmer changing them to shut up the compiler would break the
   program's portability.

   Here we assume what is presently the case in all the GCC configurations
   we support: long long is always 64 bits, long is always word/address size,
   and int is always 32 bits.  */ 
/* No need to mark the typedef with __extension__.   */ 
/* bits/typesizes.h -- underlying types for *_t.  Generic version.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* See <bits/types.h> for the meaning of these macros.  This file exists so
   that <bits/types.h> need not vary across different GNU platforms.  */ 
/* Number of descriptors that can fit in an `fd_set'.  */ 
typedef unsigned long int __dev_t ;	/* Type of device numbers.  */ 
typedef unsigned int __uid_t ;	/* Type of user identifications.  */ 
typedef unsigned int __gid_t ;	/* Type of group identifications.  */ 
typedef unsigned long int __ino_t ;	/* Type of file serial numbers.  */ 
typedef unsigned long int __ino64_t ;	/* Type of file serial numbers (LFS).*/ 
typedef unsigned int __mode_t ;	/* Type of file attribute bitmasks.  */ 
typedef unsigned long int __nlink_t ;	/* Type of file link counts.  */ 
typedef long int __off_t ;	/* Type of file sizes and offsets.  */ 
typedef long int __off64_t ;	/* Type of file sizes and offsets (LFS).  */ 
typedef int __pid_t ;	/* Type of process identifications.  */ 
typedef struct { int __val [ 2 ] ; } __fsid_t ;	/* Type of file system IDs.  */ 
typedef long int __clock_t ;	/* Type of CPU usage counts.  */ 
typedef unsigned long int __rlim_t ;	/* Type for resource measurement.  */ 
typedef unsigned long int __rlim64_t ;	/* Type for resource measurement (LFS).  */ 
typedef unsigned int __id_t ;	/* General type for IDs.  */ 
typedef long int __time_t ;	/* Seconds since the Epoch.  */ 
typedef unsigned int __useconds_t ; /* Count of microseconds.  */ 
typedef long int __suseconds_t ; /* Signed count of microseconds.  */ 
typedef int __daddr_t ;	/* The type of a disk address.  */ 
typedef long int __swblk_t ;	/* Type of a swap block maybe?  */ 
typedef int __key_t ;	/* Type of an IPC key.  */ 
/* Clock ID used in clock and timer functions.  */ 
typedef int __clockid_t ;
/* Timer ID returned by `timer_create'.  */ 
typedef void * __timer_t ;
/* Type to represent block size.  */ 
typedef long int __blksize_t ;
/* Types from the Large File Support interface.  */ 
/* Type to count number of disk blocks.  */ 
typedef long int __blkcnt_t ;
typedef long int __blkcnt64_t ;
/* Type to count file system blocks.  */ 
typedef unsigned long int __fsblkcnt_t ;
typedef unsigned long int __fsblkcnt64_t ;
/* Type to count file system nodes.  */ 
typedef unsigned long int __fsfilcnt_t ;
typedef unsigned long int __fsfilcnt64_t ;
typedef long int __ssize_t ; /* Type of a byte count, or error.  */ 
/* These few don't really vary by system, they always correspond
   to one of the other defined types.  */ 
typedef __off64_t __loff_t ;	/* Type of file sizes and offsets (LFS).  */ 
typedef __quad_t * __qaddr_t ;
typedef char * __caddr_t ;
/* Duplicates info from stdint.h but this is used in unistd.h.  */ 
typedef long int __intptr_t ;
/* Duplicate info from sys/socket.h.  */ 
typedef unsigned int __socklen_t ;
/* Define outside of namespace so the C++ is happy.  */ 
struct _IO_FILE ;
/* The opaque type of streams.  This is the definition used elsewhere.  */ 
typedef struct _IO_FILE FILE ;
/* The opaque type of streams.  This is the definition used elsewhere.  */ 
typedef struct _IO_FILE __FILE ;
/* Copyright (C) 1991-1995,1997-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Written by Per Bothner <bothner@cygnus.com>.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.

   As a special exception, if you link the code in this file with
   files compiled with a GNU compiler to produce an executable,
   that does not cause the resulting executable to be covered by
   the GNU Lesser General Public License.  This exception does not
   however invalidate any other reasons why the executable file
   might be covered by the GNU Lesser General Public License.
   This exception applies to code released by its copyright holders
   in files containing the exception.  */ 
/* This file is needed by libio to define various configuration parameters.
   These are always the same in the GNU C library.  */ 
/* Define types for libio in terms of the standard internal type names.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Copyright (C) 1995-2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *      ISO C99 Standard: 7.24
 *	Extended multibyte and wide character utilities	<wchar.h>
 */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
typedef unsigned int wint_t ;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* We try to get wint_t from <stddef.h>, but not all GCC versions define it
   there.  So define it ourselves if it remains undefined.  */ 
/* Work around problems with the <stddef.h> file which doesn't put
   wint_t in the std namespace.  */ 
/* Conversion state information.  */ 
typedef struct
{
  int __count ;
  union
  {
    unsigned int __wch ;
    char __wchb [ 4 ] ;
  } __value ;	/* Value so far.  */ 
} __mbstate_t ;
/* The rest of the file is only used if used if __need_mbstate_t is not
   defined.  */ 
/* Undefined all __need_* constants in case we are included to get those
   constants but the whole file was already read.  */ 
typedef struct
{
  __off_t __pos ;
  __mbstate_t __state ;
} _G_fpos_t ;
typedef struct
{
  __off64_t __pos ;
  __mbstate_t __state ;
} _G_fpos64_t ;
typedef int _G_int16_t __attribute__ ( ( __mode__ ( __HI__ ) ) ) ;
typedef int _G_int32_t __attribute__ ( ( __mode__ ( __SI__ ) ) ) ;
typedef unsigned int _G_uint16_t __attribute__ ( ( __mode__ ( __HI__ ) ) ) ;
typedef unsigned int _G_uint32_t __attribute__ ( ( __mode__ ( __SI__ ) ) ) ;
/* These library features are always available in the GNU C library.  */ 
/* This is defined by <bits/stat.h> if `st_blksize' exists.  */ 
/* These are the vtbl details for ELF.  */ 
/* ALL of these should be defined in _G_config.h */ 
/* This define avoids name pollution if we're using GNU stdarg.h */ 
/* 
* stdarg.h
*
*      Copyright 1990-2000, The Portland Group, Incorporated.
*      Copyright 2000-2002, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* 
* va_list.h
*
*      Copyright 1990-2000, The Portland Group, Incorporated.
*      Copyright 2000-2007, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* for osx86-64, also defined in _types.h */ 
typedef struct __pgi_tag {
    unsigned int gp_offset ;
    unsigned int fp_offset ;
    char * overflow_arg_area ;	/* Address of memory args area. */ 
    char * reg_save_area ;	/* Address of where we stored the regs. */ 
} __pgi_va_list [ 1 ] ;
typedef __pgi_va_list va_list ;
typedef __pgi_va_list __gnuc_va_list ;
extern void * __builtin_va_arg ( ) ;
/* For backward compatibility */ 
/* Magic numbers and bits for the _flags field.
   The magic numbers use the high-order bits of _flags;
   the remaining bits are available for variable flags.
   Note: The magic numbers must all be negative if stdio
   emulation is desired. */ 
/* These are "formatting flags" matching the iostream fmtflags enum values. */ 
struct _IO_jump_t ; struct _IO_FILE ;
/* Handle lock.  */ 
typedef void _IO_lock_t ;
/* A streammarker remembers a position in a buffer. */ 
struct _IO_marker {
  struct _IO_marker * _next ;
  struct _IO_FILE * _sbuf ;
  /* If _pos >= 0
 it points to _buf->Gbase()+_pos. FIXME comment */ 
  /* if _pos < 0, it points to _buf->eBptr()+_pos. FIXME comment */ 
  int _pos ;
} ;
/* This is the structure from the libstdc++ codecvt class.  */ 
enum __codecvt_result
{
  __codecvt_ok ,
  __codecvt_partial ,
  __codecvt_error ,
  __codecvt_noconv
} ;
struct _IO_FILE {
  int _flags ;	/* High-order word is _IO_MAGIC; rest is flags. */ 
  /* The following pointers correspond to the C++ streambuf protocol. */ 
  /* Note:  Tk uses the _IO_read_ptr and _IO_read_end fields directly. */ 
  char * _IO_read_ptr ;	/* Current read pointer */ 
  char * _IO_read_end ;	/* End of get area. */ 
  char * _IO_read_base ;	/* Start of putback+get area. */ 
  char * _IO_write_base ;	/* Start of put area. */ 
  char * _IO_write_ptr ;	/* Current put pointer. */ 
  char * _IO_write_end ;	/* End of put area. */ 
  char * _IO_buf_base ;	/* Start of reserve area. */ 
  char * _IO_buf_end ;	/* End of reserve area. */ 
  /* The following fields are used to support backing up and undo. */ 
  char * _IO_save_base ; /* Pointer to start of non-current get area. */ 
  char * _IO_backup_base ; /* Pointer to first valid character of backup area */ 
  char * _IO_save_end ; /* Pointer to end of non-current get area. */ 
  struct _IO_marker * _markers ;
  struct _IO_FILE * _chain ;
  int _fileno ;
  int _flags2 ;
  __off_t _old_offset ; /* This used to be _offset but it's too small.  */ 
  /* 1+column number of pbase(); 0 is unknown. */ 
  unsigned short _cur_column ;
  signed char _vtable_offset ;
  char _shortbuf [ 1 ] ;
  /*  char* _save_gptr;  char* _save_egptr; */ 
  _IO_lock_t * _lock ;
  __off64_t _offset ;
  void * __pad1 ;
  void * __pad2 ;
  void * __pad3 ;
  void * __pad4 ;
  size_t __pad5 ;
  int _mode ;
  /* Make sure we don't get into trouble again.  */ 
  char _unused2 [ 15 * sizeof ( int ) - 4 * sizeof ( void * ) - sizeof ( size_t ) ] ;
} ;
typedef struct _IO_FILE _IO_FILE ;
struct _IO_FILE_plus ;
extern struct _IO_FILE_plus _IO_2_1_stdin_ ;
extern struct _IO_FILE_plus _IO_2_1_stdout_ ;
extern struct _IO_FILE_plus _IO_2_1_stderr_ ;
/* Functions to do I/O and file management for a stream.  */ 
/* Read NBYTES bytes from COOKIE into a buffer pointed to by BUF.
   Return number of bytes read.  */ 
typedef __ssize_t __io_read_fn ( void * __cookie , char * __buf , size_t __nbytes ) ;
/* Write N bytes pointed to by BUF to COOKIE.  Write all N bytes
   unless there is an error.  Return number of bytes written, or -1 if
   there is an error without writing anything.  If the file has been
   opened for append (__mode.__append set), then set the file pointer
   to the end of the file and then do the write; if not, just write at
   the current file pointer.  */ 
typedef __ssize_t __io_write_fn ( void * __cookie , const char * __buf ,
				 size_t __n ) ;
/* Move COOKIE's file position to *POS bytes from the
   beginning of the file (if W is SEEK_SET),
   the current position (if W is SEEK_CUR),
   or the end of the file (if W is SEEK_END).
   Set *POS to the new file position.
   Returns zero if successful, nonzero if not.  */ 
typedef int __io_seek_fn ( void * __cookie , __off64_t * __pos , int __w ) ;
/* Close COOKIE.  */ 
typedef int __io_close_fn ( void * __cookie ) ;
extern int __underflow ( _IO_FILE * ) ;
extern int __uflow ( _IO_FILE * ) ;
extern int __overflow ( _IO_FILE * , int ) ;
extern int _IO_getc ( _IO_FILE * __fp ) ;
extern int _IO_putc ( int __c , _IO_FILE * __fp ) ;
extern int _IO_feof ( _IO_FILE * __fp ) ;
extern int _IO_ferror ( _IO_FILE * __fp ) ;
extern int _IO_peekc_locked ( _IO_FILE * __fp ) ;
/* This one is for Emacs. */ 
extern void _IO_flockfile ( _IO_FILE * ) ;
extern void _IO_funlockfile ( _IO_FILE * ) ;
extern int _IO_ftrylockfile ( _IO_FILE * ) ;
extern int _IO_vfscanf ( _IO_FILE * __restrict , const char * __restrict ,
			__gnuc_va_list , int * __restrict ) ;
extern int _IO_vfprintf ( _IO_FILE * __restrict , const char * __restrict ,
			 __gnuc_va_list ) ;
extern __ssize_t _IO_padn ( _IO_FILE * , int , __ssize_t ) ;
extern size_t _IO_sgetn ( _IO_FILE * , void * , size_t ) ;
extern __off64_t _IO_seekoff ( _IO_FILE * , __off64_t , int , int ) ;
extern __off64_t _IO_seekpos ( _IO_FILE * , __off64_t , int ) ;
extern void _IO_free_backup_area ( _IO_FILE * ) ;
/* The type of the second argument to `fgetpos' and `fsetpos'.  */ 
typedef _G_fpos_t fpos_t ;
/* The possibilities for the third argument to `setvbuf'.  */ 
/* Default buffer size.  */ 
/* End of file character.
   Some things throughout the library rely on this being -1.  */ 
/* The possibilities for the third argument to `fseek'.
   These values should not be changed.  */ 
/* Default path prefix for `tempnam' and `tmpnam'.  */ 
/* Get the values:
   L_tmpnam	How long an array of chars must be to be passed to `tmpnam'.
   TMP_MAX	The minimum number of unique filenames generated by tmpnam
   		(and tempnam when it uses tmpnam's name space),
		or tempnam (the two are separate).
   L_ctermid	How long an array to pass to `ctermid'.
   L_cuserid	How long an array to pass to `cuserid'.
   FOPEN_MAX	Minimum number of files that can be open at once.
   FILENAME_MAX	Maximum length of a filename.  */ 
/* Copyright (C) 1994, 1997, 1998, 1999 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Standard streams.  */ 
extern struct _IO_FILE * stdin ;	/* Standard input stream.  */ 
extern struct _IO_FILE * stdout ;	/* Standard output stream.  */ 
extern struct _IO_FILE * stderr ;	/* Standard error output stream.  */ 
/* C89/C99 say they're macros.  Make them happy.  */ 
/* Remove file FILENAME.  */ 
extern int remove ( const char * __filename ) ;
/* Rename file OLD to NEW.  */ 
extern int rename ( const char * __old , const char * __new ) ;
/* Create a temporary file and open it read/write.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern FILE * tmpfile ( void ) ;
/* Generate a temporary filename.  */ 
extern char * tmpnam ( char * __s ) ;
/* This is the reentrant variant of `tmpnam'.  The only difference is
   that it does not allow S to be NULL.  */ 
extern char * tmpnam_r ( char * __s ) ;
/* Generate a unique temporary filename using up to five characters of PFX
   if it is not NULL.  The directory to put this file in is searched for
   as follows: First the environment variable "TMPDIR" is checked.
   If it contains the name of a writable directory, that directory is used.
   If not and if DIR is not NULL, that value is checked.  If that fails,
   P_tmpdir is tried and finally "/tmp".  The storage for the filename
   is allocated by `malloc'.  */ 
extern char * tempnam ( const char * __dir , const char * __pfx )
       ;
/* Close STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fclose ( FILE * __stream ) ;
/* Flush STREAM, or all streams if STREAM is NULL.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fflush ( FILE * __stream ) ;
/* Faster versions when locking is not required.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */ 
extern int fflush_unlocked ( FILE * __stream ) ;
/* Open a file and create a new stream for it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern FILE * fopen ( const char * __restrict __filename ,
		    const char * __restrict __modes ) ;
/* Open a file, replacing an existing stream with it.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern FILE * freopen ( const char * __restrict __filename ,
		      const char * __restrict __modes ,
		      FILE * __restrict __stream ) ;
/* Create a new stream that refers to an existing system file descriptor.  */ 
extern FILE * fdopen ( int __fd , const char * __modes ) ;
/* If BUF is NULL, make STREAM unbuffered.
   Else make it use buffer BUF, of size BUFSIZ.  */ 
extern void setbuf ( FILE * __restrict __stream , char * __restrict __buf ) ;
/* Make STREAM use buffering mode MODE.
   If BUF is not NULL, use N bytes of it for buffering;
   else allocate an internal buffer N bytes long.  */ 
extern int setvbuf ( FILE * __restrict __stream , char * __restrict __buf ,
		    int __modes , size_t __n ) ;
/* If BUF is NULL, make STREAM unbuffered.
   Else make it use SIZE bytes of BUF for buffering.  */ 
extern void setbuffer ( FILE * __restrict __stream , char * __restrict __buf ,
		       size_t __size ) ;
/* Make STREAM line-buffered.  */ 
extern void setlinebuf ( FILE * __stream ) ;
/* Write formatted output to STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fprintf ( FILE * __restrict __stream ,
		    const char * __restrict __format , ... ) ;
/* Write formatted output to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int printf ( const char * __restrict __format , ... ) ;
/* Write formatted output to S.  */ 
extern int sprintf ( char * __restrict __s ,
		    const char * __restrict __format , ... ) ;
/* Write formatted output to S from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int vfprintf ( FILE * __restrict __s , const char * __restrict __format ,
		     __gnuc_va_list __arg ) ;
/* Write formatted output to stdout from argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int vprintf ( const char * __restrict __format , __gnuc_va_list __arg ) ;
/* Write formatted output to S from argument list ARG.  */ 
extern int vsprintf ( char * __restrict __s , const char * __restrict __format ,
		     __gnuc_va_list __arg ) ;
/* Maximum chars of output to write in MAXLEN.  */ 
extern int snprintf ( char * __restrict __s , size_t __maxlen ,
		     const char * __restrict __format , ... )
      __attribute__ ( ( __format__ ( __printf__ , 3 , 4 ) ) ) ;
extern int vsnprintf ( char * __restrict __s , size_t __maxlen ,
		      const char * __restrict __format , __gnuc_va_list __arg )
      __attribute__ ( ( __format__ ( __printf__ , 3 , 0 ) ) ) ;
/* Read formatted input from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fscanf ( FILE * __restrict __stream ,
		   const char * __restrict __format , ... ) ;
/* Read formatted input from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int scanf ( const char * __restrict __format , ... ) ;
/* Read formatted input from S.  */ 
extern int sscanf ( const char * __restrict __s ,
		   const char * __restrict __format , ... ) ;
extern int __isoc99_fscanf ( FILE * __restrict __stream ,
			    const char * __restrict __format , ... ) ;
extern int __isoc99_scanf ( const char * __restrict __format , ... ) ;
extern int __isoc99_sscanf ( const char * __restrict __s ,
			    const char * __restrict __format , ... ) ;
/* Read formatted input from S into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int vfscanf ( FILE * __restrict __s , const char * __restrict __format ,
		    __gnuc_va_list __arg )
     __attribute__ ( ( __format__ ( __scanf__ , 2 , 0 ) ) ) ;
/* Read formatted input from stdin into argument list ARG.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int vscanf ( const char * __restrict __format , __gnuc_va_list __arg )
     __attribute__ ( ( __format__ ( __scanf__ , 1 , 0 ) ) ) ;
/* Read formatted input from S into argument list ARG.  */ 
extern int vsscanf ( const char * __restrict __s ,
		    const char * __restrict __format , __gnuc_va_list __arg )
      __attribute__ ( ( __format__ ( __scanf__ , 2 , 0 ) ) ) ;
extern int __isoc99_vfscanf ( FILE * __restrict __s ,
			     const char * __restrict __format ,
			     __gnuc_va_list __arg ) ;
extern int __isoc99_vscanf ( const char * __restrict __format ,
			    __gnuc_va_list __arg ) ;
extern int __isoc99_vsscanf ( const char * __restrict __s ,
			     const char * __restrict __format ,
			     __gnuc_va_list __arg ) ;
/* Read a character from STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int fgetc ( FILE * __stream ) ;
extern int getc ( FILE * __stream ) ;
/* Read a character from stdin.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int getchar ( void ) ;
/* The C standard explicitly says this is a macro, so we always do the
   optimization for it.  */ 
/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int getc_unlocked ( FILE * __stream ) ;
extern int getchar_unlocked ( void ) ;
/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */ 
extern int fgetc_unlocked ( FILE * __stream ) ;
/* Write a character to STREAM.

   These functions are possible cancellation points and therefore not
   marked with __THROW.

   These functions is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fputc ( int __c , FILE * __stream ) ;
extern int putc ( int __c , FILE * __stream ) ;
/* Write a character to stdout.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int putchar ( int __c ) ;
/* The C standard explicitly says this can be a macro,
   so we always do the optimization for it.  */ 
/* Faster version when locking is not necessary.

   This function is not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation it is a cancellation point and
   therefore not marked with __THROW.  */ 
extern int fputc_unlocked ( int __c , FILE * __stream ) ;
/* These are defined in POSIX.1:1996.

   These functions are possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int putc_unlocked ( int __c , FILE * __stream ) ;
extern int putchar_unlocked ( int __c ) ;
/* Get a word (int) from STREAM.  */ 
extern int getw ( FILE * __stream ) ;
/* Write a word (int) to STREAM.  */ 
extern int putw ( int __w , FILE * __stream ) ;
/* Get a newline-terminated string of finite length from STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern char * fgets ( char * __restrict __s , int __n , FILE * __restrict __stream )
     ;
/* Get a newline-terminated string from stdin, removing the newline.
   DO NOT USE THIS FUNCTION!!  There is no limit on how much it will read.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern char * gets ( char * __s ) ;
/* Write a string to STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int fputs ( const char * __restrict __s , FILE * __restrict __stream ) ;
/* Write a string, followed by a newline, to stdout.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int puts ( const char * __s ) ;
/* Push a character back onto the input buffer of STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int ungetc ( int __c , FILE * __stream ) ;
/* Read chunks of generic data from STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern size_t fread ( void * __restrict __ptr , size_t __size ,
		     size_t __n , FILE * __restrict __stream ) ;
/* Write chunks of generic data to STREAM.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern size_t fwrite ( const void * __restrict __ptr , size_t __size ,
		      size_t __n , FILE * __restrict __s ) ;
/* Faster versions when locking is not necessary.

   These functions are not part of POSIX and therefore no official
   cancellation point.  But due to similarity with an POSIX interface
   or due to the implementation they are cancellation points and
   therefore not marked with __THROW.  */ 
extern size_t fread_unlocked ( void * __restrict __ptr , size_t __size ,
			      size_t __n , FILE * __restrict __stream ) ;
extern size_t fwrite_unlocked ( const void * __restrict __ptr , size_t __size ,
			       size_t __n , FILE * __restrict __stream ) ;
/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fseek ( FILE * __stream , long int __off , int __whence ) ;
/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern long int ftell ( FILE * __stream ) ;
/* Rewind to the beginning of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern void rewind ( FILE * __stream ) ;
/* The Single Unix Specification, Version 2, specifies an alternative,
   more adequate interface for the two functions above which deal with
   file offset.  `long int' is not the right type.  These definitions
   are originally defined in the Large File Support API.  */ 
/* Seek to a certain position on STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fseeko ( FILE * __stream , __off_t __off , int __whence ) ;
/* Return the current position of STREAM.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern __off_t ftello ( FILE * __stream ) ;
/* Get STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fgetpos ( FILE * __restrict __stream , fpos_t * __restrict __pos ) ;
/* Set STREAM's position.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int fsetpos ( FILE * __stream , const fpos_t * __pos ) ;
/* Clear the error and EOF indicators for STREAM.  */ 
extern void clearerr ( FILE * __stream ) ;
/* Return the EOF indicator for STREAM.  */ 
extern int feof ( FILE * __stream ) ;
/* Return the error indicator for STREAM.  */ 
extern int ferror ( FILE * __stream ) ;
/* Faster versions when locking is not required.  */ 
extern void clearerr_unlocked ( FILE * __stream ) ;
extern int feof_unlocked ( FILE * __stream ) ;
extern int ferror_unlocked ( FILE * __stream ) ;
/* Print a message describing the meaning of the value of errno.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern void perror ( const char * __s ) ;
/* Provide the declarations for `sys_errlist' and `sys_nerr' if they
   are available on this system.  Even if available, these variables
   should not be used directly.  The `strerror' function provides
   all the necessary functionality.  */ 
/* Declare sys_errlist and sys_nerr, or don't.  Compatibility (do) version.
   Copyright (C) 2002 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* sys_errlist and sys_nerr are deprecated.  Use strerror instead.  */ 
extern int sys_nerr ;
extern const char * const sys_errlist [ ] ;
/* Return the system file descriptor for STREAM.  */ 
extern int fileno ( FILE * __stream ) ;
/* Faster version when locking is not required.  */ 
extern int fileno_unlocked ( FILE * __stream ) ;
/* Create a new stream connected to a pipe running the given command.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern FILE * popen ( const char * __command , const char * __modes ) ;
/* Close a stream opened by popen and return the status of its child.

   This function is a possible cancellation point and therefore not
   marked with __THROW.  */ 
extern int pclose ( FILE * __stream ) ;
/* Return the name of the controlling terminal.  */ 
extern char * ctermid ( char * __s ) ;
/* These are defined in POSIX.1:1996.  */ 
/* Acquire ownership of STREAM.  */ 
extern void flockfile ( FILE * __stream ) ;
/* Try to acquire ownership of STREAM but do not block if it is not
   possible.  */ 
extern int ftrylockfile ( FILE * __stream ) ;
/* Relinquish the ownership granted for STREAM.  */ 
extern void funlockfile ( FILE * __stream ) ;
/* If we are compiling with optimizing read this file.  It contains
   several optimizing inline functions and macros.  */ 
/* Copyright (C) 1991,1992,1994,1995,1996,1997,1998,1999,2000,2001,2002
   	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
typedef __u_char u_char ;
typedef __u_short u_short ;
typedef __u_int u_int ;
typedef __u_long u_long ;
typedef __quad_t quad_t ;
typedef __u_quad_t u_quad_t ;
typedef __fsid_t fsid_t ;
typedef __loff_t loff_t ;
typedef __ino_t ino_t ;
typedef __dev_t dev_t ;
typedef __gid_t gid_t ;
typedef __mode_t mode_t ;
typedef __nlink_t nlink_t ;
typedef __uid_t uid_t ;
typedef __off_t off_t ;
typedef __pid_t pid_t ;
typedef __id_t id_t ;
typedef __ssize_t ssize_t ;
typedef __daddr_t daddr_t ;
typedef __caddr_t caddr_t ;
typedef __key_t key_t ;
/* Copyright (C) 1991-1999,2000,2001,2002,2003,2006
	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Returned by `time'.  */ 
typedef __time_t time_t ;
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Clock ID used in clock and timer functions.  */ 
typedef __clockid_t clockid_t ;
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Timer ID returned by `timer_create'.  */ 
typedef __timer_t timer_t ;
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Old compatibility names for C types.  */ 
typedef unsigned long int ulong ;
typedef unsigned short int ushort ;
typedef unsigned int uint ;
/* These size-specific names are used by some of the inet code.  */ 
/* These types are defined by the ISO C99 header <inttypes.h>. */ 
typedef	char int8_t ;
typedef	short int int16_t ;
typedef	int int32_t ;
/* #  if __GLIBC_HAVE_LONG_LONG */ 
 typedef long long int int64_t ;
/* But these were defined by ISO C without the first `_'.  */ 
typedef	unsigned char u_int8_t ;
typedef	unsigned short int u_int16_t ;
typedef	unsigned int u_int32_t ;
/* # if __GLIBC_HAVE_LONG_LONG  */ 
 typedef unsigned long long int u_int64_t ;
typedef int register_t ;
/* In BSD <sys/types.h> is expected to define BYTE_ORDER.  */ 
/* Copyright (C) 1992, 1996, 1997, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Definitions for byte order, according to significance of bytes,
   from low addresses to high addresses.  The value is what you get by
   putting '4' in the most significant byte, '3' in the second most
   significant byte, '2' in the second least significant byte, and '1'
   in the least significant byte, and then writing down one digit for
   each byte, starting with the byte at the lowest address at the left,
   and proceeding to the byte with the highest address at the right.  */ 
/* This file defines `__BYTE_ORDER' for the particular machine.  */ 
/* x86_64 is little-endian.  */ 
/* Some machines may need to use a different endianness for floating point
   values.  */ 
/* It also defines `fd_set' and the FD_* macros for `select'.  */ 
/* `fd_set' type and related macros, and `select'/`pselect' declarations.
   Copyright (C) 1996,97,98,99,2000,01,02,2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*	POSIX 1003.1g: 6.2 Select from File Descriptor Sets <sys/select.h>  */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Get definition of needed basic types.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Get __FD_* definitions.  */ 
/* Copyright (C) 1997, 1998, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* We don't use `memset' because this would require a prototype and
   the array isn't too big.  */ 
/* Get __sigset_t.  */ 
/* __sig_atomic_t, __sigset_t, and related definitions.  Linux version.
   Copyright (C) 1991, 1992, 1994, 1996, 1997, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
typedef int __sig_atomic_t ;
/* A `sigset_t' has a bit for each signal.  */ 
typedef struct
  {
    unsigned long int __val [ ( 1024 / ( 8 * sizeof ( unsigned long int ) ) ) ] ;
  } __sigset_t ;
/* We only want to define these functions if <signal.h> was actually
   included; otherwise we were included just to define the types.  Since we
   are namespace-clean, it wouldn't hurt to define extra macros.  But
   trouble can be caused by functions being defined (e.g., any global
   register vars declared later will cause compilation errors).  */ 
typedef __sigset_t sigset_t ;
/* Get definition of timer specification structures.  */ 
/* Copyright (C) 1991-1999,2000,2001,2002,2003,2006
	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* POSIX.1b structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */ 
struct timespec
  {
    __time_t tv_sec ;	/* Seconds.  */ 
    long int tv_nsec ;	/* Nanoseconds.  */ 
  } ;
/* System-dependent timing definitions.  Generic version.
   Copyright (C) 1996,1997,1999-2002,2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <time.h> instead.
 */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* A time value that is accurate to the nearest
   microsecond but also has a range of years.  */ 
struct timeval
  {
    __time_t tv_sec ;	/* Seconds.  */ 
    __suseconds_t tv_usec ;	/* Microseconds.  */ 
  } ;
typedef __suseconds_t suseconds_t ;
/* The fd_set member is required to be an array of longs.  */ 
typedef long int __fd_mask ;
/* Some versions of <linux/posix_types.h> define these macros.  */ 
/* It's easier to assume 8-bit bytes than to get CHAR_BIT.  */ 
/* fd_set for select and pselect.  */ 
typedef struct
  {
    /* XPG4.2 requires this member name.  Otherwise avoid the name
       from the global namespace.  */ 
    __fd_mask __fds_bits [ 1024 / ( 8 * sizeof ( __fd_mask ) ) ] ;
  } fd_set ;
/* Maximum number of file descriptors in `fd_set'.  */ 
/* Sometimes the fd_set member is assumed to have this type.  */ 
typedef __fd_mask fd_mask ;
/* Number of bits per word of `fd_set' (some code assumes this is 32).  */ 
/* Access macros for `fd_set'.  */ 
/* Check the first NFDS descriptors each in READFDS (if not NULL) for read
   readiness, in WRITEFDS (if not NULL) for write readiness, and in EXCEPTFDS
   (if not NULL) for exceptional conditions.  If TIMEOUT is not NULL, time out
   after waiting the interval specified therein.  Returns the number of ready
   descriptors, or -1 for errors.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int select ( int __nfds , fd_set * __restrict __readfds ,
		   fd_set * __restrict __writefds ,
		   fd_set * __restrict __exceptfds ,
		   struct timeval * __restrict __timeout ) ;
/* Same as above only that the TIMEOUT value is given with higher
   resolution and a sigmask which is been set temporarily.  This version
   should be used.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int pselect ( int __nfds , fd_set * __restrict __readfds ,
		    fd_set * __restrict __writefds ,
		    fd_set * __restrict __exceptfds ,
		    const struct timespec * __restrict __timeout ,
		    const __sigset_t * __restrict __sigmask ) ;
/* BSD defines these symbols, so we follow.  */ 
/* Definitions of macros to access `dev_t' values.
   Copyright (C) 1996, 1997, 1999, 2003, 2004, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* If the compiler does not know long long it is out of luck.  We are
   not going to hack weird hacks to support the dev_t representation
   they need.  */ 
extern unsigned int gnu_dev_major ( unsigned long long int __dev )
     ;
extern unsigned int gnu_dev_minor ( unsigned long long int __dev )
     ;
extern unsigned long long int gnu_dev_makedev ( unsigned int __major ,
					       unsigned int __minor )
     ;
/* Access the functions with their traditional names.  */ 
/* Types from the Large File Support interface.  */ 
typedef __blkcnt_t blkcnt_t ;	/* Type to count number of disk blocks.  */ 
typedef __fsblkcnt_t fsblkcnt_t ; /* Type to count file system blocks.  */ 
typedef __fsfilcnt_t fsfilcnt_t ; /* Type to count file system inodes.  */ 
/* Now add the thread types.  */ 
/* Copyright (C) 2002,2003,2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@redhat.com>, 2002.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Determine the wordsize from the preprocessor defines.  */ 
/* Thread identifiers.  The structure of the attribute type is not
   exposed on purpose.  */ 
typedef unsigned long int pthread_t ;
typedef union
{
  char __size [ 56 ] ;
  long int __align ;
} pthread_attr_t ;
typedef struct __pthread_internal_list
{
  struct __pthread_internal_list * __prev ;
  struct __pthread_internal_list * __next ;
} __pthread_list_t ;
/* Data structures for mutex handling.  The structure of the attribute
   type is not exposed on purpose.  */ 
typedef union
{
  struct __pthread_mutex_s
  {
    int __lock ;
    unsigned int __count ;
    int __owner ;
    unsigned int __nusers ;
    /* KIND must stay at this position in the structure to maintain
       binary compatibility.  */ 
    int __kind ;
    int __spins ;
    __pthread_list_t __list ;
  } __data ;
  char __size [ 40 ] ;
  long int __align ;
} pthread_mutex_t ;
typedef union
{
  char __size [ 4 ] ;
  int __align ;
} pthread_mutexattr_t ;
/* Data structure for conditional variable handling.  The structure of
   the attribute type is not exposed on purpose.  */ 
typedef union
{
  struct
  {
    int __lock ;
    unsigned int __futex ;
     unsigned long long int __total_seq ;
     unsigned long long int __wakeup_seq ;
     unsigned long long int __woken_seq ;
    void * __mutex ;
    unsigned int __nwaiters ;
    unsigned int __broadcast_seq ;
  } __data ;
  char __size [ 48 ] ;
   long long int __align ;
} pthread_cond_t ;
typedef union
{
  char __size [ 4 ] ;
  int __align ;
} pthread_condattr_t ;
/* Keys for thread-specific data */ 
typedef unsigned int pthread_key_t ;
/* Once-only execution */ 
typedef int pthread_once_t ;
/* Data structure for read-write lock variable handling.  The
   structure of the attribute type is not exposed on purpose.  */ 
typedef union
{
  struct
  {
    int __lock ;
    unsigned int __nr_readers ;
    unsigned int __readers_wakeup ;
    unsigned int __writer_wakeup ;
    unsigned int __nr_readers_queued ;
    unsigned int __nr_writers_queued ;
    int __writer ;
    int __shared ;
    unsigned long int __pad1 ;
    unsigned long int __pad2 ;
    /* FLAGS must stay at this position in the structure to maintain
       binary compatibility.  */ 
    unsigned int __flags ;
  } __data ;
  char __size [ 56 ] ;
  long int __align ;
} pthread_rwlock_t ;
typedef union
{
  char __size [ 8 ] ;
  long int __align ;
} pthread_rwlockattr_t ;
/* POSIX spinlock data type.  */ 
typedef volatile int pthread_spinlock_t ;
/* POSIX barriers data type.  The structure of the type is
   deliberately not exposed.  */ 
typedef union
{
  char __size [ 32 ] ;
  long int __align ;
} pthread_barrier_t ;
typedef union
{
  char __size [ 4 ] ;
  int __align ;
} pthread_barrierattr_t ;
/* Copyright (C) 1991, 1992, 1995-2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 5.6 File Characteristics	<sys/stat.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* Copyright (C) 1991-1999,2000,2001,2002,2003,2006
	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.23 Date and time	<time.h>
 */ 
/* The Single Unix specification says that some more types are
   available here.  */ 
/* Copyright (C) 1999,2000,2001,2002,2003 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Versions of the `struct stat' data structure.  */ 
/* x86-64 versions of the `xmknod' interface.  */ 
struct stat
  {
    __dev_t st_dev ;	/* Device.  */ 
    __ino_t st_ino ;	/* File serial number.	*/ 
    __nlink_t st_nlink ;	/* Link count.  */ 
    __mode_t st_mode ;	/* File mode.  */ 
    __uid_t st_uid ;	/* User ID of the file's owner.	*/ 
    __gid_t st_gid ;	/* Group ID of the file's group.*/ 
    int pad0 ;
    __dev_t st_rdev ;	/* Device number, if device.  */ 
    __off_t st_size ;	/* Size of file, in bytes.  */ 
    __blksize_t st_blksize ;	/* Optimal block size for I/O.  */ 
    __blkcnt_t st_blocks ;	/* Number 512-byte blocks allocated. */ 
    /* Nanosecond resolution timestamps are stored in a format
       equivalent to 'struct timespec'.  This is the type used
       whenever possible but the Unix namespace rules do not allow the
       identifier 'timespec' to appear in the <sys/stat.h> header.
       Therefore we have to handle the use of this header in strictly
       standard-compliant sources special.  */ 
    struct timespec st_atim ;	/* Time of last access.  */ 
    struct timespec st_mtim ;	/* Time of last modification.  */ 
    struct timespec st_ctim ;	/* Time of last status change.  */ 
    long int __unused [ 3 ] ;
  } ;
/* Tell code we have these members.  */ 
/* Nanosecond resolution time values are supported.  */ 
/* Encoding of the file mode.  */ 
/* File types.  */ 
/* POSIX.1b objects.  Note that these macros always evaluate to zero.  But
   they do it by enforcing the correct use of the macros.  */ 
/* Protection bits.  */ 
/* Test macros for file types.	*/ 
/* These are from POSIX.1b.  If the objects are not implemented using separate
   distinct file types, the macros always will evaluate to zero.  Unlike the
   other S_* macros the following three take a pointer to a `struct stat'
   object as the argument.  */ 
/* Protection bits.  */ 
/* Save swapped text after use (sticky bit).  This is pretty well obsolete.  */ 
/* Read, write, and execute by owner.  */ 
/* Read, write, and execute by group.  */ 
/* Read, write, and execute by others.  */ 
/* Macros for common mode bit masks.  */ 
/* Get file attributes for FILE and put them in BUF.  */ 
extern int stat ( const char * __restrict __file ,
		 struct stat * __restrict __buf ) ;
/* Get file attributes for the file, device, pipe, or socket
   that file descriptor FD is open on and put them in BUF.  */ 
extern int fstat ( int __fd , struct stat * __buf ) ;
/* Get file attributes about FILE and put them in BUF.
   If FILE is a symbolic link, do not follow it.  */ 
extern int lstat ( const char * __restrict __file ,
		  struct stat * __restrict __buf ) ;
/* Set file access permissions for FILE to MODE.
   If FILE is a symbolic link, this affects its target instead.  */ 
extern int chmod ( const char * __file , __mode_t __mode )
      ;
/* Set file access permissions for FILE to MODE.
   If FILE is a symbolic link, this affects the link itself
   rather than its target.  */ 
extern int lchmod ( const char * __file , __mode_t __mode )
      ;
/* Set file access permissions of the file FD is open on to MODE.  */ 
extern int fchmod ( int __fd , __mode_t __mode ) ;
/* Set the file creation mask of the current process to MASK,
   and return the old creation mask.  */ 
extern __mode_t umask ( __mode_t __mask ) ;
/* Create a new directory named PATH, with permission bits MODE.  */ 
extern int mkdir ( const char * __path , __mode_t __mode )
      ;
/* Create a device file named PATH, with permission and special bits MODE
   and device number DEV (which can be constructed from major and minor
   device numbers with the `makedev' macro above).  */ 
extern int mknod ( const char * __path , __mode_t __mode , __dev_t __dev )
      ;
/* Create a new FIFO named PATH, with permission bits MODE.  */ 
extern int mkfifo ( const char * __path , __mode_t __mode )
      ;


/* To allow the `struct stat' structure and the file type `mode_t'
   bits to vary without changing shared library major version number,
   the `stat' family of functions and `mknod' are in fact inline
   wrappers around calls to `xstat', `fxstat', `lxstat', and `xmknod',
   which all take a leading version-number argument designating the
   data structure and bits used.  <bits/stat.h> defines _STAT_VER with
   the version number corresponding to `struct stat' as defined in
   that file; and _MKNOD_VER with the version number corresponding to
   the S_IF* macros defined therein.  It is arranged that when not
   inlined these function are always statically linked; that way a
   dynamically-linked executable always encodes the version number
   corresponding to the data structures it uses, so the `x' functions
   in the shared library can adapt without needing to recompile all
   callers.  */ 
/* Wrappers for stat and mknod system calls.  */ 
extern int __fxstat ( int __ver , int __fildes , struct stat * __stat_buf )
      ;
extern int __xstat ( int __ver , const char * __filename ,
		    struct stat * __stat_buf ) ;
extern int __lxstat ( int __ver , const char * __filename ,
		     struct stat * __stat_buf ) ;
extern int __fxstatat ( int __ver , int __fildes , const char * __filename ,
		       struct stat * __stat_buf , int __flag )
      ;
extern int __xmknod ( int __ver , const char * __path , __mode_t __mode ,
		     __dev_t * __dev ) ;
extern int __xmknodat ( int __ver , int __fd , const char * __path ,
		       __mode_t __mode , __dev_t * __dev )
      ;
/* Copyright (C) 1991,1992,1994-2001,2003,2004,2005,2006,2007
	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 6.5 File Control Operations	<fcntl.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* This must be early so <bits/fcntl.h> can define types winningly.  */ 
/* Get the definitions of O_*, F_*, FD_*: all the
   numbers and flag bits for `open', `fcntl', et al.  */ 
/* O_*, F_*, FD_* bit values for Linux/x86-64.
   Copyright (C) 2001, 2002, 2004, 2006, 2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Copyright (C) 1991,1992,1994,1995,1996,1997,1998,1999,2000,2001,2002
   	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
 */ 
/* Determine the wordsize from the preprocessor defines.  */ 
/* open/fcntl - O_SYNC is only implemented on blocks devices and on files
   located on an ext2 file system */ 
/* For now Linux has synchronisity options for data and read operations.
   We define the symbols here but let them do the same as O_SYNC since
   this is a superset.	*/ 
/* Values for the second argument to `fcntl'.  */ 
/* Not necessary, we always have 64-bit offsets.  */ 
/* For F_[GET|SET]FD.  */ 
/* For posix fcntl() and `l_type' field of a `struct flock' for lockf().  */ 
/* For old implementation of bsd flock().  */ 
/* Operations for bsd flock(), also used by the kernel implementation.	*/ 
struct flock
  {
    short int l_type ;	/* Type of lock: F_RDLCK, F_WRLCK, or F_UNLCK.	*/ 
    short int l_whence ;	/* Where `l_start' is relative to (like `lseek').  */ 
    __off_t l_start ;	/* Offset where the lock begins.  */ 
    __off_t l_len ;	/* Size of the locked area; zero means until EOF.  */ 
    __pid_t l_pid ;	/* Process holding the lock.  */ 
  } ;
/* Define some more compatibility macros to be backward compatible with
   BSD systems which did not managed to hide these kernel macros.  */ 
/* Advise to `posix_fadvise'.  */ 
/* For XPG all symbols from <sys/stat.h> should also be available.  */ 
/* Values for the second argument to access.
   These may be OR'd together.  */ 
/* XPG wants the following symbols.  */ 
/* Do the file control operation described by CMD on FD.
   The remaining arguments are interpreted depending on CMD.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int fcntl ( int __fd , int __cmd , ... ) ;
/* Open FILE and return a new file descriptor for it, or -1 on error.
   OFLAG determines the type of access used.  If O_CREAT is on OFLAG,
   the third argument is taken as a `mode_t', the mode of the created file.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int open ( const char * __file , int __oflag , ... ) ;
/* Create and open FILE, with mode MODE.  This takes an `int' MODE
   argument because that is what `mode_t' will be widened to.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int creat ( const char * __file , __mode_t __mode ) ;
/* NOTE: These declarations also appear in <unistd.h>; be sure to keep both
   files consistent.  Some systems have them there and some here, and some
   software depends on the macros being defined without including both.  */ 
/* `lockf' is a simpler interface to the locking facilities of `fcntl'.
   LEN is always relative to the current file position.
   The CMD argument is one of the following.  */ 
extern int lockf ( int __fd , int __cmd , __off_t __len ) ;
/* Advice the system about the expected behaviour of the application with
   respect to the file associated with FD.  */ 
extern int posix_fadvise ( int __fd , __off_t __offset , __off_t __len ,
			  int __advise ) ;
/* Reserve storage for the data of the file associated with FD.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int posix_fallocate ( int __fd , __off_t __offset , __off_t __len ) ;
/* Define some inlines helping to catch common problems.  */ 
/* Copyright (C) 1991-2006, 2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 2.10 Symbolic Constants		<unistd.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* These may be used to determine what facilities are present at compile time.
   Their values can be obtained at run time from `sysconf'.  */ 
/* POSIX Standard approved as ISO/IEC 9945-1 as of December 2001.  */ 
/* These are not #ifdef __USE_POSIX2 because they are
   in the theoretically application-owned namespace.  */ 
/* The utilities on GNU systems also correspond to this version.  */ 
/* If defined, the implementation supports the
   C Language Bindings Option.  */ 
/* If defined, the implementation supports the
   C Language Development Utilities Option.  */ 
/* If defined, the implementation supports the
   Software Development Utilities Option.  */ 
/* If defined, the implementation supports the
   creation of locales with the localedef utility.  */ 
/* X/Open version number to which the library conforms.  It is selectable.  */ 
/* Commands and utilities from XPG4 are available.  */ 
/* We are compatible with the old published standards as well.  */ 
/* The X/Open Unix extensions are available.  */ 
/* Encryption is present.  */ 
/* The enhanced internationalization capabilities according to XPG4.2
   are present.  */ 
/* The legacy interfaces are also available.  */ 
/* Get values of POSIX options:

   If these symbols are defined, the corresponding features are
   always available.  If not, they may be available sometimes.
   The current values can be obtained with `sysconf'.

   _POSIX_JOB_CONTROL		Job control is supported.
   _POSIX_SAVED_IDS		Processes have a saved set-user-ID
				and a saved set-group-ID.
   _POSIX_REALTIME_SIGNALS	Real-time, queued signals are supported.
   _POSIX_PRIORITY_SCHEDULING	Priority scheduling is supported.
   _POSIX_TIMERS		POSIX.4 clocks and timers are supported.
   _POSIX_ASYNCHRONOUS_IO	Asynchronous I/O is supported.
   _POSIX_PRIORITIZED_IO	Prioritized asynchronous I/O is supported.
   _POSIX_SYNCHRONIZED_IO	Synchronizing file data is supported.
   _POSIX_FSYNC			The fsync function is present.
   _POSIX_MAPPED_FILES		Mapping of files to memory is supported.
   _POSIX_MEMLOCK		Locking of all memory is supported.
   _POSIX_MEMLOCK_RANGE		Locking of ranges of memory is supported.
   _POSIX_MEMORY_PROTECTION	Setting of memory protections is supported.
   _POSIX_MESSAGE_PASSING	POSIX.4 message queues are supported.
   _POSIX_SEMAPHORES		POSIX.4 counting semaphores are supported.
   _POSIX_SHARED_MEMORY_OBJECTS	POSIX.4 shared memory objects are supported.
   _POSIX_THREADS		POSIX.1c pthreads are supported.
   _POSIX_THREAD_ATTR_STACKADDR	Thread stack address attribute option supported.
   _POSIX_THREAD_ATTR_STACKSIZE	Thread stack size attribute option supported.
   _POSIX_THREAD_SAFE_FUNCTIONS	Thread-safe functions are supported.
   _POSIX_THREAD_PRIORITY_SCHEDULING
				POSIX.1c thread execution scheduling supported.
   _POSIX_THREAD_PRIO_INHERIT	Thread priority inheritance option supported.
   _POSIX_THREAD_PRIO_PROTECT	Thread priority protection option supported.
   _POSIX_THREAD_PROCESS_SHARED	Process-shared synchronization supported.
   _POSIX_PII			Protocol-independent interfaces are supported.
   _POSIX_PII_XTI		XTI protocol-indep. interfaces are supported.
   _POSIX_PII_SOCKET		Socket protocol-indep. interfaces are supported.
   _POSIX_PII_INTERNET		Internet family of protocols supported.
   _POSIX_PII_INTERNET_STREAM	Connection-mode Internet protocol supported.
   _POSIX_PII_INTERNET_DGRAM	Connectionless Internet protocol supported.
   _POSIX_PII_OSI		ISO/OSI family of protocols supported.
   _POSIX_PII_OSI_COTS		Connection-mode ISO/OSI service supported.
   _POSIX_PII_OSI_CLTS		Connectionless ISO/OSI service supported.
   _POSIX_POLL			Implementation supports `poll' function.
   _POSIX_SELECT		Implementation supports `select' and `pselect'.

   _XOPEN_REALTIME		X/Open realtime support is available.
   _XOPEN_REALTIME_THREADS	X/Open realtime thread support is available.
   _XOPEN_SHM			Shared memory interface according to XPG4.2.

   _XBS5_ILP32_OFF32		Implementation provides environment with 32-bit
				int, long, pointer, and off_t types.
   _XBS5_ILP32_OFFBIG		Implementation provides environment with 32-bit
				int, long, and pointer and off_t with at least
				64 bits.
   _XBS5_LP64_OFF64		Implementation provides environment with 32-bit
				int, and 64-bit long, pointer, and off_t types.
   _XBS5_LPBIG_OFFBIG		Implementation provides environment with at
				least 32 bits int and long, pointer, and off_t
				with at least 64 bits.

   If any of these symbols is defined as -1, the corresponding option is not
   true for any file.  If any is defined as other than -1, the corresponding
   option is true for all files.  If a symbol is not defined at all, the value
   for a specific file can be obtained from `pathconf' and `fpathconf'.

   _POSIX_CHOWN_RESTRICTED	Only the super user can use `chown' to change
				the owner of a file.  `chown' can only be used
				to change the group ID of a file to a group of
				which the calling process is a member.
   _POSIX_NO_TRUNC		Pathname components longer than
				NAME_MAX generate an error.
   _POSIX_VDISABLE		If defined, if the value of an element of the
				`c_cc' member of `struct termios' is
				_POSIX_VDISABLE, no character will have the
				effect associated with that element.
   _POSIX_SYNC_IO		Synchronous I/O may be performed.
   _POSIX_ASYNC_IO		Asynchronous I/O may be performed.
   _POSIX_PRIO_IO		Prioritized Asynchronous I/O may be performed.

   Support for the Large File Support interface is not generally available.
   If it is available the following constants are defined to one.
   _LFS64_LARGEFILE		Low-level I/O supports large files.
   _LFS64_STDIO			Standard I/O supports large files.
   */ 
/* Define POSIX options for Linux.
   Copyright (C) 1996-2004, 2006 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */ 
/* Job control is supported.  */ 
/* Processes have a saved set-user-ID and a saved set-group-ID.  */ 
/* Priority scheduling is supported.  */ 
/* Synchronizing file data is supported.  */ 
/* The fsync function is present.  */ 
/* Mapping of files to memory is supported.  */ 
/* Locking of all memory is supported.  */ 
/* Locking of ranges of memory is supported.  */ 
/* Setting of memory protections is supported.  */ 
/* Only root can change owner of file.  */ 
/* `c_cc' member of 'struct termios' structure can be disabled by
   using the value _POSIX_VDISABLE.  */ 
/* Filenames are not silently truncated.  */ 
/* X/Open realtime support is available.  */ 
/* X/Open thread realtime support is available.  */ 
/* XPG4.2 shared memory is supported.  */ 
/* Tell we have POSIX threads.  */ 
/* We have the reentrant functions described in POSIX.  */ 
/* We provide priority scheduling for threads.  */ 
/* We support user-defined stack sizes.  */ 
/* We support user-defined stacks.  */ 
/* We support priority inheritence.  */ 
/* We support priority protection, though only for non-robust
   mutexes.  */ 
/* We support POSIX.1b semaphores.  */ 
/* Real-time signals are supported.  */ 
/* We support asynchronous I/O.  */ 
/* Alternative name for Unix98.  */ 
/* Support for prioritization is also available.  */ 
/* The LFS support in asynchronous I/O is also available.  */ 
/* The rest of the LFS is also available.  */ 
/* POSIX shared memory objects are implemented.  */ 
/* CPU-time clocks support needs to be checked at runtime.  */ 
/* Clock support in threads must be also checked at runtime.  */ 
/* GNU libc provides regular expression handling.  */ 
/* Reader/Writer locks are available.  */ 
/* We have a POSIX shell.  */ 
/* We support the Timeouts option.  */ 
/* We support spinlocks.  */ 
/* The `spawn' function family is supported.  */ 
/* We have POSIX timers.  */ 
/* The barrier functions are available.  */ 
/* POSIX message queues are available.  */ 
/* Thread process-shared synchronization is supported.  */ 
/* The monotonic clock might be available.  */ 
/* The clock selection interfaces are available.  */ 
/* Advisory information interfaces are available.  */ 
/* IPv6 support is available.  */ 
/* Raw socket support is available.  */ 
/* We have at least one terminal.  */ 
/* Neither process nor thread sporadic server interfaces is available.  */ 
/* trace.h is not available.  */ 
/* Typed memory objects are not available.  */ 
/* Get the environment definitions from Unix98.  */ 
/* Standard file descriptors.  */ 
/* All functions that are not declared anywhere else.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* The Single Unix specification says that some more types are
   available here.  */ 
typedef __useconds_t useconds_t ;
typedef __intptr_t intptr_t ;
typedef __socklen_t socklen_t ;
/* Values for the second argument to access.
   These may be OR'd together.  */ 
/* Test for access to NAME using the real UID and real GID.  */ 
extern int access ( const char * __name , int __type ) ;
/* Values for the WHENCE argument to lseek.  */ 
/* Old BSD names for the same constants; just for compatibility.  */ 
/* Move FD's file position to OFFSET bytes from the
   beginning of the file (if WHENCE is SEEK_SET),
   the current position (if WHENCE is SEEK_CUR),
   or the end of the file (if WHENCE is SEEK_END).
   Return the new file position.  */ 
extern __off_t lseek ( int __fd , __off_t __offset , int __whence ) ;
/* Close the file descriptor FD.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int close ( int __fd ) ;
/* Read NBYTES into BUF from FD.  Return the
   number read, -1 for errors or 0 for EOF.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern ssize_t read ( int __fd , void * __buf , size_t __nbytes ) ;
/* Write N bytes of BUF to FD.  Return the number written, or -1.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern ssize_t write ( int __fd , const void * __buf , size_t __n ) ;
/* Create a one-way communication channel (pipe).
   If successful, two file descriptors are stored in PIPEDES;
   bytes written on PIPEDES[1] can be read from PIPEDES[0].
   Returns 0 if successful, -1 if not.  */ 
extern int pipe ( int __pipedes [ 2 ] ) ;
/* Schedule an alarm.  In SECONDS seconds, the process will get a SIGALRM.
   If SECONDS is zero, any currently scheduled alarm will be cancelled.
   The function returns the number of seconds remaining until the last
   alarm scheduled would have signaled, or zero if there wasn't one.
   There is no return value to indicate an error, but you can set `errno'
   to 0 and check its value after calling `alarm', and this might tell you.
   The signal may come late due to processor scheduling.  */ 
extern unsigned int alarm ( unsigned int __seconds ) ;
/* Make the process sleep for SECONDS seconds, or until a signal arrives
   and is not ignored.  The function returns the number of seconds less
   than SECONDS which it actually slept (thus zero if it slept the full time).
   If a signal handler does a `longjmp' or modifies the handling of the
   SIGALRM signal while inside `sleep' call, the handling of the SIGALRM
   signal afterwards is undefined.  There is no return value to indicate
   error, but if `sleep' returns SECONDS, it probably didn't work.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern unsigned int sleep ( unsigned int __seconds ) ;
/* Set an alarm to go off (generating a SIGALRM signal) in VALUE
   microseconds.  If INTERVAL is nonzero, when the alarm goes off, the
   timer is reset to go off every INTERVAL microseconds thereafter.
   Returns the number of microseconds remaining before the alarm.  */ 
extern __useconds_t ualarm ( __useconds_t __value , __useconds_t __interval )
     ;
/* Sleep USECONDS microseconds, or until a signal arrives that is not blocked
   or ignored.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int usleep ( __useconds_t __useconds ) ;
/* Suspend the process until a signal arrives.
   This always returns -1 and sets `errno' to EINTR.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int pause ( void ) ;
/* Change the owner and group of FILE.  */ 
extern int chown ( const char * __file , __uid_t __owner , __gid_t __group )
       ;
/* Change the owner and group of the file that FD is open on.  */ 
extern int fchown ( int __fd , __uid_t __owner , __gid_t __group ) ;
/* Change owner and group of FILE, if it is a symbolic
   link the ownership of the symbolic link is changed.  */ 
extern int lchown ( const char * __file , __uid_t __owner , __gid_t __group )
       ;
/* Change the process's working directory to PATH.  */ 
extern int chdir ( const char * __path ) ;
/* Change the process's working directory to the one FD is open on.  */ 
extern int fchdir ( int __fd ) ;
/* Get the pathname of the current working directory,
   and put it in SIZE bytes of BUF.  Returns NULL if the
   directory couldn't be determined or SIZE was too small.
   If successful, returns BUF.  In GNU, if BUF is NULL,
   an array is allocated with `malloc'; the array is SIZE
   bytes long, unless SIZE == 0, in which case it is as
   big as necessary.  */ 
extern char * getcwd ( char * __buf , size_t __size ) ;
/* Put the absolute pathname of the current working directory in BUF.
   If successful, return BUF.  If not, put an error message in
   BUF and return NULL.  BUF should be at least PATH_MAX bytes long.  */ 
extern char * getwd ( char * __buf )
        ;
/* Duplicate FD, returning a new file descriptor on the same file.  */ 
extern int dup ( int __fd ) ;
/* Duplicate FD to FD2, closing FD2 and making it open on the same file.  */ 
extern int dup2 ( int __fd , int __fd2 ) ;
/* NULL-terminated array of "NAME=VALUE" environment variables.  */ 
extern char * * __environ ;
/* Replace the current process, executing PATH with arguments ARGV and
   environment ENVP.  ARGV and ENVP are terminated by NULL pointers.  */ 
extern int execve ( const char * __path , char * const __argv [ ] ,
		   char * const __envp [ ] ) ;
/* Execute PATH with arguments ARGV and environment from `environ'.  */ 
extern int execv ( const char * __path , char * const __argv [ ] )
      ;
/* Execute PATH with all arguments after PATH until a NULL pointer,
   and the argument after that for environment.  */ 
extern int execle ( const char * __path , const char * __arg , ... )
      ;
/* Execute PATH with all arguments after PATH until
   a NULL pointer and environment from `environ'.  */ 
extern int execl ( const char * __path , const char * __arg , ... )
      ;
/* Execute FILE, searching in the `PATH' environment variable if it contains
   no slashes, with arguments ARGV and environment from `environ'.  */ 
extern int execvp ( const char * __file , char * const __argv [ ] )
      ;
/* Execute FILE, searching in the `PATH' environment variable if
   it contains no slashes, with all arguments after FILE until a
   NULL pointer and environment from `environ'.  */ 
extern int execlp ( const char * __file , const char * __arg , ... )
      ;
/* Add INC to priority of the current process.  */ 
extern int nice ( int __inc ) ;
/* Terminate program execution with the low-order 8 bits of STATUS.  */ 
extern void _exit ( int __status ) __attribute__ ( ( __noreturn__ ) ) ;
/* Get the `_PC_*' symbols for the NAME argument to `pathconf' and `fpathconf';
   the `_SC_*' symbols for the NAME argument to `sysconf';
   and the `_CS_*' symbols for the NAME argument to `confstr'.  */ 
/* `sysconf', `pathconf', and `confstr' NAME values.  Generic version.
   Copyright (C) 1993,1995-1998,2000,2001,2003,2004,2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Values for the NAME argument to `pathconf' and `fpathconf'.  */ 
enum
  {
    _PC_LINK_MAX ,
    _PC_MAX_CANON ,
    _PC_MAX_INPUT ,
    _PC_NAME_MAX ,
    _PC_PATH_MAX ,
    _PC_PIPE_BUF ,
    _PC_CHOWN_RESTRICTED ,
    _PC_NO_TRUNC ,
    _PC_VDISABLE ,
    _PC_SYNC_IO ,
    _PC_ASYNC_IO ,
    _PC_PRIO_IO ,
    _PC_SOCK_MAXBUF ,
    _PC_FILESIZEBITS ,
    _PC_REC_INCR_XFER_SIZE ,
    _PC_REC_MAX_XFER_SIZE ,
    _PC_REC_MIN_XFER_SIZE ,
    _PC_REC_XFER_ALIGN ,
    _PC_ALLOC_SIZE_MIN ,
    _PC_SYMLINK_MAX ,
    _PC_2_SYMLINKS
  } ;
/* Values for the argument to `sysconf'.  */ 
enum
  {
    _SC_ARG_MAX ,
    _SC_CHILD_MAX ,
    _SC_CLK_TCK ,
    _SC_NGROUPS_MAX ,
    _SC_OPEN_MAX ,
    _SC_STREAM_MAX ,
    _SC_TZNAME_MAX ,
    _SC_JOB_CONTROL ,
    _SC_SAVED_IDS ,
    _SC_REALTIME_SIGNALS ,
    _SC_PRIORITY_SCHEDULING ,
    _SC_TIMERS ,
    _SC_ASYNCHRONOUS_IO ,
    _SC_PRIORITIZED_IO ,
    _SC_SYNCHRONIZED_IO ,
    _SC_FSYNC ,
    _SC_MAPPED_FILES ,
    _SC_MEMLOCK ,
    _SC_MEMLOCK_RANGE ,
    _SC_MEMORY_PROTECTION ,
    _SC_MESSAGE_PASSING ,
    _SC_SEMAPHORES ,
    _SC_SHARED_MEMORY_OBJECTS ,
    _SC_AIO_LISTIO_MAX ,
    _SC_AIO_MAX ,
    _SC_AIO_PRIO_DELTA_MAX ,
    _SC_DELAYTIMER_MAX ,
    _SC_MQ_OPEN_MAX ,
    _SC_MQ_PRIO_MAX ,
    _SC_VERSION ,
    _SC_PAGESIZE ,
    _SC_RTSIG_MAX ,
    _SC_SEM_NSEMS_MAX ,
    _SC_SEM_VALUE_MAX ,
    _SC_SIGQUEUE_MAX ,
    _SC_TIMER_MAX ,
    /* Values for the argument to `sysconf'
       corresponding to _POSIX2_* symbols.  */ 
    _SC_BC_BASE_MAX ,
    _SC_BC_DIM_MAX ,
    _SC_BC_SCALE_MAX ,
    _SC_BC_STRING_MAX ,
    _SC_COLL_WEIGHTS_MAX ,
    _SC_EQUIV_CLASS_MAX ,
    _SC_EXPR_NEST_MAX ,
    _SC_LINE_MAX ,
    _SC_RE_DUP_MAX ,
    _SC_CHARCLASS_NAME_MAX ,
    _SC_2_VERSION ,
    _SC_2_C_BIND ,
    _SC_2_C_DEV ,
    _SC_2_FORT_DEV ,
    _SC_2_FORT_RUN ,
    _SC_2_SW_DEV ,
    _SC_2_LOCALEDEF ,
    _SC_PII ,
    _SC_PII_XTI ,
    _SC_PII_SOCKET ,
    _SC_PII_INTERNET ,
    _SC_PII_OSI ,
    _SC_POLL ,
    _SC_SELECT ,
    _SC_UIO_MAXIOV ,
    _SC_IOV_MAX = _SC_UIO_MAXIOV ,
    _SC_PII_INTERNET_STREAM ,
    _SC_PII_INTERNET_DGRAM ,
    _SC_PII_OSI_COTS ,
    _SC_PII_OSI_CLTS ,
    _SC_PII_OSI_M ,
    _SC_T_IOV_MAX ,
    /* Values according to POSIX 1003.1c (POSIX threads).  */ 
    _SC_THREADS ,
    _SC_THREAD_SAFE_FUNCTIONS ,
    _SC_GETGR_R_SIZE_MAX ,
    _SC_GETPW_R_SIZE_MAX ,
    _SC_LOGIN_NAME_MAX ,
    _SC_TTY_NAME_MAX ,
    _SC_THREAD_DESTRUCTOR_ITERATIONS ,
    _SC_THREAD_KEYS_MAX ,
    _SC_THREAD_STACK_MIN ,
    _SC_THREAD_THREADS_MAX ,
    _SC_THREAD_ATTR_STACKADDR ,
    _SC_THREAD_ATTR_STACKSIZE ,
    _SC_THREAD_PRIORITY_SCHEDULING ,
    _SC_THREAD_PRIO_INHERIT ,
    _SC_THREAD_PRIO_PROTECT ,
    _SC_THREAD_PROCESS_SHARED ,
    _SC_NPROCESSORS_CONF ,
    _SC_NPROCESSORS_ONLN ,
    _SC_PHYS_PAGES ,
    _SC_AVPHYS_PAGES ,
    _SC_ATEXIT_MAX ,
    _SC_PASS_MAX ,
    _SC_XOPEN_VERSION ,
    _SC_XOPEN_XCU_VERSION ,
    _SC_XOPEN_UNIX ,
    _SC_XOPEN_CRYPT ,
    _SC_XOPEN_ENH_I18N ,
    _SC_XOPEN_SHM ,
    _SC_2_CHAR_TERM ,
    _SC_2_C_VERSION ,
    _SC_2_UPE ,
    _SC_XOPEN_XPG2 ,
    _SC_XOPEN_XPG3 ,
    _SC_XOPEN_XPG4 ,
    _SC_CHAR_BIT ,
    _SC_CHAR_MAX ,
    _SC_CHAR_MIN ,
    _SC_INT_MAX ,
    _SC_INT_MIN ,
    _SC_LONG_BIT ,
    _SC_WORD_BIT ,
    _SC_MB_LEN_MAX ,
    _SC_NZERO ,
    _SC_SSIZE_MAX ,
    _SC_SCHAR_MAX ,
    _SC_SCHAR_MIN ,
    _SC_SHRT_MAX ,
    _SC_SHRT_MIN ,
    _SC_UCHAR_MAX ,
    _SC_UINT_MAX ,
    _SC_ULONG_MAX ,
    _SC_USHRT_MAX ,
    _SC_NL_ARGMAX ,
    _SC_NL_LANGMAX ,
    _SC_NL_MSGMAX ,
    _SC_NL_NMAX ,
    _SC_NL_SETMAX ,
    _SC_NL_TEXTMAX ,
    _SC_XBS5_ILP32_OFF32 ,
    _SC_XBS5_ILP32_OFFBIG ,
    _SC_XBS5_LP64_OFF64 ,
    _SC_XBS5_LPBIG_OFFBIG ,
    _SC_XOPEN_LEGACY ,
    _SC_XOPEN_REALTIME ,
    _SC_XOPEN_REALTIME_THREADS ,
    _SC_ADVISORY_INFO ,
    _SC_BARRIERS ,
    _SC_BASE ,
    _SC_C_LANG_SUPPORT ,
    _SC_C_LANG_SUPPORT_R ,
    _SC_CLOCK_SELECTION ,
    _SC_CPUTIME ,
    _SC_THREAD_CPUTIME ,
    _SC_DEVICE_IO ,
    _SC_DEVICE_SPECIFIC ,
    _SC_DEVICE_SPECIFIC_R ,
    _SC_FD_MGMT ,
    _SC_FIFO ,
    _SC_PIPE ,
    _SC_FILE_ATTRIBUTES ,
    _SC_FILE_LOCKING ,
    _SC_FILE_SYSTEM ,
    _SC_MONOTONIC_CLOCK ,
    _SC_MULTI_PROCESS ,
    _SC_SINGLE_PROCESS ,
    _SC_NETWORKING ,
    _SC_READER_WRITER_LOCKS ,
    _SC_SPIN_LOCKS ,
    _SC_REGEXP ,
    _SC_REGEX_VERSION ,
    _SC_SHELL ,
    _SC_SIGNALS ,
    _SC_SPAWN ,
    _SC_SPORADIC_SERVER ,
    _SC_THREAD_SPORADIC_SERVER ,
    _SC_SYSTEM_DATABASE ,
    _SC_SYSTEM_DATABASE_R ,
    _SC_TIMEOUTS ,
    _SC_TYPED_MEMORY_OBJECTS ,
    _SC_USER_GROUPS ,
    _SC_USER_GROUPS_R ,
    _SC_2_PBS ,
    _SC_2_PBS_ACCOUNTING ,
    _SC_2_PBS_LOCATE ,
    _SC_2_PBS_MESSAGE ,
    _SC_2_PBS_TRACK ,
    _SC_SYMLOOP_MAX ,
    _SC_STREAMS ,
    _SC_2_PBS_CHECKPOINT ,
    _SC_V6_ILP32_OFF32 ,
    _SC_V6_ILP32_OFFBIG ,
    _SC_V6_LP64_OFF64 ,
    _SC_V6_LPBIG_OFFBIG ,
    _SC_HOST_NAME_MAX ,
    _SC_TRACE ,
    _SC_TRACE_EVENT_FILTER ,
    _SC_TRACE_INHERIT ,
    _SC_TRACE_LOG ,
    _SC_LEVEL1_ICACHE_SIZE ,
    _SC_LEVEL1_ICACHE_ASSOC ,
    _SC_LEVEL1_ICACHE_LINESIZE ,
    _SC_LEVEL1_DCACHE_SIZE ,
    _SC_LEVEL1_DCACHE_ASSOC ,
    _SC_LEVEL1_DCACHE_LINESIZE ,
    _SC_LEVEL2_CACHE_SIZE ,
    _SC_LEVEL2_CACHE_ASSOC ,
    _SC_LEVEL2_CACHE_LINESIZE ,
    _SC_LEVEL3_CACHE_SIZE ,
    _SC_LEVEL3_CACHE_ASSOC ,
    _SC_LEVEL3_CACHE_LINESIZE ,
    _SC_LEVEL4_CACHE_SIZE ,
    _SC_LEVEL4_CACHE_ASSOC ,
    _SC_LEVEL4_CACHE_LINESIZE ,
    /* Leave room here, maybe we need a few more cache levels some day.  */ 
    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50 ,
    _SC_RAW_SOCKETS
  } ;
/* Values for the NAME argument to `confstr'.  */ 
enum
  {
    _CS_PATH ,	/* The default search path.  */ 
    _CS_V6_WIDTH_RESTRICTED_ENVS ,
    _CS_GNU_LIBC_VERSION ,
    _CS_GNU_LIBPTHREAD_VERSION ,
    _CS_LFS_CFLAGS = 1000 ,
    _CS_LFS_LDFLAGS ,
    _CS_LFS_LIBS ,
    _CS_LFS_LINTFLAGS ,
    _CS_LFS64_CFLAGS ,
    _CS_LFS64_LDFLAGS ,
    _CS_LFS64_LIBS ,
    _CS_LFS64_LINTFLAGS ,
    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100 ,
    _CS_XBS5_ILP32_OFF32_LDFLAGS ,
    _CS_XBS5_ILP32_OFF32_LIBS ,
    _CS_XBS5_ILP32_OFF32_LINTFLAGS ,
    _CS_XBS5_ILP32_OFFBIG_CFLAGS ,
    _CS_XBS5_ILP32_OFFBIG_LDFLAGS ,
    _CS_XBS5_ILP32_OFFBIG_LIBS ,
    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS ,
    _CS_XBS5_LP64_OFF64_CFLAGS ,
    _CS_XBS5_LP64_OFF64_LDFLAGS ,
    _CS_XBS5_LP64_OFF64_LIBS ,
    _CS_XBS5_LP64_OFF64_LINTFLAGS ,
    _CS_XBS5_LPBIG_OFFBIG_CFLAGS ,
    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS ,
    _CS_XBS5_LPBIG_OFFBIG_LIBS ,
    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS ,
    _CS_POSIX_V6_ILP32_OFF32_CFLAGS ,
    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS ,
    _CS_POSIX_V6_ILP32_OFF32_LIBS ,
    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS ,
    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS ,
    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS ,
    _CS_POSIX_V6_ILP32_OFFBIG_LIBS ,
    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS ,
    _CS_POSIX_V6_LP64_OFF64_CFLAGS ,
    _CS_POSIX_V6_LP64_OFF64_LDFLAGS ,
    _CS_POSIX_V6_LP64_OFF64_LIBS ,
    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS ,
    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS ,
    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS ,
    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS ,
    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS
  } ;
/* Get file-specific configuration information about PATH.  */ 
extern long int pathconf ( const char * __path , int __name )
      ;
/* Get file-specific configuration about descriptor FD.  */ 
extern long int fpathconf ( int __fd , int __name ) ;
/* Get the value of the system variable NAME.  */ 
extern long int sysconf ( int __name ) ;
/* Get the value of the string-valued system variable NAME.  */ 
extern size_t confstr ( int __name , char * __buf , size_t __len ) ;
/* Get the process ID of the calling process.  */ 
extern __pid_t getpid ( void ) ;
/* Get the process ID of the calling process's parent.  */ 
extern __pid_t getppid ( void ) ;
/* Get the process group ID of the calling process.
   This function is different on old BSD. */ 
extern __pid_t getpgrp ( void ) ;
/* Get the process group ID of process PID.  */ 
extern __pid_t __getpgid ( __pid_t __pid ) ;
/* Set the process group ID of the process matching PID to PGID.
   If PID is zero, the current process's process group ID is set.
   If PGID is zero, the process ID of the process is used.  */ 
extern int setpgid ( __pid_t __pid , __pid_t __pgid ) ;
/* Both System V and BSD have `setpgrp' functions, but with different
   calling conventions.  The BSD function is the same as POSIX.1 `setpgid'
   (above).  The System V function takes no arguments and puts the calling
   process in its on group like `setpgid (0, 0)'.

   New programs should always use `setpgid' instead.

   The default in GNU is to provide the System V function.  The BSD
   function is available under -D_BSD_SOURCE.  */ 
/* Set the process group ID of the calling process to its own PID.
   This is exactly the same as `setpgid (0, 0)'.  */ 
extern int setpgrp ( void ) ;
/* Create a new session with the calling process as its leader.
   The process group IDs of the session and the calling process
   are set to the process ID of the calling process, which is returned.  */ 
extern __pid_t setsid ( void ) ;
/* Get the real user ID of the calling process.  */ 
extern __uid_t getuid ( void ) ;
/* Get the effective user ID of the calling process.  */ 
extern __uid_t geteuid ( void ) ;
/* Get the real group ID of the calling process.  */ 
extern __gid_t getgid ( void ) ;
/* Get the effective group ID of the calling process.  */ 
extern __gid_t getegid ( void ) ;
/* If SIZE is zero, return the number of supplementary groups
   the calling process is in.  Otherwise, fill in the group IDs
   of its supplementary groups in LIST and return the number written.  */ 
extern int getgroups ( int __size , __gid_t __list [ ] ) ;
/* Set the user ID of the calling process to UID.
   If the calling process is the super-user, set the real
   and effective user IDs, and the saved set-user-ID to UID;
   if not, the effective user ID is set to UID.  */ 
extern int setuid ( __uid_t __uid ) ;
/* Set the real user ID of the calling process to RUID,
   and the effective user ID of the calling process to EUID.  */ 
extern int setreuid ( __uid_t __ruid , __uid_t __euid ) ;
/* Set the effective user ID of the calling process to UID.  */ 
extern int seteuid ( __uid_t __uid ) ;
/* Set the group ID of the calling process to GID.
   If the calling process is the super-user, set the real
   and effective group IDs, and the saved set-group-ID to GID;
   if not, the effective group ID is set to GID.  */ 
extern int setgid ( __gid_t __gid ) ;
/* Set the real group ID of the calling process to RGID,
   and the effective group ID of the calling process to EGID.  */ 
extern int setregid ( __gid_t __rgid , __gid_t __egid ) ;
/* Set the effective group ID of the calling process to GID.  */ 
extern int setegid ( __gid_t __gid ) ;
/* Clone the calling process, creating an exact copy.
   Return -1 for errors, 0 to the new process,
   and the process ID of the new process to the old process.  */ 
extern __pid_t fork ( void ) ;
/* Clone the calling process, but without copying the whole address space.
   The calling process is suspended until the new process exits or is
   replaced by a call to `execve'.  Return -1 for errors, 0 to the new process,
   and the process ID of the new process to the old process.  */ 
extern __pid_t vfork ( void ) ;
/* Return the pathname of the terminal FD is open on, or NULL on errors.
   The returned storage is good only until the next call to this function.  */ 
extern char * ttyname ( int __fd ) ;
/* Store at most BUFLEN characters of the pathname of the terminal FD is
   open on in BUF.  Return 0 on success, otherwise an error number.  */ 
extern int ttyname_r ( int __fd , char * __buf , size_t __buflen )
       ;
/* Return 1 if FD is a valid descriptor associated
   with a terminal, zero if not.  */ 
extern int isatty ( int __fd ) ;
/* Return the index into the active-logins file (utmp) for
   the controlling terminal.  */ 
extern int ttyslot ( void ) ;
/* Make a link to FROM named TO.  */ 
extern int link ( const char * __from , const char * __to )
       ;
/* Make a symbolic link to FROM named TO.  */ 
extern int symlink ( const char * __from , const char * __to )
       ;
/* Read the contents of the symbolic link PATH into no more than
   LEN bytes of BUF.  The contents are not null-terminated.
   Returns the number of characters read, or -1 for errors.  */ 
extern ssize_t readlink ( const char * __restrict __path ,
			 char * __restrict __buf , size_t __len )
       ;
/* Remove the link NAME.  */ 
extern int unlink ( const char * __name ) ;
/* Remove the directory PATH.  */ 
extern int rmdir ( const char * __path ) ;
/* Return the foreground process group ID of FD.  */ 
extern __pid_t tcgetpgrp ( int __fd ) ;
/* Set the foreground process group ID of FD set PGRP_ID.  */ 
extern int tcsetpgrp ( int __fd , __pid_t __pgrp_id ) ;
/* Return the login name of the user.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern char * getlogin ( void ) ;
/* Return at most NAME_LEN characters of the login name of the user in NAME.
   If it cannot be determined or some other error occurred, return the error
   code.  Otherwise return 0.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int getlogin_r ( char * __name , size_t __name_len ) ;
/* Set the login name returned by `getlogin'.  */ 
extern int setlogin ( const char * __name ) ;
/* Get definitions and prototypes for functions to process the
   arguments in ARGV (ARGC of them, minus the program name) for
   options given in OPTS.  */ 
/* Declarations for getopt.
   Copyright (C) 1989-1994,1996-1999,2001,2003,2004
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* If __GNU_LIBRARY__ is not already defined, either we are being used
   standalone, or this is the first header included in the source file.
   If we are being used with glibc, we need to include <features.h>, but
   that does not exist if we are standalone.  So: if __GNU_LIBRARY__ is
   not defined, include <ctype.h>, which will pull in <features.h> for us
   if it's from glibc.  (Why ctype.h?  It's guaranteed to exist and it
   doesn't flood the namespace with stuff the way some other headers do.)  */ 
/* For communication from `getopt' to the caller.
   When `getopt' finds an option that takes an argument,
   the argument value is returned here.
   Also, when `ordering' is RETURN_IN_ORDER,
   each non-option ARGV-element is returned here.  */ 
extern char * optarg ;
/* Index in ARGV of the next element to be scanned.
   This is used for communication to and from the caller
   and for communication between successive calls to `getopt'.

   On entry to `getopt', zero means this is the first call; initialize.

   When `getopt' returns -1, this is the index of the first of the
   non-option elements that the caller should itself scan.

   Otherwise, `optind' communicates from one call to the next
   how much of ARGV has been scanned so far.  */ 
extern int optind ;
/* Callers store zero here to inhibit the error message `getopt' prints
   for unrecognized options.  */ 
extern int opterr ;
/* Set to an option character which was unrecognized.  */ 
extern int optopt ;
/* Get definitions and prototypes for functions to process the
   arguments in ARGV (ARGC of them, minus the program name) for
   options given in OPTS.

   Return the option character from OPTS just read.  Return -1 when
   there are no more options.  For unrecognized options, or options
   missing arguments, `optopt' is set to the option letter, and '?' is
   returned.

   The OPTS string is a list of characters which are recognized option
   letters, optionally followed by colons, specifying that that letter
   takes an argument, to be placed in `optarg'.

   If a letter in OPTS is followed by two colons, its argument is
   optional.  This behavior is specific to the GNU `getopt'.

   The argument `--' causes premature termination of argument
   scanning, explicitly telling `getopt' that there are no more
   options.

   If OPTS begins with `--', then non-option arguments are treated as
   arguments to the option '\0'.  This behavior is specific to the GNU
   `getopt'.  */ 
/* Many other libraries have conflicting prototypes for getopt, with
   differences in the consts, in stdlib.h.  To avoid compilation
   errors, only prototype getopt for the GNU C library.  */ 
extern int getopt ( int ___argc , char * const * ___argv , const char * __shortopts )
       ;
/* Make sure we later can get all the definitions and declarations.  */ 
/* Put the name of the current host in no more than LEN bytes of NAME.
   The result is null-terminated if LEN is large enough for the full
   name and the terminator.  */ 
extern int gethostname ( char * __name , size_t __len ) ;
/* Set the name of the current host to NAME, which is LEN bytes long.
   This call is restricted to the super-user.  */ 
extern int sethostname ( const char * __name , size_t __len )
       ;
/* Set the current machine's Internet number to ID.
   This call is restricted to the super-user.  */ 
extern int sethostid ( long int __id ) ;
/* Get and set the NIS (aka YP) domain name, if any.
   Called just like `gethostname' and `sethostname'.
   The NIS domain name is usually the empty string when not using NIS.  */ 
extern int getdomainname ( char * __name , size_t __len )
       ;
extern int setdomainname ( const char * __name , size_t __len )
       ;
/* Revoke access permissions to all processes currently communicating
   with the control terminal, and then send a SIGHUP signal to the process
   group of the control terminal.  */ 
extern int vhangup ( void ) ;
/* Revoke the access of all descriptors currently open on FILE.  */ 
extern int revoke ( const char * __file ) ;
/* Enable statistical profiling, writing samples of the PC into at most
   SIZE bytes of SAMPLE_BUFFER; every processor clock tick while profiling
   is enabled, the system examines the user PC and increments
   SAMPLE_BUFFER[((PC - OFFSET) / 2) * SCALE / 65536].  If SCALE is zero,
   disable profiling.  Returns zero on success, -1 on error.  */ 
extern int profil ( unsigned short int * __sample_buffer , size_t __size ,
		   size_t __offset , unsigned int __scale )
      ;
/* Turn accounting on if NAME is an existing file.  The system will then write
   a record for each process as it terminates, to this file.  If NAME is NULL,
   turn accounting off.  This call is restricted to the super-user.  */ 
extern int acct ( const char * __name ) ;
/* Successive calls return the shells listed in `/etc/shells'.  */ 
extern char * getusershell ( void ) ;
extern void endusershell ( void ) ; /* Discard cached info.  */ 
extern void setusershell ( void ) ; /* Rewind and re-read the file.  */ 
/* Put the program in the background, and dissociate from the controlling
   terminal.  If NOCHDIR is zero, do `chdir ("/")'.  If NOCLOSE is zero,
   redirects stdin, stdout, and stderr to /dev/null.  */ 
extern int daemon ( int __nochdir , int __noclose ) ;
/* Make PATH be the root directory (the starting point for absolute paths).
   This call is restricted to the super-user.  */ 
extern int chroot ( const char * __path ) ;
/* Prompt with PROMPT and read a string from the terminal without echoing.
   Uses /dev/tty if possible; otherwise stderr and stdin.  */ 
extern char * getpass ( const char * __prompt ) ;
/* Make all changes done to FD actually appear on disk.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int fsync ( int __fd ) ;
/* Return identifier for the current host.  */ 
extern long int gethostid ( void ) ;
/* Make all changes done to all files actually appear on disk.  */ 
extern void sync ( void ) ;
/* Return the number of bytes in a page.  This is the system's page size,
   which is not necessarily the same as the hardware page size.  */ 
extern int getpagesize ( void ) __attribute__ ( ( __const__ ) ) ;
/* Return the maximum number of file descriptors
   the current process could possibly have.  */ 
extern int getdtablesize ( void ) ;
/* Truncate FILE to LENGTH bytes.  */ 
extern int truncate ( const char * __file , __off_t __length )
       ;
/* Truncate the file FD is open on to LENGTH bytes.  */ 
extern int ftruncate ( int __fd , __off_t __length ) ;
/* Set the end of accessible data space (aka "the break") to ADDR.
   Returns zero on success and -1 for errors (with errno set).  */ 
extern int brk ( void * __addr ) ;
/* Increase or decrease the end of accessible data space by DELTA bytes.
   If successful, returns the address the previous end of data space
   (i.e. the beginning of the new space, if DELTA > 0);
   returns (void *) -1 for errors (with errno set).  */ 
extern void * sbrk ( intptr_t __delta ) ;
/* Invoke `system call' number SYSNO, passing it the remaining arguments.
   This is completely system-dependent, and not often useful.

   In Unix, `syscall' sets `errno' for all errors and most calls return -1
   for errors; in many systems you cannot pass arguments or get return
   values for all system calls (`pipe', `fork', and `getppid' typically
   among them).

   In Mach, all system calls take normal arguments and always return an
   error code (zero for success).  */ 
extern long int syscall ( long int __sysno , ... ) ;
/* Synchronize at least the data part of a file with the underlying
   media.  */ 
extern int fdatasync ( int __fildes ) ;
/* XPG4.2 specifies that prototypes for the encryption functions must
   be defined here.  */ 
/* The Single Unix specification demands this prototype to be here.
   It is also found in <stdio.h>.  */ 
/* Define some macros helping to catch buffer overflows.  */ 
/* Prototypes and definition for malloc implementation.
   Copyright (C) 1996, 1997, 1999, 2000, 2002-2004, 2005, 2007
   Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* snaroff@next.com says the NeXT needs this.  */ 
/* Irix 5.1 needs this.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
typedef long int ptrdiff_t ;
/* If this symbol has done its job, get rid of it.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* On BSD/386 1.1, at least, machine/ansi.h defines _BSD_WCHAR_T_
   instead of _WCHAR_T_, and _BSD_RUNE_T_ (which, unlike the other
   symbols in the _FOO_T_ family, stays defined even after its
   corresponding type is defined).  If we define wchar_t, then we
   must undef _WCHAR_T_; for BSD/386 1.1 (and perhaps others), if
   we undef _WCHAR_T_, then we must also define rune_t, since 
   headers like runetype.h assume that if machine/ansi.h is included,
   and _BSD_WCHAR_T_ is not defined, then rune_t is available.
   machine/ansi.h says, "Note that _WCHAR_T_ and _RUNE_T_ must be of
   the same type." */ 
/* FreeBSD 5 can't be handled well using "traditional" logic above
   since it no longer defines _BSD_RUNE_T_ yet still desires to export
   rune_t in some cases... */ 
typedef int wchar_t ;
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Offset of member MEMBER in a struct of type TYPE. */ 
/* Used by GNU libc internals. */ 
/* Allocate SIZE bytes of memory.  */ 
extern void * malloc ( size_t __size ) ;
/* Allocate NMEMB elements of SIZE bytes each, all initialized to 0.  */ 
extern void * calloc ( size_t __nmemb , size_t __size )
        ;
/* Re-allocate the previously allocated block in __ptr, making the new
   block SIZE bytes long.  */ 
/* __attribute_malloc__ is not used, because if realloc returns
   the same pointer that was passed to it, aliasing needs to be allowed
   between objects pointed by the old and new pointers.  */ 
extern void * realloc ( void * __ptr , size_t __size )
       ;
/* Free a block allocated by `malloc', `realloc' or `calloc'.  */ 
extern void free ( void * __ptr ) ;
/* Free a block allocated by `calloc'. */ 
extern void cfree ( void * __ptr ) ;
/* Allocate SIZE bytes allocated to ALIGNMENT bytes.  */ 
extern void * memalign ( size_t __alignment , size_t __size )
        ;
/* Allocate SIZE bytes on a page boundary.  */ 
extern void * valloc ( size_t __size )
        ;
/* Equivalent to valloc(minimum-page-that-holds(n)), that is, round up
   __size to nearest pagesize. */ 
extern void * pvalloc ( size_t __size )
        ;
/* Underlying allocation function; successive calls should return
   contiguous pieces of memory.  */ 
extern void * ( * __morecore ) ( ptrdiff_t __size ) ;
/* Default value of `__morecore'.  */ 
extern void * __default_morecore ( ptrdiff_t __size )
       ;
/* SVID2/XPG mallinfo structure */ 
struct mallinfo {
  int arena ; /* non-mmapped space allocated from system */ 
  int ordblks ; /* number of free chunks */ 
  int smblks ; /* number of fastbin blocks */ 
  int hblks ; /* number of mmapped regions */ 
  int hblkhd ; /* space in mmapped regions */ 
  int usmblks ; /* maximum total allocated space */ 
  int fsmblks ; /* space available in freed fastbin blocks */ 
  int uordblks ; /* total allocated space */ 
  int fordblks ; /* total free space */ 
  int keepcost ; /* top-most, releasable (via malloc_trim) space */ 
} ;
/* Returns a copy of the updated current mallinfo. */ 
extern struct mallinfo mallinfo ( void ) ;
/* SVID2/XPG mallopt options */ 
/* mallopt options that actually do something */ 
/* General SVID/XPG interface to tunable parameters. */ 
extern int mallopt ( int __param , int __val ) ;
/* Release all but __pad bytes of freed top-most memory back to the
   system. Return 1 if successful, else 0. */ 
extern int malloc_trim ( size_t __pad ) ;
/* Report the number of usable allocated bytes associated with allocated
   chunk __ptr. */ 
extern size_t malloc_usable_size ( void * __ptr ) ;
/* Prints brief summary statistics on stderr. */ 
extern void malloc_stats ( void ) ;
/* Record the state of all malloc variables in an opaque data structure. */ 
extern void * malloc_get_state ( void ) ;
/* Restore the state of all malloc variables from data obtained with
   malloc_get_state(). */ 
extern int malloc_set_state ( void * __ptr ) ;
/* Called once when malloc is initialized; redefining this variable in
   the application provides the preferred way to set up the hook
   pointers. */ 
extern void ( * __malloc_initialize_hook ) ( void ) ;
/* Hooks for debugging and user-defined versions. */ 
extern void ( * __free_hook ) ( void * __ptr , const void * ) ;
extern void * ( * __malloc_hook ) ( size_t __size , const void * ) ;
extern void * ( * __realloc_hook ) ( void * __ptr , size_t __size , const void * ) ;
extern void * ( * __memalign_hook ) ( size_t __alignment , size_t __size , const void * ) ;
extern void ( * __after_morecore_hook ) ( void ) ;
/* Activate a standard set of debugging hooks. */ 
extern void __malloc_check_init ( void ) ;
/* Copyright (C) 1991,92,93,95,96,97,98,99,2001,2002,2004,2007
   	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard 7.4: Character handling	<ctype.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* bits/types.h -- definitions of __*_t types underlying *_t types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 * Never include this file directly; use <sys/types.h> instead.
 */ 
/* These are all the characteristics of characters.
   If there get to be more than 16 distinct characteristics,
   many things must be changed that use `unsigned short int's.

   The characteristics are stored always in network byte order (big
   endian).  We define the bit value interpretations here dependent on the
   machine's byte order.  */ 
/* Copyright (C) 1992, 1996, 1997, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
enum
{
  _ISupper = ( ( 0 ) < 8 ? ( ( 1 << ( 0 ) ) << 8 ) : ( ( 1 << ( 0 ) ) >> 8 ) ) ,	/* UPPERCASE.  */ 
  _ISlower = ( ( 1 ) < 8 ? ( ( 1 << ( 1 ) ) << 8 ) : ( ( 1 << ( 1 ) ) >> 8 ) ) ,	/* lowercase.  */ 
  _ISalpha = ( ( 2 ) < 8 ? ( ( 1 << ( 2 ) ) << 8 ) : ( ( 1 << ( 2 ) ) >> 8 ) ) ,	/* Alphabetic.  */ 
  _ISdigit = ( ( 3 ) < 8 ? ( ( 1 << ( 3 ) ) << 8 ) : ( ( 1 << ( 3 ) ) >> 8 ) ) ,	/* Numeric.  */ 
  _ISxdigit = ( ( 4 ) < 8 ? ( ( 1 << ( 4 ) ) << 8 ) : ( ( 1 << ( 4 ) ) >> 8 ) ) ,	/* Hexadecimal numeric.  */ 
  _ISspace = ( ( 5 ) < 8 ? ( ( 1 << ( 5 ) ) << 8 ) : ( ( 1 << ( 5 ) ) >> 8 ) ) ,	/* Whitespace.  */ 
  _ISprint = ( ( 6 ) < 8 ? ( ( 1 << ( 6 ) ) << 8 ) : ( ( 1 << ( 6 ) ) >> 8 ) ) ,	/* Printing.  */ 
  _ISgraph = ( ( 7 ) < 8 ? ( ( 1 << ( 7 ) ) << 8 ) : ( ( 1 << ( 7 ) ) >> 8 ) ) ,	/* Graphical.  */ 
  _ISblank = ( ( 8 ) < 8 ? ( ( 1 << ( 8 ) ) << 8 ) : ( ( 1 << ( 8 ) ) >> 8 ) ) ,	/* Blank (usually SPC and TAB).  */ 
  _IScntrl = ( ( 9 ) < 8 ? ( ( 1 << ( 9 ) ) << 8 ) : ( ( 1 << ( 9 ) ) >> 8 ) ) ,	/* Control character.  */ 
  _ISpunct = ( ( 10 ) < 8 ? ( ( 1 << ( 10 ) ) << 8 ) : ( ( 1 << ( 10 ) ) >> 8 ) ) ,	/* Punctuation.  */ 
  _ISalnum = ( ( 11 ) < 8 ? ( ( 1 << ( 11 ) ) << 8 ) : ( ( 1 << ( 11 ) ) >> 8 ) )	/* Alphanumeric.  */ 
} ;
/* These are defined in ctype-info.c.
   The declarations here must match those in localeinfo.h.

   In the thread-specific locale model (see `uselocale' in <locale.h>)
   we cannot use global variables for these as was done in the past.
   Instead, the following accessor functions return the address of
   each variable, which is local to the current thread if multithreaded.

   These point into arrays of 384, so they can be indexed by any `unsigned
   char' value [0,255]; by EOF (-1); or by any `signed char' value
   [-128,-1).  ISO C requires that the ctype functions work for `unsigned
   char' values and for EOF; we also support negative `signed char' values
   for broken old programs.  The case conversion arrays are of `int's
   rather than `unsigned char's because tolower (EOF) must be EOF, which
   doesn't fit into an `unsigned char'.  But today more important is that
   the arrays are also used for multi-byte character sets.  */ 
extern const unsigned short int * * __ctype_b_loc ( void )
     __attribute__ ( ( const ) ) ;
extern const __int32_t * * __ctype_tolower_loc ( void )
     __attribute__ ( ( const ) ) ;
extern const __int32_t * * __ctype_toupper_loc ( void )
     __attribute__ ( ( const ) ) ;
/* The following names are all functions:
     int isCHARACTERISTIC(int c);
   which return nonzero iff C has CHARACTERISTIC.
   For the meaning of the characteristic names, see the `enum' above.  */ 
extern int isalnum ( int ) ;
extern int isalpha ( int ) ;
extern int iscntrl ( int ) ;
extern int isdigit ( int ) ;
extern int islower ( int ) ;
extern int isgraph ( int ) ;
extern int isprint ( int ) ;
extern int ispunct ( int ) ;
extern int isspace ( int ) ;
extern int isupper ( int ) ;
extern int isxdigit ( int ) ;
/* Return the lowercase version of C.  */ 
extern int tolower ( int __c ) ;
/* Return the uppercase version of C.  */ 
extern int toupper ( int __c ) ;
/* ISO C99 introduced one new function.  */ 
extern int isblank ( int ) ;
/* Return nonzero iff C is in the ASCII set
   (i.e., is no more than 7 bits wide).  */ 
extern int isascii ( int __c ) ;
/* Return the part of C that is in the ASCII set
   (i.e., the low-order 7 bits of C).  */ 
extern int toascii ( int __c ) ;
/* These are the same as `toupper' and `tolower' except that they do not
   check the argument for being in the range of a `char'.  */ 
extern int _toupper ( int ) ;
extern int _tolower ( int ) ;
/* This code is needed for the optimized mapping functions.  */ 
/* Copyright (C) 1991-1993, 1995-2004, 2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.21 String handling	<string.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Get size_t and NULL from <stddef.h>.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* If this symbol has done its job, get rid of it.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Offset of member MEMBER in a struct of type TYPE. */ 
/* Copy N bytes of SRC to DEST.  */ 
extern void * memcpy ( void * __restrict __dest ,
		     const void * __restrict __src , size_t __n )
      ;
/* Copy N bytes of SRC to DEST, guaranteeing
   correct behavior for overlapping strings.  */ 
extern void * memmove ( void * __dest , const void * __src , size_t __n )
      ;
/* Copy no more than N bytes of SRC to DEST, stopping when C is found.
   Return the position in DEST one byte past where C was copied,
   or NULL if C was not found in the first N bytes of SRC.  */ 
extern void * memccpy ( void * __restrict __dest , const void * __restrict __src ,
		      int __c , size_t __n )
      ;
/* Set N bytes of S to C.  */ 
extern void * memset ( void * __s , int __c , size_t __n ) ;
/* Compare N bytes of S1 and S2.  */ 
extern int memcmp ( const void * __s1 , const void * __s2 , size_t __n )
       ;
/* Search N bytes of S for C.  */ 
extern void * memchr ( const void * __s , int __c , size_t __n )
        ;
/* Copy SRC to DEST.  */ 
extern char * strcpy ( char * __restrict __dest , const char * __restrict __src )
      ;
/* Copy no more than N characters of SRC to DEST.  */ 
extern char * strncpy ( char * __restrict __dest ,
		      const char * __restrict __src , size_t __n )
      ;
/* Append SRC onto DEST.  */ 
extern char * strcat ( char * __restrict __dest , const char * __restrict __src )
      ;
/* Append no more than N characters from SRC onto DEST.  */ 
extern char * strncat ( char * __restrict __dest , const char * __restrict __src ,
		      size_t __n ) ;
/* Compare S1 and S2.  */ 
extern int strcmp ( const char * __s1 , const char * __s2 )
       ;
/* Compare N characters of S1 and S2.  */ 
extern int strncmp ( const char * __s1 , const char * __s2 , size_t __n )
       ;
/* Compare the collated forms of S1 and S2.  */ 
extern int strcoll ( const char * __s1 , const char * __s2 )
       ;
/* Put a transformation of SRC into no more than N bytes of DEST.  */ 
extern size_t strxfrm ( char * __restrict __dest ,
		       const char * __restrict __src , size_t __n )
      ;
/* Duplicate S, returning an identical malloc'd string.  */ 
extern char * strdup ( const char * __s )
       ;
/* Return a malloc'd copy of at most N bytes of STRING.  The
   resultant string is terminated even if no null terminator
   appears before STRING[N].  */ 
/* Find the first occurrence of C in S.  */ 
extern char * strchr ( const char * __s , int __c )
       ;
/* Find the last occurrence of C in S.  */ 
extern char * strrchr ( const char * __s , int __c )
       ;
/* Return the length of the initial segment of S which
   consists entirely of characters not in REJECT.  */ 
extern size_t strcspn ( const char * __s , const char * __reject )
       ;
/* Return the length of the initial segment of S which
   consists entirely of characters in ACCEPT.  */ 
extern size_t strspn ( const char * __s , const char * __accept )
       ;
/* Find the first occurrence in S of any character in ACCEPT.  */ 
extern char * strpbrk ( const char * __s , const char * __accept )
       ;
/* Find the first occurrence of NEEDLE in HAYSTACK.  */ 
extern char * strstr ( const char * __haystack , const char * __needle )
       ;
/* Divide S into tokens separated by characters in DELIM.  */ 
extern char * strtok ( char * __restrict __s , const char * __restrict __delim )
      ;
/* Divide S into tokens separated by characters in DELIM.  Information
   passed between calls are stored in SAVE_PTR.  */ 
extern char * __strtok_r ( char * __restrict __s ,
			 const char * __restrict __delim ,
			 char * * __restrict __save_ptr )
      ;
extern char * strtok_r ( char * __restrict __s , const char * __restrict __delim ,
		       char * * __restrict __save_ptr )
      ;
/* Return the length of S.  */ 
extern size_t strlen ( const char * __s )
       ;
/* Return a string describing the meaning of the `errno' code in ERRNUM.  */ 
extern char * strerror ( int __errnum ) ;
/* Reentrant version of `strerror'.
   There are 2 flavors of `strerror_r', GNU which returns the string
   and may or may not use the supplied temporary buffer and POSIX one
   which fills the string into the buffer.
   To use the POSIX version, -D_XOPEN_SOURCE=600 or -D_POSIX_C_SOURCE=200112L
   without -D_GNU_SOURCE is needed, otherwise the GNU version is
   preferred.  */ 
/* Fill BUF with a string describing the meaning of the `errno' code in
   ERRNUM.  */ 
extern int __xpg_strerror_r ( int __errnum , char * __buf , size_t __buflen )
      ;
/* We define this function always since `bzero' is sometimes needed when
   the namespace rules does not allow this.  */ 
extern void __bzero ( void * __s , size_t __n ) ;
/* Copy N bytes of SRC to DEST (like memmove, but args reversed).  */ 
extern void bcopy ( const void * __src , void * __dest , size_t __n )
      ;
/* Set N bytes of S to 0.  */ 
extern void bzero ( void * __s , size_t __n ) ;
/* Compare N bytes of S1 and S2 (same as memcmp).  */ 
extern int bcmp ( const void * __s1 , const void * __s2 , size_t __n )
       ;
/* Find the first occurrence of C in S (same as strchr).  */ 
extern char * index ( const char * __s , int __c )
       ;
/* Find the last occurrence of C in S (same as strrchr).  */ 
extern char * rindex ( const char * __s , int __c )
       ;
/* Return the position of the first bit set in I, or 0 if none are set.
   The least-significant bit is position 1, the most-significant 32.  */ 
extern int ffs ( int __i ) __attribute__ ( ( __const__ ) ) ;
/* The following two functions are non-standard but necessary for non-32 bit
   platforms.  */ 
/* Compare S1 and S2, ignoring case.  */ 
extern int strcasecmp ( const char * __s1 , const char * __s2 )
       ;
/* Compare no more than N chars of S1 and S2, ignoring case.  */ 
extern int strncasecmp ( const char * __s1 , const char * __s2 , size_t __n )
       ;
/* Return the next DELIM-delimited token from *STRINGP,
   terminating it with a '\0', and update *STRINGP to point past it.  */ 
extern char * strsep ( char * * __restrict __stringp ,
		     const char * __restrict __delim )
      ;
/* Include the C library file for definition/control */ 
/* Things that might be changed for new systems are there. */ 
/* This source file should not (need to) be edited, merely recompiled */ 
/*     Include file to define variables for Fortran to C interface(s) */ 
/*     Robert Grumbine 16 March 1998                  */ 
/*     NOSEEK added 25 March 1998                  */ 
/*     CRAY compatibility added 20 April 1998      */ 
/* The following line should be either undef or define VERBOSE */ 
/* The latter gives noisy debugging output, while the former */ 
/*   relies solely on the return codes */ 
/* Declare the system type, supported options are: */ 
/* LINUX, SGI, HP, CRAY90, IBM4, IBM8, LINUXF90 */ 
/*  #define IBM4  */ 
/*
 *  stdlib.h - interface to PGI routines. Also includes standard stdlib.h
 */ 
/*include "/usr/include/stdlib.h" */ 
/* Copyright (C) 1991-2003,2004,2005,2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	ISO C99 Standard: 7.20 General utilities	<stdlib.h>
 */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* Get size_t, wchar_t and NULL from <stddef.h>.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* If this symbol has done its job, get rid of it.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Offset of member MEMBER in a struct of type TYPE. */ 
/* Returned by `div'.  */ 
typedef struct
  {
    int quot ;	/* Quotient.  */ 
    int rem ;	/* Remainder.  */ 
  } div_t ;
/* Returned by `ldiv'.  */ 
typedef struct
  {
    long int quot ;	/* Quotient.  */ 
    long int rem ;	/* Remainder.  */ 
  } ldiv_t ;
/* Returned by `lldiv'.  */ 
 typedef struct
  {
    long long int quot ;	/* Quotient.  */ 
    long long int rem ;	/* Remainder.  */ 
  } lldiv_t ;
/* The largest number rand will return (same as INT_MAX).  */ 
/* We define these the same for all machines.
   Changes from this to the outside world should be done in `_exit'.  */ 
/* Maximum length of a multibyte character in the current locale.  */ 
extern size_t __ctype_get_mb_cur_max ( void ) ;
/* Convert a string to a floating-point number.  */ 
extern double atof ( const char * __nptr )
        ;
/* Convert a string to an integer.  */ 
extern int atoi ( const char * __nptr )
        ;
/* Convert a string to a long integer.  */ 
extern long int atol ( const char * __nptr )
        ;
/* Convert a string to a long long integer.  */ 
 extern long long int atoll ( const char * __nptr )
        ;
/* Convert a string to a floating-point number.  */ 
extern double strtod ( const char * __restrict __nptr ,
		      char * * __restrict __endptr )
       ;
/* Likewise for `float' and `long double' sizes of floating-point numbers.  */ 
extern float strtof ( const char * __restrict __nptr ,
		     char * * __restrict __endptr ) ;
extern long double strtold ( const char * __restrict __nptr ,
			    char * * __restrict __endptr )
       ;
/* Convert a string to a long integer.  */ 
extern long int strtol ( const char * __restrict __nptr ,
			char * * __restrict __endptr , int __base )
       ;
/* Convert a string to an unsigned long integer.  */ 
extern unsigned long int strtoul ( const char * __restrict __nptr ,
				  char * * __restrict __endptr , int __base )
       ;
/* Convert a string to a quadword integer.  */ 
extern long long int strtoq ( const char * __restrict __nptr ,
			     char * * __restrict __endptr , int __base )
       ;
/* Convert a string to an unsigned quadword integer.  */ 
extern unsigned long long int strtouq ( const char * __restrict __nptr ,
				       char * * __restrict __endptr , int __base )
       ;
/* Convert a string to a quadword integer.  */ 
extern long long int strtoll ( const char * __restrict __nptr ,
			      char * * __restrict __endptr , int __base )
       ;
/* Convert a string to an unsigned quadword integer.  */ 
extern unsigned long long int strtoull ( const char * __restrict __nptr ,
					char * * __restrict __endptr , int __base )
       ;
/* Convert N to base 64 using the digits "./0-9A-Za-z", least-significant
   digit first.  Returns a pointer to static storage overwritten by the
   next call.  */ 
extern char * l64a ( long int __n ) ;
/* Read a number from a string S in base 64 as above.  */ 
extern long int a64l ( const char * __s )
        ;
/* Copyright (C) 1991,1992,1994,1995,1996,1997,1998,1999,2000,2001,2002
   	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/*
 *	POSIX Standard: 2.6 Primitive System Data Types	<sys/types.h>
 */ 
/* These are the functions that actually do things.  The `random', `srandom',
   `initstate' and `setstate' functions are those from BSD Unices.
   The `rand' and `srand' functions are required by the ANSI standard.
   We provide both interfaces to the same random number generator.  */ 
/* Return a random long integer between 0 and RAND_MAX inclusive.  */ 
extern long int random ( void ) ;
/* Seed the random number generator with the given number.  */ 
extern void srandom ( unsigned int __seed ) ;
/* Initialize the random number generator to use state buffer STATEBUF,
   of length STATELEN, and seed it with SEED.  Optimal lengths are 8, 16,
   32, 64, 128 and 256, the bigger the better; values less than 8 will
   cause an error and values greater than 256 will be rounded down.  */ 
extern char * initstate ( unsigned int __seed , char * __statebuf ,
			size_t __statelen ) ;
/* Switch the random number generator to state buffer STATEBUF,
   which should have been previously initialized by `initstate'.  */ 
extern char * setstate ( char * __statebuf ) ;
/* Reentrant versions of the `random' family of functions.
   These functions all use the following data structure to contain
   state, rather than global state variables.  */ 
struct random_data
  {
    int32_t * fptr ;	/* Front pointer.  */ 
    int32_t * rptr ;	/* Rear pointer.  */ 
    int32_t * state ;	/* Array of state values.  */ 
    int rand_type ;	/* Type of random number generator.  */ 
    int rand_deg ;	/* Degree of random number generator.  */ 
    int rand_sep ;	/* Distance between front and rear.  */ 
    int32_t * end_ptr ;	/* Pointer behind state table.  */ 
  } ;
extern int random_r ( struct random_data * __restrict __buf ,
		     int32_t * __restrict __result ) ;
extern int srandom_r ( unsigned int __seed , struct random_data * __buf )
      ;
extern int initstate_r ( unsigned int __seed , char * __restrict __statebuf ,
			size_t __statelen ,
			struct random_data * __restrict __buf )
      ;
extern int setstate_r ( char * __restrict __statebuf ,
		       struct random_data * __restrict __buf )
      ;
/* Return a random integer between 0 and RAND_MAX inclusive.  */ 
extern int rand ( void ) ;
/* Seed the random number generator with the given number.  */ 
extern void srand ( unsigned int __seed ) ;
/* Reentrant interface according to POSIX.1.  */ 
extern int rand_r ( unsigned int * __seed ) ;
/* System V style 48-bit random number generator functions.  */ 
/* Return non-negative, double-precision floating-point value in [0.0,1.0).  */ 
extern double drand48 ( void ) ;
extern double erand48 ( unsigned short int __xsubi [ 3 ] ) ;
/* Return non-negative, long integer in [0,2^31).  */ 
extern long int lrand48 ( void ) ;
extern long int nrand48 ( unsigned short int __xsubi [ 3 ] )
      ;
/* Return signed, long integers in [-2^31,2^31).  */ 
extern long int mrand48 ( void ) ;
extern long int jrand48 ( unsigned short int __xsubi [ 3 ] )
      ;
/* Seed random number generator.  */ 
extern void srand48 ( long int __seedval ) ;
extern unsigned short int * seed48 ( unsigned short int __seed16v [ 3 ] )
      ;
extern void lcong48 ( unsigned short int __param [ 7 ] ) ;
/* Data structure for communication with thread safe versions.  This
   type is to be regarded as opaque.  It's only exported because users
   have to allocate objects of this type.  */ 
struct drand48_data
  {
    unsigned short int __x [ 3 ] ;	/* Current state.  */ 
    unsigned short int __old_x [ 3 ] ; /* Old state.  */ 
    unsigned short int __c ;	/* Additive const. in congruential formula.  */ 
    unsigned short int __init ;	/* Flag for initializing.  */ 
    unsigned long long int __a ;	/* Factor in congruential formula.  */ 
  } ;
/* Return non-negative, double-precision floating-point value in [0.0,1.0).  */ 
extern int drand48_r ( struct drand48_data * __restrict __buffer ,
		      double * __restrict __result ) ;
extern int erand48_r ( unsigned short int __xsubi [ 3 ] ,
		      struct drand48_data * __restrict __buffer ,
		      double * __restrict __result ) ;
/* Return non-negative, long integer in [0,2^31).  */ 
extern int lrand48_r ( struct drand48_data * __restrict __buffer ,
		      long int * __restrict __result )
      ;
extern int nrand48_r ( unsigned short int __xsubi [ 3 ] ,
		      struct drand48_data * __restrict __buffer ,
		      long int * __restrict __result )
      ;
/* Return signed, long integers in [-2^31,2^31).  */ 
extern int mrand48_r ( struct drand48_data * __restrict __buffer ,
		      long int * __restrict __result )
      ;
extern int jrand48_r ( unsigned short int __xsubi [ 3 ] ,
		      struct drand48_data * __restrict __buffer ,
		      long int * __restrict __result )
      ;
/* Seed random number generator.  */ 
extern int srand48_r ( long int __seedval , struct drand48_data * __buffer )
      ;
extern int seed48_r ( unsigned short int __seed16v [ 3 ] ,
		     struct drand48_data * __buffer ) ;
extern int lcong48_r ( unsigned short int __param [ 7 ] ,
		      struct drand48_data * __buffer )
      ;
/* Allocate SIZE bytes of memory.  */ 
extern void * malloc ( size_t __size ) ;
/* Allocate NMEMB elements of SIZE bytes each, all initialized to 0.  */ 
extern void * calloc ( size_t __nmemb , size_t __size )
       ;
/* Re-allocate the previously allocated block
   in PTR, making the new block SIZE bytes long.  */ 
/* __attribute_malloc__ is not used, because if realloc returns
   the same pointer that was passed to it, aliasing needs to be allowed
   between objects pointed by the old and new pointers.  */ 
extern void * realloc ( void * __ptr , size_t __size )
      ;
/* Free a block allocated by `malloc', `realloc' or `calloc'.  */ 
extern void free ( void * __ptr ) ;
/* Free a block.  An alias for `free'.	(Sun Unices).  */ 
extern void cfree ( void * __ptr ) ;
/* Copyright (C) 1992, 1996, 1997 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */ 
/* Copyright (C) 1991,1992,1993,1995-2006,2007 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */ 
/* 
* stddef.h
*
*      Copyright 2005, STMicroelectronics, Incorporated.
*      All rights reserved.
*
*        STMICROELECTRONICS, INCORPORATED PROPRIETARY INFORMATION
* This software is supplied under the terms of a license agreement
* or nondisclosure agreement with STMicroelectronics and may not be
* copied or disclosed except in accordance with the terms of that
* agreement.
*/ 
/* Copyright (C) 1989, 1997, 1998, 1999, 2000, 2002, 2004
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */ 
/* As a special exception, if you include this header file into source
   files compiled by GCC, this header file does not by itself cause
   the resulting executable to be covered by the GNU General Public
   License.  This exception does not however invalidate any other
   reasons why the executable file might be covered by the GNU General
   Public License.  */ 
/*
 * ISO C Standard:  7.17  Common definitions  <stddef.h>
 */ 
/* Any one of these symbols __need_* means that GNU libc
   wants us just to define one data type.  So don't define
   the symbols that indicate this file's entire job has been done.  */ 
/* This avoids lossage on SunOS but only if stdtypes.h comes first.
   There's no way to win with the other order!  Sun lossage.  */ 
/* On 4.3bsd-net2, make sure ansi.h is included, so we have
   one less case to deal with in the following.  */ 
/* On FreeBSD 5, machine/ansi.h does not exist anymore... */ 
/* In 4.3bsd-net2, machine/ansi.h defines these symbols, which are
   defined if the corresponding type is *not* defined.
   FreeBSD-2.1 defines _MACHINE_ANSI_H_ instead of _ANSI_H_ */ 
/* Sequent's header files use _PTRDIFF_T_ in some conflicting way.
   Just ignore it.  */ 
/* On VxWorks, <type/vxTypesBase.h> may have defined macros like
   _TYPE_size_t which will typedef size_t.  fixincludes patched the
   vxTypesBase.h so that this macro is only defined if _GCC_SIZE_T is
   not defined, and so that defining this macro defines _GCC_SIZE_T.
   If we find that the macros are still defined at this point, we must
   invoke them so that the type is defined as expected.  */ 
/* In case nobody has defined these types, but we aren't running under
   GCC 2.00, make sure that __PTRDIFF_TYPE__, __SIZE_TYPE__, and
   __WCHAR_TYPE__ have reasonable values.  This can happen if the
   parts of GCC is compiled by an older compiler, that actually
   include gstddef.h, such as collect2.  */ 
/* Signed type of difference of two pointers.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* If this symbol has done its job, get rid of it.  */ 
/* Unsigned type of `sizeof' something.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/* Wide character type.
   Locale-writers should change this as necessary to
   be big enough to hold unique values not between 0 and 127,
   and not (wchar_t) -1, for each defined multibyte character.  */ 
/* Define this type if we are doing the whole job,
   or if we want this type in particular.  */ 
/*  In 4.3bsd-net2, leave these undefined to indicate that size_t, etc.
    are already defined.  */ 
/*  BSD/OS 3.1 and FreeBSD [23].x require the MACHINE_ANSI_H check here.  */ 
/* A null pointer constant.  */ 
/* Offset of member MEMBER in a struct of type TYPE. */ 
/* Remove any previous definitions.  */ 
/* Allocate a block that will be freed when the calling function exits.  */ 
extern void * __alloca ( size_t __size ) ;
extern void * alloca ( size_t __size ) ;
extern void * __builtin_alloca ( size_t __size ) ;
/* Allocate SIZE bytes on a page boundary.  The storage cannot be freed.  */ 
extern void * valloc ( size_t __size ) ;
/* Allocate memory of SIZE bytes with an alignment of ALIGNMENT.  */ 
extern int posix_memalign ( void * * __memptr , size_t __alignment , size_t __size )
       ;
/* Abort execution and generate a core-dump.  */ 
extern void abort ( void ) __attribute__ ( ( __noreturn__ ) ) ;
/* Register a function to be called when `exit' is called.  */ 
extern int atexit ( void ( * __func ) ( void ) ) ;
/* Register a function to be called with the status
   given to `exit' and the given argument.  */ 
extern int on_exit ( void ( * __func ) ( int __status , void * __arg ) , void * __arg )
      ;
/* Call all functions registered with `atexit' and `on_exit',
   in the reverse of the order in which they were registered
   perform stdio cleanup, and terminate program execution with STATUS.  */ 
extern void exit ( int __status ) __attribute__ ( ( __noreturn__ ) ) ;
/* Terminate the program with STATUS without calling any of the
   functions registered with `atexit' or `on_exit'.  */ 
extern void _Exit ( int __status ) __attribute__ ( ( __noreturn__ ) ) ;
/* Return the value of envariable NAME, or NULL if it doesn't exist.  */ 
extern char * getenv ( const char * __name ) ;
/* This function is similar to the above but returns NULL if the
   programs is running with SUID or SGID enabled.  */ 
extern char * __secure_getenv ( const char * __name )
       ;
/* The SVID says this is in <stdio.h>, but this seems a better place.	*/ 
/* Put STRING, which is of the form "NAME=VALUE", in the environment.
   If there is no `=', remove NAME from the environment.  */ 
extern int putenv ( char * __string ) ;
/* Set NAME to VALUE in the environment.
   If REPLACE is nonzero, overwrite an existing value.  */ 
extern int setenv ( const char * __name , const char * __value , int __replace )
      ;
/* Remove the variable NAME from the environment.  */ 
extern int unsetenv ( const char * __name ) ;
/* The `clearenv' was planned to be added to POSIX.1 but probably
   never made it.  Nevertheless the POSIX.9 standard (POSIX bindings
   for Fortran 77) requires this function.  */ 
extern int clearenv ( void ) ;
/* Generate a unique temporary file name from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the file name unique.
   Returns TEMPLATE, or a null pointer if it cannot get a unique file name.  */ 
extern char * mktemp ( char * __template ) ;
/* Generate a unique temporary file name from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the filename unique.
   Returns a file descriptor open on the file for reading and writing,
   or -1 if it cannot create a uniquely-named file.

   This function is a possible cancellation points and therefore not
   marked with __THROW.  */ 
extern int mkstemp ( char * __template ) ;
/* Create a unique temporary directory from TEMPLATE.
   The last six characters of TEMPLATE must be "XXXXXX";
   they are replaced with a string that makes the directory name unique.
   Returns TEMPLATE, or a null pointer if it cannot get a unique name.
   The directory is created mode 700.  */ 
extern char * mkdtemp ( char * __template ) ;
/* Execute the given line as a shell command.

   This function is a cancellation point and therefore not marked with
   __THROW.  */ 
extern int system ( const char * __command ) ;
/* Return the canonical absolute name of file NAME.  If RESOLVED is
   null, the result is malloc'd; otherwise, if the canonical name is
   PATH_MAX chars or more, returns null with `errno' set to
   ENAMETOOLONG; if the name fits in fewer than PATH_MAX chars,
   returns the name in RESOLVED.  */ 
extern char * realpath ( const char * __restrict __name ,
		       char * __restrict __resolved ) ;
/* Shorthand for type of comparison functions.  */ 
typedef int ( * __compar_fn_t ) ( const void * , const void * ) ;
/* Do a binary search for KEY in BASE, which consists of NMEMB elements
   of SIZE bytes each, using COMPAR to perform the comparisons.  */ 
extern void * bsearch ( const void * __key , const void * __base ,
		      size_t __nmemb , size_t __size , __compar_fn_t __compar )
      ;
/* Sort NMEMB elements of BASE, of SIZE bytes each,
   using COMPAR to perform the comparisons.  */ 
extern void qsort ( void * __base , size_t __nmemb , size_t __size ,
		   __compar_fn_t __compar ) ;
/* Return the absolute value of X.  */ 
extern int abs ( int __x ) __attribute__ ( ( __const__ ) ) ;
extern long int labs ( long int __x ) __attribute__ ( ( __const__ ) ) ;
 extern long long int llabs ( long long int __x )
      __attribute__ ( ( __const__ ) ) ;
/* Return the `div_t', `ldiv_t' or `lldiv_t' representation
   of the value of NUMER over DENOM. */ 
/* GCC may have built-ins for these someday.  */ 
extern div_t div ( int __numer , int __denom )
      __attribute__ ( ( __const__ ) ) ;
extern ldiv_t ldiv ( long int __numer , long int __denom )
      __attribute__ ( ( __const__ ) ) ;
 extern lldiv_t lldiv ( long long int __numer ,
				    long long int __denom )
      __attribute__ ( ( __const__ ) ) ;
/* Convert floating point numbers to strings.  The returned values are
   valid only until another call to the same function.  */ 
/* Convert VALUE to a string with NDIGIT digits and return a pointer to
   this.  Set *DECPT with the position of the decimal character and *SIGN
   with the sign of the number.  */ 
extern char * ecvt ( double __value , int __ndigit , int * __restrict __decpt ,
		   int * __restrict __sign ) ;
/* Convert VALUE to a string rounded to NDIGIT decimal digits.  Set *DECPT
   with the position of the decimal character and *SIGN with the sign of
   the number.  */ 
extern char * fcvt ( double __value , int __ndigit , int * __restrict __decpt ,
		   int * __restrict __sign ) ;
/* If possible convert VALUE to a string with NDIGIT significant digits.
   Otherwise use exponential representation.  The resulting string will
   be written to BUF.  */ 
extern char * gcvt ( double __value , int __ndigit , char * __buf )
       ;
/* Long double versions of above functions.  */ 
extern char * qecvt ( long double __value , int __ndigit ,
		    int * __restrict __decpt , int * __restrict __sign )
       ;
extern char * qfcvt ( long double __value , int __ndigit ,
		    int * __restrict __decpt , int * __restrict __sign )
       ;
extern char * qgcvt ( long double __value , int __ndigit , char * __buf )
       ;
/* Reentrant version of the functions above which provide their own
   buffers.  */ 
extern int ecvt_r ( double __value , int __ndigit , int * __restrict __decpt ,
		   int * __restrict __sign , char * __restrict __buf ,
		   size_t __len ) ;
extern int fcvt_r ( double __value , int __ndigit , int * __restrict __decpt ,
		   int * __restrict __sign , char * __restrict __buf ,
		   size_t __len ) ;
extern int qecvt_r ( long double __value , int __ndigit ,
		    int * __restrict __decpt , int * __restrict __sign ,
		    char * __restrict __buf , size_t __len )
      ;
extern int qfcvt_r ( long double __value , int __ndigit ,
		    int * __restrict __decpt , int * __restrict __sign ,
		    char * __restrict __buf , size_t __len )
      ;
/* Return the length of the multibyte character
   in S, which is no longer than N.  */ 
extern int mblen ( const char * __s , size_t __n ) ;
/* Return the length of the given multibyte character,
   putting its `wchar_t' representation in *PWC.  */ 
extern int mbtowc ( wchar_t * __restrict __pwc ,
		   const char * __restrict __s , size_t __n ) ;
/* Put the multibyte character represented
   by WCHAR in S, returning its length.  */ 
extern int wctomb ( char * __s , wchar_t __wchar ) ;
/* Convert a multibyte string to a wide char string.  */ 
extern size_t mbstowcs ( wchar_t * __restrict __pwcs ,
			const char * __restrict __s , size_t __n ) ;
/* Convert a wide char string to multibyte string.  */ 
extern size_t wcstombs ( char * __restrict __s ,
			const wchar_t * __restrict __pwcs , size_t __n )
     ;
/* Determine whether the string value of RESPONSE matches the affirmation
   or negative response expression as specified by the LC_MESSAGES category
   in the program's current locale.  Returns 1 if affirmative, 0 if
   negative, and -1 if not matching.  */ 
extern int rpmatch ( const char * __response ) ;
/* X/Open pseudo terminal handling.  */ 
/* Return a master pseudo-terminal handle.  */ 
extern int posix_openpt ( int __oflag ) ;
/* Put the 1 minute, 5 minute and 15 minute load averages into the first
   NELEM elements of LOADAVG.  Return the number written (never more than
   three, but may be less than NELEM), or -1 if an error occurred.  */ 
extern int getloadavg ( double __loadavg [ ] , int __nelem )
      ;
/* Define some macros helping to catch buffer overflows.  */ 
/*** PGI specific builtins ***/ 
/* always use __builtin_abs */ 
/* Do not change things below here yourself */ 
/*     IO-related (bacio.c, banio.c) */ 
/* Return Codes:  */ 
/*  0    All was well                                   */ 
/* -1    Tried to open read only _and_ write only       */ 
/* -2    Tried to read and write in the same call       */ 
/* -3    Internal failure in name processing            */ 
/* -4    Failure in opening file                        */ 
/* -5    Tried to read on a write-only file             */ 
/* -6    Failed in read to find the 'start' location    */ 
/* -7    Tried to write to a read only file             */ 
/* -8    Failed in write to find the 'start' location   */ 
/* -9    Error in close                                 */ 
/* -10   Read or wrote fewer data than requested        */ 
/* Note: In your Fortran code, call bacio, not bacio_.  */ 
/*int bacio_(int * mode, int * start, int * size, int * no, int * nactual,   */ 
/*          int * fdes, const char *fname, char *data, int  namelen,         */ 
/*          int  datanamelen)                                                */ 
/* Arguments: */ 
/* Mode is the integer specifying operations to be performed                 */ 
/*    see the clib.inc file for the values.  Mode is obtained                */ 
/*    by adding together the values corresponding to the operations          */ 
/*    The best method is to include the clib.inc file and refer to the       */ 
/*    names for the operations rather than rely on hard-coded values         */ 
/* Start is the byte number to start your operation from.  0 is the first    */ 
/*    byte in the file, not 1.                                               */ 
/* Newpos is the position in the file after a read or write has been         */ 
/*    performed.  You'll need this if you're doing 'seeking' read/write      */ 
/* Size is the size of the objects you are trying to read.  Rely on the      */ 
/*    values in the locale.inc file.  Types are CHARACTER, INTEGER, REAL,    */ 
/*    COMPLEX.  Specify the correct value by using SIZEOF_type, where type   */ 
/*    is one of these.  (After having included the locale.inc file)          */ 
/* no is the number of things to read or write (characters, integers,        */ 
/*                                                              whatever)    */ 
/* nactual is the number of things actually read or written.  Check that     */ 
/*    you got what you wanted.                                               */ 
/* fdes is an integer 'file descriptor'.  This is not a Fortran Unit Number  */ 
/*    You can use it, however, to refer to files you've previously opened.   */ 
/* fname is the name of the file.  This only needs to be defined when you    */ 
/*    are opening a file.  It must be (on the Fortran side) declared as      */ 
/*    CHARACTER*N, where N is a length greater than or equal to the length   */ 
/*    of the file name.  CHARACTER*1 fname[80] (for example) will fail.      */ 
/* data is the name of the entity (variable, vector, array) that you want    */ 
/*    to write data out from or read it in to.  The fact that C is declaring */ 
/*    it to be a char * does not affect your fortran.                        */ 
/* namelen - Do NOT specify this.  It is created automagically by the        */ 
/*    Fortran compiler                                                       */ 
/* datanamelen - Ditto                                                       */ 
/* What is going on here is that although the Fortran caller will always */ 
/*   be calling bacio, the called C routine name will change from system */ 
/*   to system. */ 
  int bacio_
         ( int * mode , int * start , int * newpos , int * size , int * no , 
          int * nactual , int * fdes , const char * fname , char * datary , 
          int namelen , int datanamelen ) {
  int i , j , jret , seekret ;
  char * realname , * tempchar ;
  int tcharval ;
  size_t count ;
/* Initialization(s) */ 
  * nactual = 0 ;
/* Check for illegal combinations of options */ 
  if ( ( 1 & * mode ) &&
     ( ( 2 & * mode ) || ( 128 & * mode ) || ( 256 & * mode ) ) ) {
     
     return - 1 ;
  }
  if ( ( 16 & * mode ) && ( 32 & * mode ) ) {
     
     return - 2 ;
  }
/* This section handles Fortran to C translation of strings so as to */ 
/*   be able to open the files Fortran is expecting to be opened.    */ 
  
  if ( ( 1 & * mode ) || ( 2 & * mode ) || 
       ( 128 & * mode ) || ( 256 & * mode ) ||
       ( 4 & * mode ) ) {
    
    realname = ( char * ) malloc ( namelen * sizeof ( char ) ) ;
    if ( realname == ( ( void * ) 0 ) ) { 
      
      return - 3 ;
    }
    tempchar = ( char * ) malloc ( sizeof ( char ) * 1 ) ;
    i = 0 ;
    j = 0 ;
    * tempchar = fname [ i ] ;
    tcharval = * tempchar ;
    while ( i == j && i < namelen ) {
       fflush ( stdout ) ; 
       if ( ( ( * __ctype_b_loc ( ) ) [ ( int ) ( ( tcharval ) ) ] & ( unsigned short int ) _ISgraph ) ) {
         realname [ j ] = fname [ i ] ;
         j += 1 ;
       }
       i += 1 ;
       * tempchar = fname [ i ] ;
       tcharval = * tempchar ;
    }
    
    realname [ j ] = '\0' ;
  } 
   
/* Open files with correct read/write and file permission. */ 
  if ( 1 & * mode ) {
    
     * fdes = open ( realname , 00 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 2 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 128 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 | 01000 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 256 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 | 02000 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 4 & * mode ) {
    
     * fdes = open ( realname , 02 | 0100 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else {
    
  }
  if ( * fdes < 0 ) {
    
    return - 4 ;
  }
  else {
    
  }
/* Read data as requested */ 
  if ( 16 & * mode &&
   ( ( 2 & * mode ) || ( 128 & * mode ) || ( 256 & * mode ) ) ) {
    
    return - 5 ;
  }
  else if ( 16 & * mode ) {
  /* Read in some data */ 
    if ( ! ( * mode & 64 ) ) {
      seekret = lseek ( * fdes , * start , 0 ) ;
      if ( seekret == - 1 ) {
        
        return - 6 ;
      }
      
    }
    
    if ( datary == ( ( void * ) 0 ) ) {
      printf ( "Massive catastrophe -- datary pointer is NULL\n" ) ;
      return - 666 ;
    }
    
    count = ( size_t ) * no ;
    jret = read ( * fdes , ( void * ) datary , count ) ;
    if ( jret != * no ) {
      
    } 
    else {
    
    }
    * nactual = jret ;
    * newpos = * start + jret ;
  }
/* Done with reading */ 
 
/* See if we should be writing */ 
  if ( 32 & * mode && 1 & * mode ) {
    
     return - 7 ;
  }
  else if ( 32 & * mode ) {
    if ( ! ( * mode & 64 ) ) {
      seekret = lseek ( * fdes , * start , 0 ) ;
      if ( seekret == - 1 ) {
      
        return - 8 ;
      }
    }
    
    if ( datary == ( ( void * ) 0 ) ) {
      printf ( "Massive catastrophe -- datary pointer is NULL\n" ) ;
      return - 666 ;
    }
    
    count = ( size_t ) * no ;
    jret = write ( * fdes , ( void * ) datary , count ) ;
    if ( jret != * no ) {
    
      * nactual = jret ;
      * newpos = * start + jret ;
    }
    else {
    
       * nactual = jret ;
       * newpos = * start + jret ;
    }
  }
/* Done with writing */ 
    
/* Close file if requested */ 
  if ( 8 & * mode ) {
    jret = close ( * fdes ) ;
    if ( jret != 0 ) { 
    
      return - 9 ;
    }
  }
/* Done closing */ 
/* Check that if we were reading or writing, that we actually got what */ 
/*  we expected, else return a -10.  Return 0 (success) if we're here  */ 
/*  and weren't reading or writing */ 
  if ( ( * mode & 16 || * mode & 32 ) && ( * nactual != * no ) ) {
    return - 10 ;
  }
  else {
    return 0 ;
  }
} 
  int banio_
         ( int * mode , int * start , int * newpos , int * size , int * no , 
          int * nactual , int * fdes , const char * fname , char * datary , 
          int namelen ) {
  int i , j , jret , seekret ;
  char * realname , * tempchar ;
  int tcharval ;
/* Initialization(s) */ 
  * nactual = 0 ;
/* Check for illegal combinations of options */ 
  if ( ( 1 & * mode ) &&
     ( ( 2 & * mode ) || ( 128 & * mode ) || ( 256 & * mode ) ) ) {
     
     return - 1 ;
  }
  if ( ( 16 & * mode ) && ( 32 & * mode ) ) {
     
     return - 2 ;
  }
/* This section handles Fortran to C translation of strings so as to */ 
/*   be able to open the files Fortran is expecting to be opened.    */ 
  
  if ( ( 1 & * mode ) || ( 2 & * mode ) || 
       ( 128 & * mode ) || ( 256 & * mode ) ||
       ( 4 & * mode ) ) {
    
    realname = ( char * ) malloc ( namelen * sizeof ( char ) ) ;
    if ( realname == ( ( void * ) 0 ) ) { 
      
      return - 3 ;
    }
    tempchar = ( char * ) malloc ( sizeof ( char ) * 1 ) ;
    i = 0 ;
    j = 0 ;
    * tempchar = fname [ i ] ;
    tcharval = * tempchar ;
    while ( i == j && i < namelen ) {
       fflush ( stdout ) ; 
       if ( ( ( * __ctype_b_loc ( ) ) [ ( int ) ( ( tcharval ) ) ] & ( unsigned short int ) _ISgraph ) ) {
         realname [ j ] = fname [ i ] ;
         j += 1 ;
       }
       i += 1 ;
       * tempchar = fname [ i ] ;
       tcharval = * tempchar ;
    }
    
    realname [ j ] = '\0' ;
  } 
   
/* Open files with correct read/write and file permission. */ 
  if ( 1 & * mode ) {
    
     * fdes = open ( realname , 00 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 2 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 128 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 | 01000 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 256 & * mode ) {
    
     * fdes = open ( realname , 01 | 0100 | 02000 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else if ( 4 & * mode ) {
    
     * fdes = open ( realname , 02 | 0100 , ( 0400 | 0200 | 0100 ) | ( ( 0400 | 0200 | 0100 ) >> 3 ) | ( ( ( 0400 | 0200 | 0100 ) >> 3 ) >> 3 ) ) ;
  }
  else {
    
  }
  if ( * fdes < 0 ) {
    
    return - 4 ;
  }
  else {
    
  }
/* Read data as requested */ 
  if ( 16 & * mode &&
   ( ( 2 & * mode ) || ( 128 & * mode ) || ( 256 & * mode ) ) ) {
    
    return - 5 ;
  }
  else if ( 16 & * mode ) {
  /* Read in some data */ 
    if ( ! ( * mode & 64 ) ) {
      seekret = lseek ( * fdes , * start , 0 ) ;
      if ( seekret == - 1 ) {
        
        return - 6 ;
      }
      
    }
    jret = read ( * fdes , datary , * no * ( * size ) ) ;
    if ( jret != * no * ( * size ) ) {
      
      * nactual = jret / ( * size ) ;
      * newpos = * start + jret ;
    } 
    
    * nactual = jret / ( * size ) ;
    * newpos = * start + jret ;
  }
/* Done with reading */ 
 
/* See if we should be writing */ 
  if ( 32 & * mode && 1 & * mode ) {
    
     return - 7 ;
  }
  else if ( 32 & * mode ) {
    if ( ! ( * mode & 64 ) ) {
      seekret = lseek ( * fdes , * start , 0 ) ;
      if ( seekret == - 1 ) {
      
        return - 8 ;
      }
      
    }
    jret = write ( * fdes , datary , * no * ( * size ) ) ;
    if ( jret != * no * ( * size ) ) {
    
      * nactual = jret / ( * size ) ;
      * newpos = * start + jret ;
    }
    else {
    
       * nactual = jret / ( * size ) ;
       * newpos = * start + jret ;
    }
  }
/* Done with writing */ 
    
/* Close file if requested */ 
  if ( 8 & * mode ) {
    jret = close ( * fdes ) ;
    if ( jret != 0 ) { 
    
      return - 9 ;
    }
  }
/* Done closing */ 
/* Check that if we were reading or writing, that we actually got what */ 
/*  we expected, else return a -10.  Return 0 (success) if we're here  */ 
/*  and weren't reading or writing */ 
  if ( ( * mode & 16 || * mode & 32 ) && ( * nactual != * no ) ) {
    return - 10 ;
  }
  else {
    return 0 ;
  }
} 
