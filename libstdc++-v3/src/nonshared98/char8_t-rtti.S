/* Copyright (C) 2012-2024 Free Software Foundation, Inc.

   This file is part of the GNU ISO C++ Library.  This library is free
   software; you can redistribute it and/or modify it under the
   terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#if defined __x86_64__ || defined __powerpc64__ || defined __s390x__ || defined __ia64__ || defined __aarch64__ \
    || defined __i386__ || defined __powerpc__ || defined __s390__
#ifdef __i386__
#define ALIGN1	.align 4
#elif defined __x86_64__
#define ALIGN1	.align 32
#define ALIGN2	.align 16
#elif defined __ia64__
#define ALIGN1	.align 8
#define ALIGN3	.align 8
#define SECTION3(x).section .gnu.linkonce.s.x,"aws",@progbits
#define POINTER	data8
#define FLAGS data4
#define PAD .skip 4
#define STRING stringz
#elif defined __powerpc64__
#define ALIGN1	.align 3
#define ALIGN3	.align 3
#elif defined __powerpc__
#define ALIGN1	.align 2
#define ALIGN3	.align 2
#define SECTION2(x).section .gnu.linkonce.s.x,"aw",@progbits
#define SECTION3(x)SECTION2(x)
#elif defined __aarch64__
#define ALIGN1	.align 3
#define ALIGN3	.align 3
#define POINTER	.xword
#define FLAGS	.word
#define OBJECT	%object
#elif defined __s390x__
#define ALIGN1	.align 8
#define ALIGN3	.align 2
#elif defined __s390__
#define ALIGN1	.align 4
#define ALIGN3	.align 2
#endif
#if defined __x86_64__ || defined __powerpc64__ || defined __s390x__ || defined __ia64__ || defined __aarch64__
#define SIZE1	32
#define SIZE2	16
#define OFF	16
#ifndef POINTER
#define POINTER	.quad
#endif
#ifndef FLAGS
#define FLAGS .long
#endif
#ifndef PAD
#define PAD .zero 4
#endif
#else
#define SIZE1	16
#define SIZE2	8
#define OFF	8
#ifndef POINTER
#define POINTER	.long
#endif
#ifndef FLAGS
#define FLAGS .long
#endif
#ifndef PAD
#define PAD
#endif
#endif
#ifndef SYM
#define SYM(x)x
#endif
#ifndef ALIGN2
#define ALIGN2 ALIGN1
#endif
#ifndef ALIGN3
#define ALIGN3
#endif
#ifndef OBJECT
#define OBJECT @object
#endif
#ifndef SECTION1
#define SECTION1(x).section .gnu.linkonce.d.rel.ro.x,"aw",@progbits
#endif
#ifndef SECTION2
#define SECTION2(x)SECTION1(x)
#endif
#ifndef SECTION3
#define SECTION3(x).section .gnu.linkonce.r.x,"a",@progbits
#endif
#ifndef STRING
#define STRING .string
#endif

	.weak	SYM(_ZTIPKDu)
	SECTION1(_ZTIPKDu)
	ALIGN1
	.type	SYM(_ZTIPKDu), OBJECT
	.size	SYM(_ZTIPKDu), SIZE1
_ZTIPKDu:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDu)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDu)
	.weak	SYM(_ZTIPDu)
	SECTION1(_ZTIPDu)
	ALIGN1
	.type	SYM(_ZTIPDu), OBJECT
	.size	SYM(_ZTIPDu), SIZE1
_ZTIPDu:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDu)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDu)
	.weak	SYM(_ZTIDu)
	SECTION2(_ZTIDu)
	ALIGN2
	.type	SYM(_ZTIDu), OBJECT
	.size	SYM(_ZTIDu), SIZE2
_ZTIDu:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDu)
	.weak	SYM(_ZTSPKDu)
	.hidden	SYM(_ZTSPKDu)
	SECTION3(_ZTSPKDu)
	ALIGN3
	.type	SYM(_ZTSPKDu), OBJECT
	.size	SYM(_ZTSPKDu), 5
_ZTSPKDu:
	STRING	"PKDu"
	.weak	SYM(_ZTSPDu)
	.hidden	SYM(_ZTSPDu)
	SECTION3(_ZTSPDu)
	ALIGN3
	.type	SYM(_ZTSPDu), OBJECT
	.size	SYM(_ZTSPDu), 4
_ZTSPDu:
	STRING	"PDu"
	.weak	SYM(_ZTSDu)
	.hidden	SYM(_ZTSDu)
	SECTION3(_ZTSDu)
	ALIGN3
	.type	SYM(_ZTSDu), OBJECT
	.size	SYM(_ZTSDu), 3
_ZTSDu:
	STRING	"Du"
#endif
	.section	.note.GNU-stack,"",@progbits
