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

#if defined __x86_64__ || defined __i386__ || defined __aarch64__
	.weak	SYM(_ZTIPKDF16_)
	SECTION1(_ZTIPKDF16_)
	ALIGN1
	.type	SYM(_ZTIPKDF16_), OBJECT
	.size	SYM(_ZTIPKDF16_), SIZE1
_ZTIPKDF16_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF16_)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF16_)
	.weak	SYM(_ZTIPDF16_)
	SECTION1(_ZTIPDF16_)
	ALIGN1
	.type	SYM(_ZTIPDF16_), OBJECT
	.size	SYM(_ZTIPDF16_), SIZE1
_ZTIPDF16_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF16_)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF16_)
	.weak	SYM(_ZTIDF16_)
	SECTION2(_ZTIDF16_)
	ALIGN2
	.type	SYM(_ZTIDF16_), OBJECT
	.size	SYM(_ZTIDF16_), SIZE2
_ZTIDF16_:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF16_)
	.weak	SYM(_ZTSPKDF16_)
	.hidden	SYM(_ZTSPKDF16_)
	SECTION3(_ZTSPKDF16_)
	ALIGN3
	.type	SYM(_ZTSPKDF16_), OBJECT
	.size	SYM(_ZTSPKDF16_), 8
_ZTSPKDF16_:
	STRING	"PKDF16_"
	.weak	SYM(_ZTSPDF16_)
	.hidden	SYM(_ZTSPDF16_)
	SECTION3(_ZTSPDF16_)
	ALIGN3
	.type	SYM(_ZTSPDF16_), OBJECT
	.size	SYM(_ZTSPDF16_), 7
_ZTSPDF16_:
	STRING	"PDF16_"
	.weak	SYM(_ZTSDF16_)
	.hidden	SYM(_ZTSDF16_)
	SECTION3(_ZTSDF16_)
	ALIGN3
	.type	SYM(_ZTSDF16_), OBJECT
	.size	SYM(_ZTSDF16_), 6
_ZTSDF16_:
	STRING	"DF16_"
#endif
#if defined __x86_64__ || defined __i386__
	.weak	SYM(_ZTIPKDF16b)
	SECTION1(_ZTIPKDF16b)
	ALIGN1
	.type	SYM(_ZTIPKDF16b), OBJECT
	.size	SYM(_ZTIPKDF16b), SIZE1
_ZTIPKDF16b:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF16b)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF16b)
	.weak	SYM(_ZTIPDF16b)
	SECTION1(_ZTIPDF16b)
	ALIGN1
	.type	SYM(_ZTIPDF16b), OBJECT
	.size	SYM(_ZTIPDF16b), SIZE1
_ZTIPDF16b:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF16b)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF16b)
	.weak	SYM(_ZTIDF16b)
	SECTION2(_ZTIDF16b)
	ALIGN2
	.type	SYM(_ZTIDF16b), OBJECT
	.size	SYM(_ZTIDF16b), SIZE2
_ZTIDF16b:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF16b)
	.weak	SYM(_ZTSPKDF16b)
	.hidden	SYM(_ZTSPKDF16b)
	SECTION3(_ZTSPKDF16b)
	ALIGN3
	.type	SYM(_ZTSPKDF16b), OBJECT
	.size	SYM(_ZTSPKDF16b), 8
_ZTSPKDF16b:
	STRING	"PKDF16b"
	.weak	SYM(_ZTSPDF16b)
	.hidden	SYM(_ZTSPDF16b)
	SECTION3(_ZTSPDF16b)
	ALIGN3
	.type	SYM(_ZTSPDF16b), OBJECT
	.size	SYM(_ZTSPDF16b), 7
_ZTSPDF16b:
	STRING	"PDF16b"
	.weak	SYM(_ZTSDF16b)
	.hidden	SYM(_ZTSDF16b)
	SECTION3(_ZTSDF16b)
	ALIGN3
	.type	SYM(_ZTSDF16b), OBJECT
	.size	SYM(_ZTSDF16b), 6
_ZTSDF16b:
	STRING	"DF16b"
#endif
#if defined __aarch64__
	.weak	SYM(_ZTIPKu6__bf16)
	SECTION1(_ZTIPKu6__bf16)
	ALIGN1
	.type	SYM(_ZTIPKu6__bf16), OBJECT
	.size	SYM(_ZTIPKu6__bf16), SIZE1
_ZTIPKu6__bf16:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKu6__bf16)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIu6__bf16)
	.weak	SYM(_ZTIPu6__bf16)
	SECTION1(_ZTIPu6__bf16)
	ALIGN1
	.type	SYM(_ZTIPu6__bf16), OBJECT
	.size	SYM(_ZTIPu6__bf16), SIZE1
_ZTIPu6__bf16:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPu6__bf16)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIu6__bf16)
	.weak	SYM(_ZTIu6__bf16)
	SECTION2(_ZTIu6__bf16)
	ALIGN2
	.type	SYM(_ZTIu6__bf16), OBJECT
	.size	SYM(_ZTIu6__bf16), SIZE2
_ZTIu6__bf16:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSu6__bf16)
	.weak	SYM(_ZTSPKu6__bf16)
	.hidden	SYM(_ZTSPKu6__bf16)
	SECTION3(_ZTSPKu6__bf16)
	ALIGN3
	.type	SYM(_ZTSPKu6__bf16), OBJECT
	.size	SYM(_ZTSPKu6__bf16), 11
_ZTSPKu6__bf16:
	STRING	"PKu6__bf16"
	.weak	SYM(_ZTSPu6__bf16)
	.hidden	SYM(_ZTSPu6__bf16)
	SECTION3(_ZTSPu6__bf16)
	ALIGN3
	.type	SYM(_ZTSPu6__bf16), OBJECT
	.size	SYM(_ZTSPu6__bf16), 10
_ZTSPu6__bf16:
	STRING	"Pu6__bf16"
	.weak	SYM(_ZTSu6__bf16)
	.hidden	SYM(_ZTSu6__bf16)
	SECTION3(_ZTSu6__bf16)
	ALIGN3
	.type	SYM(_ZTSu6__bf16), OBJECT
	.size	SYM(_ZTSu6__bf16), 9
_ZTSu6__bf16:
	STRING	"u6__bf16"
#endif
	.weak	SYM(_ZTIPKDF32_)
	SECTION1(_ZTIPKDF32_)
	ALIGN1
	.type	SYM(_ZTIPKDF32_), OBJECT
	.size	SYM(_ZTIPKDF32_), SIZE1
_ZTIPKDF32_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF32_)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF32_)
	.weak	SYM(_ZTIPDF32_)
	SECTION1(_ZTIPDF32_)
	ALIGN1
	.type	SYM(_ZTIPDF32_), OBJECT
	.size	SYM(_ZTIPDF32_), SIZE1
_ZTIPDF32_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF32_)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF32_)
	.weak	SYM(_ZTIDF32_)
	SECTION2(_ZTIDF32_)
	ALIGN2
	.type	SYM(_ZTIDF32_), OBJECT
	.size	SYM(_ZTIDF32_), SIZE2
_ZTIDF32_:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF32_)
	.weak	SYM(_ZTSPKDF32_)
	.hidden	SYM(_ZTSPKDF32_)
	SECTION3(_ZTSPKDF32_)
	ALIGN3
	.type	SYM(_ZTSPKDF32_), OBJECT
	.size	SYM(_ZTSPKDF32_), 8
_ZTSPKDF32_:
	STRING	"PKDF32_"
	.weak	SYM(_ZTSPDF32_)
	.hidden	SYM(_ZTSPDF32_)
	SECTION3(_ZTSPDF32_)
	ALIGN3
	.type	SYM(_ZTSPDF32_), OBJECT
	.size	SYM(_ZTSPDF32_), 7
_ZTSPDF32_:
	STRING	"PDF32_"
	.weak	SYM(_ZTSDF32_)
	.hidden	SYM(_ZTSDF32_)
	SECTION3(_ZTSDF32_)
	ALIGN3
	.type	SYM(_ZTSDF32_), OBJECT
	.size	SYM(_ZTSDF32_), 6
_ZTSDF32_:
	STRING	"DF32_"
	.weak	SYM(_ZTIPKDF32x)
	SECTION1(_ZTIPKDF32x)
	ALIGN1
	.type	SYM(_ZTIPKDF32x), OBJECT
	.size	SYM(_ZTIPKDF32x), SIZE1
_ZTIPKDF32x:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF32x)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF32x)
	.weak	SYM(_ZTIPDF32x)
	SECTION1(_ZTIPDF32x)
	ALIGN1
	.type	SYM(_ZTIPDF32x), OBJECT
	.size	SYM(_ZTIPDF32x), SIZE1
_ZTIPDF32x:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF32x)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF32x)
	.weak	SYM(_ZTIDF32x)
	SECTION2(_ZTIDF32x)
	ALIGN2
	.type	SYM(_ZTIDF32x), OBJECT
	.size	SYM(_ZTIDF32x), SIZE2
_ZTIDF32x:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF32x)
	.weak	SYM(_ZTSPKDF32x)
	.hidden	SYM(_ZTSPKDF32x)
	SECTION3(_ZTSPKDF32x)
	ALIGN3
	.type	SYM(_ZTSPKDF32x), OBJECT
	.size	SYM(_ZTSPKDF32x), 8
_ZTSPKDF32x:
	STRING	"PKDF32x"
	.weak	SYM(_ZTSPDF32x)
	.hidden	SYM(_ZTSPDF32x)
	SECTION3(_ZTSPDF32x)
	ALIGN3
	.type	SYM(_ZTSPDF32x), OBJECT
	.size	SYM(_ZTSPDF32x), 7
_ZTSPDF32x:
	STRING	"PDF32x"
	.weak	SYM(_ZTSDF32x)
	.hidden	SYM(_ZTSDF32x)
	SECTION3(_ZTSDF32x)
	ALIGN3
	.type	SYM(_ZTSDF32x), OBJECT
	.size	SYM(_ZTSDF32x), 6
_ZTSDF32x:
	STRING	"DF32x"
	.weak	SYM(_ZTIPKDF64_)
	SECTION1(_ZTIPKDF64_)
	ALIGN1
	.type	SYM(_ZTIPKDF64_), OBJECT
	.size	SYM(_ZTIPKDF64_), SIZE1
_ZTIPKDF64_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF64_)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF64_)
	.weak	SYM(_ZTIPDF64_)
	SECTION1(_ZTIPDF64_)
	ALIGN1
	.type	SYM(_ZTIPDF64_), OBJECT
	.size	SYM(_ZTIPDF64_), SIZE1
_ZTIPDF64_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF64_)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF64_)
	.weak	SYM(_ZTIDF64_)
	SECTION2(_ZTIDF64_)
	ALIGN2
	.type	SYM(_ZTIDF64_), OBJECT
	.size	SYM(_ZTIDF64_), SIZE2
_ZTIDF64_:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF64_)
	.weak	SYM(_ZTSPKDF64_)
	.hidden	SYM(_ZTSPKDF64_)
	SECTION3(_ZTSPKDF64_)
	ALIGN3
	.type	SYM(_ZTSPKDF64_), OBJECT
	.size	SYM(_ZTSPKDF64_), 8
_ZTSPKDF64_:
	STRING	"PKDF64_"
	.weak	SYM(_ZTSPDF64_)
	.hidden	SYM(_ZTSPDF64_)
	SECTION3(_ZTSPDF64_)
	ALIGN3
	.type	SYM(_ZTSPDF64_), OBJECT
	.size	SYM(_ZTSPDF64_), 7
_ZTSPDF64_:
	STRING	"PDF64_"
	.weak	SYM(_ZTSDF64_)
	.hidden	SYM(_ZTSDF64_)
	SECTION3(_ZTSDF64_)
	ALIGN3
	.type	SYM(_ZTSDF64_), OBJECT
	.size	SYM(_ZTSDF64_), 6
_ZTSDF64_:
	STRING	"DF64_"
	.weak	SYM(_ZTIPKDF64x)
	SECTION1(_ZTIPKDF64x)
	ALIGN1
	.type	SYM(_ZTIPKDF64x), OBJECT
	.size	SYM(_ZTIPKDF64x), SIZE1
_ZTIPKDF64x:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF64x)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF64x)
	.weak	SYM(_ZTIPDF64x)
	SECTION1(_ZTIPDF64x)
	ALIGN1
	.type	SYM(_ZTIPDF64x), OBJECT
	.size	SYM(_ZTIPDF64x), SIZE1
_ZTIPDF64x:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF64x)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF64x)
	.weak	SYM(_ZTIDF64x)
	SECTION2(_ZTIDF64x)
	ALIGN2
	.type	SYM(_ZTIDF64x), OBJECT
	.size	SYM(_ZTIDF64x), SIZE2
_ZTIDF64x:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF64x)
	.weak	SYM(_ZTSPKDF64x)
	.hidden	SYM(_ZTSPKDF64x)
	SECTION3(_ZTSPKDF64x)
	ALIGN3
	.type	SYM(_ZTSPKDF64x), OBJECT
	.size	SYM(_ZTSPKDF64x), 8
_ZTSPKDF64x:
	STRING	"PKDF64x"
	.weak	SYM(_ZTSPDF64x)
	.hidden	SYM(_ZTSPDF64x)
	SECTION3(_ZTSPDF64x)
	ALIGN3
	.type	SYM(_ZTSPDF64x), OBJECT
	.size	SYM(_ZTSPDF64x), 7
_ZTSPDF64x:
	STRING	"PDF64x"
	.weak	SYM(_ZTSDF64x)
	.hidden	SYM(_ZTSDF64x)
	SECTION3(_ZTSDF64x)
	ALIGN3
	.type	SYM(_ZTSDF64x), OBJECT
	.size	SYM(_ZTSDF64x), 6
_ZTSDF64x:
	STRING	"DF64x"
	.weak	SYM(_ZTIPKDF128_)
	SECTION1(_ZTIPKDF128_)
	ALIGN1
	.type	SYM(_ZTIPKDF128_), OBJECT
	.size	SYM(_ZTIPKDF128_), SIZE1
_ZTIPKDF128_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPKDF128_)
	FLAGS	1
	PAD
	POINTER	SYM(_ZTIDF128_)
	.weak	SYM(_ZTIPDF128_)
	SECTION1(_ZTIPDF128_)
	ALIGN1
	.type	SYM(_ZTIPDF128_), OBJECT
	.size	SYM(_ZTIPDF128_), SIZE1
_ZTIPDF128_:
	POINTER	SYM(_ZTVN10__cxxabiv119__pointer_type_infoE)+OFF
	POINTER	SYM(_ZTSPDF128_)
	FLAGS	0
	PAD
	POINTER	SYM(_ZTIDF128_)
	.weak	SYM(_ZTIDF128_)
	SECTION2(_ZTIDF128_)
	ALIGN2
	.type	SYM(_ZTIDF128_), OBJECT
	.size	SYM(_ZTIDF128_), SIZE2
_ZTIDF128_:
	POINTER	SYM(_ZTVN10__cxxabiv123__fundamental_type_infoE)+OFF
	POINTER	SYM(_ZTSDF128_)
	.weak	SYM(_ZTSPKDF128_)
	.hidden	SYM(_ZTSPKDF128_)
	SECTION3(_ZTSPKDF128_)
	ALIGN3
	.type	SYM(_ZTSPKDF128_), OBJECT
	.size	SYM(_ZTSPKDF128_), 9
_ZTSPKDF128_:
	STRING	"PKDF128_"
	.weak	SYM(_ZTSPDF128_)
	.hidden	SYM(_ZTSPDF128_)
	SECTION3(_ZTSPDF128_)
	ALIGN3
	.type	SYM(_ZTSPDF128_), OBJECT
	.size	SYM(_ZTSPDF128_), 8
_ZTSPDF128_:
	STRING	"PDF128_"
	.weak	SYM(_ZTSDF128_)
	.hidden	SYM(_ZTSDF128_)
	SECTION3(_ZTSDF128_)
	ALIGN3
	.type	SYM(_ZTSDF128_), OBJECT
	.size	SYM(_ZTSDF128_), 7
_ZTSDF128_:
	STRING	"DF128_"
	.section	.note.GNU-stack,"",@progbits
