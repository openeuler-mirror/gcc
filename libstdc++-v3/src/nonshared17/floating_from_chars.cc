// Copyright (C) 2019-2024 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include "../c++17/floating_from_chars.cc"
#if defined(__i386__) || (defined(__powerpc__) && !defined(__powerpc64__))
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE9_M_createERjj");
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE9_M_mutateEjjPKcj");
#else
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE9_M_createERmm");
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE9_M_mutateEmmPKcm");
#endif
#if defined(__s390x__) || defined(__powerpc64__)
//asm (".hidden _ZSt10from_charsPKcS0_RgSt12chars_format");
#endif
#if !defined(__i386__)
//asm (".hidden _ZSt10from_charsIiENSt9enable_ifIXsrSt5__or_IJS1_IJSt7is_sameINSt9remove_cvIT_E4typeEaES2_IS6_sES2_IS6_iES2_IS6_lES2_IS6_xES2_IS6_nEEES1_IJS2_IS6_hES2_IS6_tES2_IS6_jES2_IS6_mES2_IS6_yES2_IS6_oEEES2_IcS6_EEE5valueESt17from_chars_resultE4typeEPKcSR_RS4_i");
//asm (".hidden _ZSt10from_charsIiENSt9enable_ifIXsrSt5__or_IIS1_IISt7is_sameINSt9remove_cvIT_E4typeEaES2_IS6_sES2_IS6_iES2_IS6_lES2_IS6_xES2_IS6_nEEES1_IIS2_IS6_hES2_IS6_tES2_IS6_jES2_IS6_mES2_IS6_yES2_IS6_oEEES2_IcS6_EEE5valueESt17from_chars_resultE4typeEPKcSR_RS4_i");
#endif
//asm (".hidden _ZZNSt8__detail25__from_chars_alnum_to_valILb0EEEhhE7__table");
#ifdef __i386__
//asm (".hidden _ZSt10from_charsIiENSt9enable_ifIXsrSt5__or_IIS1_IISt7is_sameINSt9remove_cvIT_E4typeEaES2_IS6_sES2_IS6_iES2_IS6_lES2_IS6_xEEES1_IIS2_IS6_hES2_IS6_tES2_IS6_jES2_IS6_mES2_IS6_yEEES2_IcS6_EEE5valueESt17from_chars_resultE4typeEPKcSP_RS4_i");
//asm (".hidden _ZSt10from_charsIiENSt9enable_ifIXsrSt5__or_IJS1_IJSt7is_sameINSt9remove_cvIT_E4typeEaES2_IS6_sES2_IS6_iES2_IS6_lES2_IS6_xEEES1_IJS2_IS6_hES2_IS6_tES2_IS6_jES2_IS6_mES2_IS6_yEEES2_IcS6_EEE5valueESt17from_chars_resultE4typeEPKcSP_RS4_i");
#endif
#if defined(__i386__) || (defined(__powerpc__) && !defined(__powerpc64__))
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE15_M_replace_coldEPcjPKcjj");
#else
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcENSt3pmr21polymorphic_allocatorIcEEE15_M_replace_coldEPcmPKcmm");
#endif
#ifndef __riscv
asm (".hidden _ZNSt8__detail31__from_chars_alnum_to_val_tableILb0EE5valueE");
#endif
asm (".hidden _ZSt10from_charsIiLi0EESt17from_chars_resultPKcS2_RT_i");
