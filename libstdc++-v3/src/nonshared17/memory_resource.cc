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

#include "../c++17/memory_resource.cc"
asm (".hidden _ZNKSt3pmr26synchronized_pool_resource11do_is_equalERKNS_15memory_resourceE");
asm (".hidden _ZNKSt3pmr28unsynchronized_pool_resource11do_is_equalERKNS_15memory_resourceE");
asm (".hidden _ZNSt3pmr15__pool_resource14_M_alloc_poolsEv");
asm (".hidden _ZNSt3pmr15__pool_resource7releaseEv");
asm (".hidden _ZNSt3pmr15__pool_resourceC1ERKNS_12pool_optionsEPNS_15memory_resourceE");
#ifndef __riscv
asm (".hidden _ZNSt3pmr15__pool_resourceC2ERKNS_12pool_optionsEPNS_15memory_resourceE");
#endif
asm (".hidden _ZNSt3pmr15__pool_resourceD1Ev");
#ifndef __riscv
asm (".hidden _ZNSt3pmr15__pool_resourceD2Ev");
#endif
asm (".hidden _ZNSt3pmr26synchronized_pool_resource15_M_alloc_tpoolsERSt10lock_guardISt12shared_mutexE");
asm (".hidden _ZNSt3pmr26synchronized_pool_resource22_M_alloc_shared_tpoolsERSt10lock_guardISt12shared_mutexE");
asm (".hidden _ZNSt3pmr26synchronized_pool_resource24_M_thread_specific_poolsEv");
asm (".hidden _ZNSt3pmr26synchronized_pool_resource7_TPoolsD1Ev");
#ifndef __riscv
asm (".hidden _ZNSt3pmr26synchronized_pool_resource7_TPoolsD2Ev");
#endif
asm (".hidden _ZNSt3pmr26synchronized_pool_resourceD0Ev");
asm (".hidden _ZNSt3pmr28unsynchronized_pool_resourceD0Ev");
#ifndef __riscv
asm (".hidden _ZTSNSt3pmr26synchronized_pool_resourceE");
asm (".hidden _ZTSNSt3pmr28unsynchronized_pool_resourceE");
asm (".hidden _ZTVNSt3pmr26synchronized_pool_resourceE");
asm (".hidden _ZTVNSt3pmr28unsynchronized_pool_resourceE");
#endif
asm (".hidden _ZNKSt3pmr25monotonic_buffer_resource11do_is_equalERKNS_15memory_resourceE");
asm (".hidden _ZNSt3pmr15__pool_resource5_Pool12try_allocateEv");
asm (".hidden _ZNSt3pmr15__pool_resource5_Pool9replenishEPNS_15memory_resourceERKNS_12pool_optionsE");
#ifndef __i386__
asm (".hidden _ZNSt3pmr15__pool_resource10deallocateEPvmm");
asm (".hidden _ZNSt3pmr15__pool_resource8allocateEmm");
asm (".hidden _ZNSt3pmr25monotonic_buffer_resource11do_allocateEmm");
asm (".hidden _ZNSt3pmr25monotonic_buffer_resource13do_deallocateEPvmm");
asm (".hidden _ZNSt3pmr28unsynchronized_pool_resource12_M_find_poolEm");
//asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_insertIIRmS7_EEEvN9__gnu_cxx17__normal_iteratorIPS2_S5_EEDpOT_");
//asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_insertIJRmS7_EEEvN9__gnu_cxx17__normal_iteratorIPS2_S5_EEDpOT_");
#endif
#ifdef __i386__
asm (".hidden _ZNSt3pmr15__pool_resource10deallocateEPvjj");
//asm (".hidden _ZNSt3pmr15__pool_resource5_Pool10deallocateEPNS_15memory_resourceEPv");
asm (".hidden _ZNSt3pmr15__pool_resource8allocateEjj");
asm (".hidden _ZNSt3pmr25monotonic_buffer_resource11do_allocateEjj");
asm (".hidden _ZNSt3pmr25monotonic_buffer_resource13do_deallocateEPvjj");
asm (".hidden _ZNSt3pmr28unsynchronized_pool_resource12_M_find_poolEj");
//asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_insertIIRjS7_EEEvN9__gnu_cxx17__normal_iteratorIPS2_S5_EEDpOT_");
//asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_insertIJRjS7_EEEvN9__gnu_cxx17__normal_iteratorIPS2_S5_EEDpOT_");
#endif
#ifdef __powerpc64__
//asm (".hidden _ZNSt3pmr15__pool_resource5_Pool10deallocateEPNS_15memory_resourceEPv");
#endif
asm (".hidden _ZNSt22__shared_mutex_pthread6unlockEv");
#if defined(__i386__) || (defined(__powerpc__) && !defined(__powerpc64__))
asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_appendIIRjS7_EEEvDpOT_");
asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_appendIJRjS7_EEEvDpOT_");
#else
asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_appendIIRmS7_EEEvDpOT_");
#ifndef __riscv
asm (".hidden _ZNSt6vectorINSt3pmr15__pool_resource9_BigBlockENS0_21polymorphic_allocatorIS2_EEE17_M_realloc_appendIJRmS7_EEEvDpOT_");
#endif
#endif
