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

#include "../c++17/cow-fs_ops.cc"
asm (".hidden _ZNSt15_Sp_counted_ptrIDnLN9__gnu_cxx12_Lock_policyE2EE10_M_disposeEv");
asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE10_M_destroyEv");
asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE10_M_releaseEv");
//asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE16_M_push_back_auxIJRKS1_EEEvDpOT_");
//asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE16_M_push_back_auxIIRKS1_EEEvDpOT_");
//asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE12emplace_backIJS1_EEERS1_DpOT_");
//asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE12emplace_backIIS1_EEERS1_DpOT_");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EED1Ev");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EED2Ev");
asm (".hidden _ZNSt10unique_ptrINSt10filesystem4path5_List5_ImplENS2_13_Impl_deleterEED1Ev");
asm (".hidden _ZNSt10unique_ptrINSt10filesystem4path5_List5_ImplENS2_13_Impl_deleterEED2Ev");
asm (".hidden _ZSt14__copy_move_a1ILb1EPNSt10filesystem4pathES1_EN9__gnu_cxx11__enable_ifIXsrSt23__is_random_access_iterIT0_NSt15iterator_traitsIS6_E17iterator_categoryEE7__valueESt15_Deque_iteratorIT1_RSC_PSC_EE6__typeES6_S6_SF_");
asm (".hidden _ZSt23__copy_move_backward_a1ILb1EPNSt10filesystem4pathES1_EN9__gnu_cxx11__enable_ifIXsrSt23__is_random_access_iterIT0_NSt15iterator_traitsIS6_E17iterator_categoryEE7__valueESt15_Deque_iteratorIT1_RSC_PSC_EE6__typeES6_S6_SF_");
//asm (".hidden _ZSt8_DestroyISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_EEvT_S6_");
asm (".hidden _ZNSsC1ISaIcEEEPKcRKS0_");
asm (".hidden _ZNSsC2ISaIcEEEPKcRKS0_");
#ifndef __i386__
//asm (".hidden _ZNSs9_M_mutateEmmm");
asm (".hidden _ZNSt11_Deque_baseINSt10filesystem4pathESaIS1_EE17_M_initialize_mapEm");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE13_M_insert_auxINS1_8iteratorEEEvSt15_Deque_iteratorIS1_RS1_PS1_ET_SA_m");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE17_M_reallocate_mapEmb");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE23_M_new_elements_at_backEm");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE24_M_new_elements_at_frontEm");
//asm (".hidden _ZNSs6resizeEmc");
//asm (".hidden _ZNSt10filesystem4pathD1Ev");
//asm (".hidden _ZNSt10filesystem4pathD2Ev");
asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE24_M_release_last_use_coldEv");
#endif
#if defined(__x86_64__)
//asm (".hidden _ZSt13move_backwardISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
//asm (".hidden _ZSt4moveISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
#endif
#ifdef __i386__
asm (".hidden _ZNSt11_Deque_baseINSt10filesystem4pathESaIS1_EE17_M_initialize_mapEj");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE13_M_insert_auxINS1_8iteratorEEEvSt15_Deque_iteratorIS1_RS1_PS1_ET_SA_j");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE17_M_reallocate_mapEjb");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE23_M_new_elements_at_backEj");
asm (".hidden _ZNSt5dequeINSt10filesystem4pathESaIS1_EE24_M_new_elements_at_frontEj");
//asm (".hidden _ZNSs6resizeEjc");
//asm (".hidden _ZNSs9_M_mutateEjjj");
//asm (".hidden _ZSt13move_backwardISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
//asm (".hidden _ZSt4moveISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
#endif
#if defined(__s390x__)
asm (".hidden _ZSt16__do_uninit_copyINSt10filesystem4path8iteratorESt15_Deque_iteratorIS1_RS1_PS1_EET0_T_S8_S7_");
//asm (".hidden _ZSt4copyINSt10filesystem4path8iteratorESt15_Deque_iteratorIS1_RS1_PS1_EET0_T_S8_S7_");
#endif
#ifdef __aarch64__
//asm (".hidden _ZSt4copyINSt10filesystem4path8iteratorESt15_Deque_iteratorIS1_RS1_PS1_EET0_T_S8_S7_");
//asm (".hidden _ZSt13move_backwardISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
//asm (".hidden _ZSt4moveISt15_Deque_iteratorINSt10filesystem4pathERS2_PS2_ES5_ET0_T_S7_S6_");
#endif
asm (".hidden _ZNKSt10filesystem4path8filenameEv");
#ifdef __powerpc64__
//asm (".hidden _ZSt16__do_uninit_copyINSt10filesystem4path8iteratorESt15_Deque_iteratorIS1_RS1_PS1_EET0_T_S8_S7_");
//asm (".hidden _ZSt4copyINSt10filesystem4path8iteratorESt15_Deque_iteratorIS1_RS1_PS1_EET0_T_S8_S7_");
#endif
asm (".hidden _ZNSs4swapERSs");
