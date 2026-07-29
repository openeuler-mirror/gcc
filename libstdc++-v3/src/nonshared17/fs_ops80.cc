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

#include "../c++17/fs_ops.cc"
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEED0Ev");
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEED1Ev");
#ifndef __riscv
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEED2Ev");
#endif
asm (".hidden _ZNSt10filesystem12do_copy_fileEPKcS1_NS_26copy_options_existing_fileEP4statS4_RSt10error_code");
#ifndef __riscv
asm (".hidden _ZNSt15_Sp_counted_ptrIDnLN9__gnu_cxx12_Lock_policyE2EE10_M_disposeEv");
asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE10_M_destroyEv");
asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE10_M_releaseEv");
#endif
//asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE16_M_push_back_auxIJRKS2_EEEvDpOT_");
//asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE16_M_push_back_auxIIRKS2_EEEvDpOT_");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EED1Ev");
#ifndef __riscv
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EED2Ev");
asm (".hidden _ZTIN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEE");
asm (".hidden _ZTSN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEE");
asm (".hidden _ZTVN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEE");
#endif
asm (".hidden _ZNKSt10filesystem7__cxx114path8filenameEv");
asm (".hidden _ZNSt10unique_ptrINSt10filesystem7__cxx114path5_List5_ImplENS3_13_Impl_deleterEED1Ev");
#ifndef __riscv
asm (".hidden _ZNSt10unique_ptrINSt10filesystem7__cxx114path5_List5_ImplENS3_13_Impl_deleterEED2Ev");
#endif
asm (".hidden _ZSt14__copy_move_a1ILb1EPNSt10filesystem7__cxx114pathES2_EN9__gnu_cxx11__enable_ifIXsrSt23__is_random_access_iterIT0_NSt15iterator_traitsIS7_E17iterator_categoryEE7__valueESt15_Deque_iteratorIT1_RSD_PSD_EE6__typeES7_S7_SG_");
asm (".hidden _ZSt23__copy_move_backward_a1ILb1EPNSt10filesystem7__cxx114pathES2_EN9__gnu_cxx11__enable_ifIXsrSt23__is_random_access_iterIT0_NSt15iterator_traitsIS7_E17iterator_categoryEE7__valueESt15_Deque_iteratorIT1_RSD_PSD_EE6__typeES7_S7_SG_");
//asm (".hidden _ZSt8_DestroyISt15_Deque_iteratorINSt10filesystem7__cxx114pathERS3_PS3_EEvT_S7_");
#ifndef __i386__
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEC1EiSt13_Ios_Openmodem");
#ifndef __riscv
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEC2EiSt13_Ios_Openmodem");
#endif
asm (".hidden _ZNSt10filesystem8do_spaceEPKcRmS2_S2_RSt10error_code");
asm (".hidden _ZNSt11_Deque_baseINSt10filesystem7__cxx114pathESaIS2_EE17_M_initialize_mapEm");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE13_M_insert_auxINS2_8iteratorEEEvSt15_Deque_iteratorIS2_RS2_PS2_ET_SB_m");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE17_M_reallocate_mapEmb");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE23_M_new_elements_at_backEm");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE24_M_new_elements_at_frontEm");
#if !defined(__powerpc64__) && !defined(__s390x__)
//asm (".hidden _ZSt13move_backwardISt15_Deque_iteratorINSt10filesystem7__cxx114pathERS3_PS3_ES6_ET0_T_S8_S7_");
//asm (".hidden _ZSt4moveISt15_Deque_iteratorINSt10filesystem7__cxx114pathERS3_PS3_ES6_ET0_T_S8_S7_");
#endif
#endif
#if defined(__s390x__) || defined(__powerpc64__)
//asm (".hidden _ZSt13__copy_move_aILb0ENSt10filesystem7__cxx114path8iteratorESt15_Deque_iteratorIS2_RS2_PS2_EET1_T0_S9_S8_");
#endif
#ifdef __i386__
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEC1EiSt13_Ios_Openmodej");
asm (".hidden _ZN9__gnu_cxx13stdio_filebufIcSt11char_traitsIcEEC2EiSt13_Ios_Openmodej");
asm (".hidden _ZNSt10filesystem8do_spaceEPKcRyS2_S2_RSt10error_code");
asm (".hidden _ZNSt11_Deque_baseINSt10filesystem7__cxx114pathESaIS2_EE17_M_initialize_mapEj");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE13_M_insert_auxINS2_8iteratorEEEvSt15_Deque_iteratorIS2_RS2_PS2_ET_SB_j");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE17_M_reallocate_mapEjb");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE23_M_new_elements_at_backEj");
asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE24_M_new_elements_at_frontEj");
#endif
#ifdef __aarch64__
//asm (".hidden _ZSt4copyINSt10filesystem7__cxx114path8iteratorESt15_Deque_iteratorIS2_RS2_PS2_EET0_T_S9_S8_");
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE4swapERS4_");
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE6resizeEmc");
#endif
//asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE12emplace_backIIS2_EEERS2_DpOT_");
//asm (".hidden _ZNSt5dequeINSt10filesystem7__cxx114pathESaIS2_EE12emplace_backIJS2_EEERS2_DpOT_");
#ifdef __powerpc64__
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE4swapERS4_");
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE6resizeEmc");
#endif
#ifndef __i386__
//asm (".hidden _ZNSt16_Sp_counted_baseILN9__gnu_cxx12_Lock_policyE2EE24_M_release_last_use_coldEv");
#endif
#if defined(__s390x__)
asm (".hidden _ZSt16__do_uninit_copyINSt10filesystem7__cxx114path8iteratorESt15_Deque_iteratorIS2_RS2_PS2_EET0_T_S9_S8_");
//asm (".hidden _ZSt4copyINSt10filesystem7__cxx114path8iteratorESt15_Deque_iteratorIS2_RS2_PS2_EET0_T_S9_S8_");
#endif
#if defined(__x86_64__) || defined(__s390x__)
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE4swapERS4_");
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE6resizeEmc");
#endif
#ifdef __i386__
asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE4swapERS4_");
//asm (".hidden _ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE6resizeEjc");
//asm (".hidden _ZSt13move_backwardISt15_Deque_iteratorINSt10filesystem7__cxx114pathERS3_PS3_ES6_ET0_T_S8_S7_");
//asm (".hidden _ZSt4moveISt15_Deque_iteratorINSt10filesystem7__cxx114pathERS3_PS3_ES6_ET0_T_S8_S7_");
#endif
#if defined(__i386__) || (defined(__powerpc__) && !defined(__powerpc64__))
asm (".hidden _ZNSt10filesystem18copy_file_sendfileEiij");
#else
asm (".hidden _ZNSt10filesystem18copy_file_sendfileEiim");
#endif
