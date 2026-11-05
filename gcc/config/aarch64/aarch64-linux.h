/* Machine description for AArch64 architecture.
   Copyright (C) 2009-2024 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_AARCH64_LINUX_H
#define GCC_AARCH64_LINUX_H

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux-aarch64%{mbig-endian:_be}%{mabi=ilp32:_ilp32}.so.1"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER "/lib/ld-musl-aarch64%{mbig-endian:_be}%{mabi=ilp32:_ilp32}.so.1"

#undef  ASAN_CC1_SPEC
#define ASAN_CC1_SPEC "%{%:sanitize(address):-funwind-tables}"

#undef  CC1_SPEC
#define CC1_SPEC GNU_USER_TARGET_CC1_SPEC ASAN_CC1_SPEC

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#define LINUX_TARGET_LINK_SPEC  "%{h*}		\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{!static:%{!static-pie:			\
     %{rdynamic:-export-dynamic}		\
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}} \
   %{static-pie:-Bstatic -pie --no-dynamic-linker -z text} \
   -X						\
   %{mbig-endian:-EB} %{mlittle-endian:-EL}     \
   -maarch64linux%{mabi=ilp32:32}%{mbig-endian:b}"


#define LINK_SPEC LINUX_TARGET_LINK_SPEC AARCH64_ERRATA_LINK_SPEC

/* -fsimdmath vectorizes math calls into _ZGV* variants implemented by
   libmathlib (the packaged Arm optimized-routines).  Close that contract
   at link time: inject the library after the user's objects, with the
   policy selected by OPENEULER_GCC_SIMDMATH_LINK (see simdmath-link
   in gcc.cc).  */
#undef  LIB_SPEC
#define LIB_SPEC \
  "%{fsimdmath:%:simdmath-link()} " GNU_USER_TARGET_LIB_SPEC

/* crtfastmath.o sets FZ and DZ in the FPCR before main runs, so denormals
   are flushed for the whole program - a value change no amount of careful
   code generation survives.  Precedence, highest first:

     -mno-daz-ftz        never link it.  This and -mdaz-ftz are two
			 spellings of one option, so between the two of them
			 the last on the command line wins; what is meant here
			 is that a -mno-daz-ftz still in force outranks every
			 other entry below
     -shared             never, for a shared object
     -mdaz-ftz           link it, whatever model follows.  It exists so
			 that flush-to-zero can be asked for without the
			 aggressive floating-point optimizations it is
			 otherwise bundled with, which is exactly this
			 combination; Intel's -fp-model treats an explicit
			 -ftz the same way
     precise|except|strict   do not link it - a program that asked for IEEE
			 semantics must not have FTZ arrive implicitly, say
			 through a -funsafe-math-optimizations that a
			 project's CFLAGS carried in
     the rest           link it

   Deliberately confined to this file.  -ffp-model= is a common option and
   the same reasoning holds for every target that links crtfastmath.o, but
   openEuler ships and tests AArch64; a spec that cannot be exercised is a
   spec that will be wrong, and twice already an attempt to cover the other
   targets shipped an asymmetry that only a cross driver found.  The
   consequence is stated in invoke.texi: elsewhere the model does not reach
   the startup file, so -funsafe-math-optimizations -ffp-model=strict still
   flushes denormals there.  -mdaz-ftz is AArch64-only here, as the
   feature has been since it was written; x86 spells the same thing the
   same way, which is where the name comes from.  */
#define AARCH64_KEEP_DENORMALS_SPEC(CRTFASTMATH) \
  "%{!ffp-model=precise:%{!ffp-model=except:%{!ffp-model=strict:" \
  CRTFASTMATH "}}}"

#define GNU_USER_TARGET_MATHFILE_SPEC \
  "%{!shared:%{!mno-daz-ftz:%{mdaz-ftz:crtfastmath.o%s;\
     Ofast|ffast-math|funsafe-math-optimizations|ffp-model=fast:" \
     AARCH64_KEEP_DENORMALS_SPEC ("crtfastmath.o%s") "}}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC   \
  GNU_USER_TARGET_MATHFILE_SPEC " " \
  GNU_USER_TARGET_ENDFILE_SPEC

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
	GNU_USER_TARGET_OS_CPP_BUILTINS();	\
    }						\
  while (0)

#define TARGET_ASM_FILE_END aarch64_file_end_indicate_exec_stack

/* Uninitialized common symbols in non-PIE executables, even with
   strong definitions in dependent shared libraries, will resolve
   to COPY relocated symbol in the executable.  See PR65780.  */
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P default_binds_local_p_2

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#endif  /* GCC_AARCH64_LINUX_H */
