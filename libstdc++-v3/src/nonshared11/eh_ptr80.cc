// -*- C++ -*- Implement the members of exception_ptr.
// Copyright (C) 2008-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <bits/c++config.h>
#include "eh_atomics.h"

#define _GLIBCXX_EH_PTR_COMPAT

#include <exception>
#include <bits/exception_ptr.h>
#include "unwind-cxx.h"

using namespace __cxxabiv1;

void
std::__exception_ptr::exception_ptr::_M_addref() noexcept
{
  if (__builtin_expect(_M_exception_object != nullptr, true))
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      __gnu_cxx::__eh_atomic_inc (&eh->referenceCount);
    }
}

void
std::__exception_ptr::exception_ptr::_M_release() noexcept
{
  if (__builtin_expect(_M_exception_object != nullptr, true))
    {
      __cxa_refcounted_exception *eh =
	__get_refcounted_exception_header_from_obj (_M_exception_object);
      if (__gnu_cxx::__eh_atomic_dec (&eh->referenceCount))
        {
	  if (eh->exc.exceptionDestructor)
	    eh->exc.exceptionDestructor (_M_exception_object);

          __cxa_free_exception (_M_exception_object);
          _M_exception_object = nullptr;
        }
    }
}

