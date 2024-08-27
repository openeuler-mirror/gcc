// Methods for Exception Support for -*- C++ -*-

// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

//
// ISO C++ 14882: 19.1  Exception classes
//

// All exception classes still use the classic COW std::string.
#define _GLIBCXX_USE_CXX11_ABI 0
#define _GLIBCXX_DEFINE_STDEXCEPT_COPY_OPS 1
#define __cow_string __cow_stringxxx
#include <stdexcept>
#include <system_error>
#undef __cow_string

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  // Copy/move constructors and assignment operators defined using COW string.
  // These operations are noexcept even though copying a COW string is not,
  // but we know that the string member in an exception has not been "leaked"
  // so copying is a simple reference count increment.
  // For the fully dynamic string moves are not noexcept (due to needing to
  // allocate an empty string) so we just define the moves as copies here.

#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
  logic_error::logic_error(logic_error&& e) noexcept = default;

  logic_error&
  logic_error::operator=(logic_error&& e) noexcept = default;
#else
  logic_error::logic_error(logic_error&& e) noexcept
  : exception(e), _M_msg(e._M_msg) { }

  logic_error&
  logic_error::operator=(logic_error&& e) noexcept
  { _M_msg = e._M_msg; return *this; }
#endif

#if _GLIBCXX_FULLY_DYNAMIC_STRING == 0
  runtime_error::runtime_error(runtime_error&& e) noexcept = default;

  runtime_error&
  runtime_error::operator=(runtime_error&& e) noexcept = default;
#else
  runtime_error::runtime_error(runtime_error&& e) noexcept
  : exception(e), _M_msg(e._M_msg) { }

  runtime_error&
  runtime_error::operator=(runtime_error&& e) noexcept
  { _M_msg = e._M_msg; return *this; }
#endif
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
