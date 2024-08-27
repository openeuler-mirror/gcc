// Copyright (C) 2012-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882:
//

#define _GLIBCXX_USE_CXX11_ABI 0
#include <sstream>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template basic_stringbuf<char>::basic_stringbuf();
  template basic_istringstream<char>::basic_istringstream();
  template basic_ostringstream<char>::basic_ostringstream();
  template basic_stringstream<char>::basic_stringstream();
#ifdef _GLIBCXX_USE_WCHAR_T
  template basic_stringbuf<wchar_t>::basic_stringbuf();
  template basic_istringstream<wchar_t>::basic_istringstream();
  template basic_ostringstream<wchar_t>::basic_ostringstream();
  template basic_stringstream<wchar_t>::basic_stringstream();
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

asm (".hidden _ZNSt15basic_stringbufIcSt11char_traitsIcESaIcEED0Ev");
asm (".hidden _ZNSt15basic_stringbufIcSt11char_traitsIcESaIcEED1Ev");
asm (".hidden _ZNSt15basic_stringbufIcSt11char_traitsIcESaIcEED2Ev");
asm (".hidden _ZNSt15basic_stringbufIwSt11char_traitsIwESaIwEED0Ev");
asm (".hidden _ZNSt15basic_stringbufIwSt11char_traitsIwESaIwEED1Ev");
asm (".hidden _ZNSt15basic_stringbufIwSt11char_traitsIwESaIwEED2Ev");
