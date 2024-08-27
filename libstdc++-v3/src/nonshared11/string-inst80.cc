// Components for manipulating sequences of characters -*- C++ -*-

// Copyright (C) 1997-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882: 21  Strings library
//

// Written by Jason Merrill based upon the specification by Takanori Adachi
// in ANSI X3J16/94-0013R2.  Rewritten by Nathan Myers.

#ifndef _GLIBCXX_USE_CXX11_ABI
// Instantiations in this file use the new SSO std::string ABI unless included
// by another file which defines _GLIBCXX_USE_CXX11_ABI=0.
# define _GLIBCXX_USE_CXX11_ABI 1
#endif

#include <string>

// Instantiation configuration.
#ifndef C
# define C char
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  typedef basic_string<C> S;

#ifndef _GLIBCXX_NONSHARED_CXX11_110
  template
    S::iterator
    S::insert(S::const_iterator, initializer_list<C>);

  template
  void S::reserve();
#endif

  template
  void S::_M_replace_cold(S::pointer, size_type, const C*, const size_type,
			  const size_type);

  template
  S::pointer S::_S_allocate(S::allocator_type &, size_type);

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
