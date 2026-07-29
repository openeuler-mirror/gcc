// Locale support -*- C++ -*-

// Copyright (C) 1999-2023 Free Software Foundation, Inc.
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
// ISO C++ 14882: 22.1  Locales
//

#ifndef _GLIBCXX_USE_CXX11_ABI
#define _GLIBCXX_USE_CXX11_ABI 1
#endif

#include <locale>

// Instantiation configuration.
#ifndef C
# define C char
# define C_is_char
#endif

#define INSTANTIATE_USE_FACET(...)			    \
  template const __VA_ARGS__*				    \
    __try_use_facet< __VA_ARGS__ >(const locale&) noexcept

#define INSTANTIATE_FACET_ACCESSORS(...)		    \
  INSTANTIATE_USE_FACET(__VA_ARGS__)

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  template time_get<C, istreambuf_iterator<C>>::iter_type time_get<C, istreambuf_iterator<C>>::_M_extract_via_format(iter_type, iter_type, ios_base&, ios_base::iostate&, tm*, const C*, __time_get_state&) const;

INSTANTIATE_FACET_ACCESSORS(collate<C>);
INSTANTIATE_FACET_ACCESSORS(numpunct<C>);
INSTANTIATE_FACET_ACCESSORS(moneypunct<C, false>);
INSTANTIATE_USE_FACET      (moneypunct<C, true>);
INSTANTIATE_FACET_ACCESSORS(time_get<C>);
INSTANTIATE_FACET_ACCESSORS(messages<C>);
INSTANTIATE_FACET_ACCESSORS(money_put<C>);
INSTANTIATE_FACET_ACCESSORS(money_get<C>);

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
#ifdef C_is_char
asm (".hidden _ZNKSt5ctypeIcE9do_narrowEcc");
asm (".hidden _ZNKSt5ctypeIcE8do_widenEPKcS2_Pc");
asm (".hidden _ZNKSt19istreambuf_iteratorIcSt11char_traitsIcEE6_M_getEv");
#endif
