// Copyright (C) 2003-2023 Free Software Foundation, Inc.
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

#include <bits/move.h>
#include <bits/stl_iterator_base_types.h>

#include <debug/debug.h>
#include <debug/safe_sequence.h>
#include <debug/safe_unordered_container.h>
#include <debug/safe_iterator.h>
#include <debug/safe_local_iterator.h>
#include <algorithm>
#include <cassert>
#include <cstring>
#include <cctype>
#include <cstdio>
#include <cstdlib>

namespace std
{
  [[__noreturn__]]
  void
  __glibcxx_assert_fail(const char* file, int line,
                        const char* function, const char* condition) noexcept
  {
    if (file && function && condition)
      fprintf(stderr, "%s:%d: %s: Assertion '%s' failed.\n",
              file, line, function, condition);
    else if (function)
      fprintf(stderr, "%s: Undefined behavior detected.\n", function);
    abort();
  }
}

#ifndef _GLIBCXX_NONSHARED_CXX11_110
using namespace std;

namespace __gnu_debug
{
  void
  _Safe_iterator_base::
  _M_reset() throw ()
  {
    _M_sequence = 0;
    _M_version = 0;
    _M_prior = 0;
    _M_next = 0;
  }

  _Safe_unordered_container_base*
  _Safe_local_iterator_base::
  _M_get_container() const _GLIBCXX_NOEXCEPT
  { return static_cast<_Safe_unordered_container_base*>(_M_sequence); }

  void
  _Safe_local_iterator_base::
  _M_attach_single(_Safe_sequence_base* __cont, bool __constant) throw ()
  {
    _M_detach_single();

    // Attach to the new container (if there is one)
    if (__cont)
      {
	_M_sequence = __cont;
	_M_version = _M_sequence->_M_version;
	_M_get_container()->_M_attach_local_single(this, __constant);
      }
  }

  void
  _Safe_local_iterator_base::
  _M_detach_single() throw ()
  {
    if (_M_sequence)
      {
	_M_get_container()->_M_detach_local_single(this);
	_M_reset();
      }
  }

  void
  _Safe_unordered_container_base::
  _M_attach_local_single(_Safe_iterator_base* __it, bool __constant) throw ()
  {
    _Safe_iterator_base*& __its =
      __constant ? _M_const_local_iterators : _M_local_iterators;
    __it->_M_next = __its;
    if (__it->_M_next)
      __it->_M_next->_M_prior = __it;
    __its = __it;
  }

  void
  _Safe_unordered_container_base::
  _M_detach_local_single(_Safe_iterator_base* __it) throw ()
  {
    // Remove __it from this container's list
    __it->_M_unlink();
    if (_M_const_local_iterators == __it)
      _M_const_local_iterators = __it->_M_next;
    if (_M_local_iterators == __it)
      _M_local_iterators = __it->_M_next;
  }

} // namespace __gnu_debug

asm (".hidden _ZN11__gnu_debug19_Safe_iterator_base8_M_resetEv");
asm (".hidden _ZN11__gnu_debug25_Safe_local_iterator_base16_M_detach_singleEv");
asm (".hidden _ZN11__gnu_debug30_Safe_unordered_container_base22_M_attach_local_singleEPNS_19_Safe_iterator_baseEb");
asm (".hidden _ZN11__gnu_debug30_Safe_unordered_container_base22_M_detach_local_singleEPNS_19_Safe_iterator_baseE");
asm (".hidden _ZNK11__gnu_debug25_Safe_local_iterator_base16_M_get_containerEv");
#endif
