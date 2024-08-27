// Wrapper of C-language FILE struct -*- C++ -*-

// Copyright (C) 2000-2024 Free Software Foundation, Inc.
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
// ISO C++ 14882: 27.8  File-based streams
//

#include <bits/largefile-config.h>
#include <bits/basic_file.h>
#include <fcntl.h>
#include <errno.h>

#ifdef _GLIBCXX_HAVE_POLL
#include <poll.h>
#endif

// Pick up ioctl on Solaris 2.8
#ifdef _GLIBCXX_HAVE_UNISTD_H
#include <unistd.h>
#endif

// Pick up FIONREAD on Solaris 2
#ifdef _GLIBCXX_HAVE_SYS_IOCTL_H
#define BSD_COMP
#include <sys/ioctl.h>
#endif

// Pick up FIONREAD on Solaris 2.5.
#ifdef _GLIBCXX_HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifdef _GLIBCXX_HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif

#if _GLIBCXX_USE__GET_OSFHANDLE
# include <stdint.h> // For intptr_t
# include <io.h>     // For _get_osfhandle
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  __basic_file<char>::native_handle_type
  __basic_file<char>::native_handle() const noexcept
  {
#ifdef _GLIBCXX_USE_STDIO_PURE
    return _M_cfile;
#elif _GLIBCXX_USE__GET_OSFHANDLE
    const intptr_t handle = _M_cfile ? _get_osfhandle(fileno(_M_cfile)) : -1;
    return reinterpret_cast<native_handle_type>(handle);
#else
    return fileno(_M_cfile);
#endif
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

