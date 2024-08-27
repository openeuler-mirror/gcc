// Explicit instantiation file.

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
// ISO C++ 14882:
//

#include <istream>
#include <iomanip>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef _GLIBCXX_USE_WCHAR_T
  template void __istream_extract(wistream&, wchar_t*, streamsize);
#endif

    void
    __istream_extract(istream& __in, char* __s, streamsize __num)
    {
      typedef basic_istream<char>       	__istream_type;
      typedef __istream_type::int_type		__int_type;
      typedef __istream_type::char_type		__char_type;
      typedef __istream_type::traits_type	__traits_type;
      typedef __istream_type::__streambuf_type  __streambuf_type;
      typedef __istream_type::__ctype_type	__ctype_type;

      streamsize __extracted = 0;
      ios_base::iostate __err = ios_base::goodbit;
      __istream_type::sentry __cerb(__in, false);
      if (__cerb)
	{
	  __try
	    {
	      // Figure out how many characters to extract.
	      streamsize __width = __in.width();
	      if (0 < __width && __width < __num)
		__num = __width;

	      const __ctype_type& __ct = use_facet<__ctype_type>(__in.getloc());

	      const __int_type __eof = __traits_type::eof();
	      __streambuf_type* __sb = __in.rdbuf();
	      __int_type __c = __sb->sgetc();

	      while (__extracted < __num - 1
		     && !__traits_type::eq_int_type(__c, __eof)
		     && !__ct.is(ctype_base::space,
				 __traits_type::to_char_type(__c)))
		{
		  streamsize __size = std::min(streamsize(__sb->egptr()
							  - __sb->gptr()),
					       streamsize(__num - __extracted
							  - 1));
		  if (__size > 1)
		    {
		      __size = (__ct.scan_is(ctype_base::space,
					     __sb->gptr() + 1,
					     __sb->gptr() + __size)
				- __sb->gptr());
		      __traits_type::copy(__s, __sb->gptr(), __size);
		      __s += __size;
		      __sb->__safe_gbump(__size);
		      __extracted += __size;
		      __c = __sb->sgetc();
		    }
		  else
		    {
		      *__s++ = __traits_type::to_char_type(__c);
		      ++__extracted;
		      __c = __sb->snextc();
		    }
		}

	      if (__extracted < __num - 1
		  && __traits_type::eq_int_type(__c, __eof))
		__err |= ios_base::eofbit;

	      // _GLIBCXX_RESOLVE_LIB_DEFECTS
	      // 68.  Extractors for char* should store null at end
	      *__s = __char_type();
	      __in.width(0);
	    }
	  __catch(__cxxabiv1::__forced_unwind&)
	    {
	      __in._M_setstate(ios_base::badbit);
	      __throw_exception_again;
	    }
	  __catch(...)
	    { __in._M_setstate(ios_base::badbit); }
	}
      if (!__extracted)
	__err |= ios_base::failbit;
      if (__err)
	__in.setstate(__err);
    }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
