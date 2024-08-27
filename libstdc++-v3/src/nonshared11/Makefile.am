## Makefile for the C++03 sources of the GNU C++ Standard library.
##
## Copyright (C) 1997-2024 Free Software Foundation, Inc.
##
## This file is part of the libstdc++ version 3 distribution.
## Process this file with automake to produce Makefile.in.

## This file is part of the GNU ISO C++ Library.  This library is free
## software; you can redistribute it and/or modify it under the
## terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3, or (at your option)
## any later version.

## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License along
## with this library; see the file COPYING3.  If not see
## <http://www.gnu.org/licenses/>.

include $(top_srcdir)/fragment.am

# Convenience library for C++11 runtime.
noinst_LTLIBRARIES = libnonshared11convenience80.la \
		     libnonshared11convenience110.la

headers =

sources80 = \
	limits.cc \
	functexcept80.cc \
	debug.cc \
	eh_ptr80.cc \
	futex80.cc \
	codecvt80.cc \
	cow-stdexcept80.cc \
	shared_ptr80.cc \
	cxx11-ios_failure80.cc \
	condition_variable80.cc \
	basic_file.cc \
	$(inst_sources80)

sources110 = \
	debug110.cc \
	condition_variable80.cc \
	basic_file.cc \
	$(inst_sources110)

if ENABLE_EXTERN_TEMPLATE
# XTEMPLATE_FLAGS = -fno-implicit-templates
inst_sources80 = \
	cow-sstream-inst80.cc \
	istream-inst80.cc \
	sstream-inst80.cc \
	cow-string-inst80.cc \
	cow-wstring-inst80.cc \
	locale-inst80.cc \
	wlocale-inst80.cc \
	string-inst80.cc \
	wstring-inst80.cc \
	cxx11-locale-inst80.cc \
	cxx11-wlocale-inst80.cc
inst_sources110 = \
	locale-inst110.cc \
	wlocale-inst110.cc \
	string-inst110.cc \
	wstring-inst110.cc \
	cxx11-locale-inst110.cc \
	cxx11-wlocale-inst110.cc
else
# XTEMPLATE_FLAGS =
inst_sources80 =
inst_sources110 =
endif

vpath % $(top_srcdir)/src/nonshared11
vpath % $(top_srcdir)

libnonshared11convenience80_la_SOURCES = $(sources80)
libnonshared11convenience110_la_SOURCES = $(sources110)

# AM_CXXFLAGS needs to be in each subdirectory so that it can be
# modified in a per-library or per-sub-library way.  Need to manually
# set this option because CONFIG_CXXFLAGS has to be after
# OPTIMIZE_CXXFLAGS on the compile line so that -O2 can be overridden
# as the occasion calls for it.
AM_CXXFLAGS = \
	-std=gnu++11 \
	$(glibcxx_lt_pic_flag) $(glibcxx_compiler_shared_flag) \
	$(XTEMPLATE_FLAGS) $(VTV_CXXFLAGS) \
	$(WARN_CXXFLAGS) $(OPTIMIZE_CXXFLAGS) $(CONFIG_CXXFLAGS)

AM_MAKEFLAGS = \
	"gxx_include_dir=$(gxx_include_dir)"

# Libtool notes

# 1) Need to explicitly set LTCXXCOMPILE so that AM_CXXFLAGS is
# last. (That way, things like -O2 passed down from the toplevel can
# be overridden by --enable-debug.)

# 2) In general, libtool expects an argument such as `--tag=CXX' when
# using the C++ compiler, because that will enable the settings
# detected when C++ support was being configured.  However, when no
# such flag is given in the command line, libtool attempts to figure
# it out by matching the compiler name in each configuration section
# against a prefix of the command line.  The problem is that, if the
# compiler name and its initial flags stored in the libtool
# configuration file don't match those in the command line, libtool
# can't decide which configuration to use, and it gives up.  The
# correct solution is to add `--tag CXX' to LTCXXCOMPILE and maybe
# CXXLINK, just after $(LIBTOOL), so that libtool doesn't have to
# attempt to infer which configuration to use.
# We have to put --tag disable-shared after --tag CXX lest things
# CXX undo the affect of disable-shared.
LTCXXCOMPILE = $(LIBTOOL) --tag CXX --tag disable-shared $(LIBTOOLFLAGS) \
	       --mode=compile $(CXX) $(TOPLEVEL_INCLUDES) \
	       $(AM_CPPFLAGS) $(CPPFLAGS) $(AM_CXXFLAGS) $(CXXFLAGS) $(EXTRA_CXX_FLAGS)

LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))

# 3) We'd have a problem when building the shared libstdc++ object if
# the rules automake generates would be used.  We cannot allow g++ to
# be used since this would add -lstdc++ to the link line which of
# course is problematic at this point.  So, we get the top-level
# directory to configure libstdc++-v3 to use gcc as the C++
# compilation driver.
CXXLINK = $(LIBTOOL) --tag CXX --tag disable-shared $(LIBTOOLFLAGS) \
	  --mode=link $(CXX) \
	  $(VTV_CXXLINKFLAGS) \
	  $(OPT_LDFLAGS) $(SECTION_LDFLAGS) $(AM_CXXFLAGS) $(LTLDFLAGS) -o $@

# Use special rules for source files that require -fchar8_t.
codecvt80.lo: codecvt80.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
limits.lo: limits.cc
	$(LTCXXCOMPILE) -fchar8_t -c $<
