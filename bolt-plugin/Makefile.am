## Process this file with automake to produce Makefile.in.

ACLOCAL_AMFLAGS = -I .. -I ../config
AUTOMAKE_OPTIONS = no-dependencies

gcc_version := $(shell @get_gcc_base_ver@ $(top_srcdir)/../gcc/BASE-VER)
target_noncanonical := @target_noncanonical@
libexecsubdir := $(libexecdir)/gcc/$(real_target_noncanonical)/$(gcc_version)$(accel_dir_suffix)

AM_CPPFLAGS = -I$(top_srcdir)/../include $(DEFS) -std=c++11
AM_CXXFLAGS = @ac_bolt_plugin_warn_cflags@ -std=c++11
AM_LDFLAGS = @ac_bolt_plugin_ldflags@
AM_LIBTOOLFLAGS = --tag=disable-static
override CXXFLAGS := $(filter-out -fsanitize=address,$(CXXFLAGS))
override LDFLAGS := $(filter-out -fsanitize=address,$(LDFLAGS))

libexecsub_LTLIBRARIES = libbolt_plugin.la
gcc_build_dir = @gcc_build_dir@
in_gcc_libs = $(foreach lib, $(libexecsub_LTLIBRARIES), $(gcc_build_dir)/$(lib))

libbolt_plugin_la_SOURCES = bolt-plugin.cc
# Note that we intentionally override the bindir supplied by ACX_LT_HOST_FLAGS.
libbolt_plugin_la_LDFLAGS = $(AM_LDFLAGS) \
	$(lt_host_flags) -module -bindir $(libexecsubdir)
# Can be simplified when libiberty becomes a normal convenience library.
libiberty = $(with_libiberty)/libiberty.a
libiberty_noasan = $(with_libiberty)/noasan/libiberty.a
libiberty_pic = $(with_libiberty)/pic/libiberty.a
Wc=-Wc,
libbolt_plugin_la_LIBADD = \
	$(if $(wildcard $(libiberty_noasan)),$(Wc)$(libiberty_noasan), \
	$(if $(wildcard $(libiberty_pic)),$(Wc)$(libiberty_pic),))
libbolt_plugin_la_LDFLAGS += \
	$(if $(wildcard $(libiberty_noasan)),, \
	$(if $(wildcard $(libiberty_pic)),,-Wc,$(libiberty)))
libbolt_plugin_la_DEPENDENCIES = \
	$(if $(wildcard $(libiberty_noasan)),$(libiberty_noasan), \
	$(if $(wildcard $(libiberty_pic)),$(libiberty_pic),))
LTLDFLAGS = $(shell $(SHELL) $(top_srcdir)/../libtool-ldflags $(LDFLAGS))
libbolt_plugin_la_LINK = $(LIBTOOL) --tag=CXX $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(CXXLD) $(AM_CXXFLAGS) ${AM_CPPFLAGS} $(CXXFLAGS) \
	$(libbolt_plugin_la_LDFLAGS) $(LTLDFLAGS) -o $@
