## Process this file with Automake to create Makefile.in
## Jim Blandy <jimb@red-bean.com> --- September 1997
##
##  	Copyright (C) 1998, 1999, 2001, 2006, 2007, 2008, 2009, 2011 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##   
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##   
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##   
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

bin_SCRIPTS = guile-config guild
EXTRA_DIST= \
  guile.m4 ChangeLog-2008			\
  guile-2.2.pc.in guile-2.2-uninstalled.pc.in	\
  guild.in guile-config.in

# What we now call `guild' used to be known as `guile-tools'.
install-data-hook:
	cd $(DESTDIR)$(bindir) && rm -f guile-tools$(EXEEXT) && \
	$(LN_S) guild$(EXEEXT) guile-tools$(EXEEXT)

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = guile-2.2.pc

## FIXME: in the future there will be direct automake support for
## doing this.  When that happens, switch over.
aclocaldir = $(datadir)/aclocal
aclocal_DATA = guile.m4

guile-config: $(srcdir)/guile-config.in $(top_builddir)/config.status
	guile="@bindir@/`echo guile | $(SED) -e '$(program_transform_name)'`" ;	\
	cat $(srcdir)/guile-config.in							\
	| $(SED) -e "s,@pkgconfigdir@,$(pkgconfigdir),g ;			\
		     s,@""PKG_CONFIG@,$(PKG_CONFIG),g ;				\
		     s,@installed_guile@,$$guile,g"				\
	> guile-config.out
	mv guile-config.out guile-config
	chmod +x guile-config

CLEANFILES = guile-config
