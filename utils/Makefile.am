# GNU Guile-RPC --- A Scheme implementation of ONC RPC.
# Copyright (C) 2007, 2008, 2010, 2014  Free Software Foundation, Inc.
#
# This file is part of GNU Guile-RPC.
#
# GNU Guile-RPC is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guile-RPC is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
# General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

EXTRA_DIST = grpc-rpcinfo.in grpc-compile.in grpc-nfs-export.in

bin_SCRIPTS = grpc-rpcinfo grpc-nfs-export grpc-compile
CLEANFILES  = $(bin_SCRIPTS)

pkgdata_DATA = nfs-server-stub.scm mount-server-stub.scm
CLEANFILES  += $(pkgdata_DATA)


GRPC_COMPILE =									\
  GUILE_AUTO_COMPILE=0								\
  GUILE_LOAD_PATH="$(top_srcdir)/modules:$$GUILE_LOAD_PATH"			\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir)/modules:$$GUILE_LOAD_COMPILED_PATH"	\
  "$(builddir)/grpc-compile"


substitute = sed -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
		 -e 's,[@]pkgdatadir[@],$(pkgdatadir),g'		\
		 -e 's,[@]GUILE[@],$(GUILE),g'				\
		 -e 's,[@]PACKAGE_STRING[@],$(PACKAGE_STRING),g'	\
		 -e 's,[@]PACKAGE_NAME[@],$(PACKAGE_NAME),g'		\
		 -e 's,[@]PACKAGE_VERSION[@],$(PACKAGE_VERSION),g'	\
		 -e 's,[@]PACKAGE_BUGREPORT[@],$(PACKAGE_BUGREPORT),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" &&	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

nfs-server-stub.scm: grpc-compile $(top_srcdir)/rpcsvc/nfs_prot.x
	$(GRPC_COMPILE) --xdr --constants --server	\
	  < "$(top_srcdir)/rpcsvc/nfs_prot.x"		\
	  > "$@.tmp"
	mv "$@.tmp" "$@"

mount-server-stub.scm: grpc-compile $(top_srcdir)/rpcsvc/mount.x
	$(GRPC_COMPILE) --xdr --constants --server	\
	  < "$(top_srcdir)/rpcsvc/mount.x"		\
	  > "$@.tmp"
	mv "$@.tmp" "$@"
