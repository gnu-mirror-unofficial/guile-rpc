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

TESTS_ENVIRONMENT =								\
  GUILE_LOAD_COMPILED_PATH="$(top_builddir)/modules:$$GUILE_LOAD_COMPILED_PATH"

# Running the '.test' files.
TEST_LOG_COMPILER = $(GUILE)
AM_TEST_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)/modules"

TESTS = xdr.test xdr-encoding.test rpc.test server-loop.test	\
        parser.test compiler.test

EXTRA_DIST = $(TESTS)

CLEANFILES = xdr-type-size.log xdr-encoding.log	\
	     rpc-message.log server-loop.log	\
	     xdr-parser.log xdr-compiler.log
