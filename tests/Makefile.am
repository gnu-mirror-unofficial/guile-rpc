# GNU Guile-RPC --- A Scheme implementation of ONC RPC.
# Copyright (C) 2007, 2008  Free Software Foundation, Inc.
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

TESTS_ENVIRONMENT = $(GUILE) -L $(top_srcdir)/modules

TESTS = xdr.test xdr-encoding.test rpc.test server-loop.test
compiler_tests = parser.test compiler.test

EXTRA_DIST = $(TESTS)

if HAVE_PARSE_LALR
TESTS += $(compiler_tests)
else
EXTRA_DIST += $(compiler_tests)
endif

CLEANFILES = xdr-type-size.log xdr-encoding.log	\
	     rpc-message.log server-loop.log	\
	     xdr-parser.log xdr-compiler.log

# XXX: Guile-Lint version 9 doesn't properly handle the
# `define-syntax' macros used by SRFI-64.
#
# if HAVE_GUILE_LINT
# all-local: $(TESTS)
#	GUILE=$(GUILE)				\
# 	$(GUILE_LINT) -L $(top_srcdir)/modules $^
# endif
