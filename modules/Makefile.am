# GNU Guile-RPC --- A Scheme implementation of ONC RPC.
# Copyright (C) 2007, 2008, 2009, 2010, 2011, 2014  Free Software Foundation, Inc.
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

SOURCES =						\
  rpc/xdr.scm rpc/xdr/types.scm				\
  rpc/rpc.scm rpc/rpc/types.scm rpc/rpc/transports.scm	\
  rpc/rpc/server.scm rpc/rpc/portmap.scm		\
  rpc/compiler/parser.scm rpc/compiler/lexer.scm	\
  rpc/compiler.scm

nobase_dist_guilemodule_DATA = $(SOURCES)

EXTRA_DIST = srfi/srfi-64.scm srfi/srfi-64.upstream.scm	\
	     rpc/compiler/lexer.l

# Building the lexers with SILex.  You must have SILex installed in
# `$(top_srcdir)/modules/silex'.  The Git repository already contains
# it.  Otherwise, you can fetch it from
# http://www.iro.umontreal.ca/~dube/silex-src.tar.gz .
#
# Note: Those files should normally be part of the distribution, making
# this rule useless to the user.
.l.scm:
	module="`basename $@ | sed -es/\\.scm$$//g`" ;				\
	GUILE_AUTO_COMPILE=0							\
	$(GUILE) -L $(abs_top_srcdir)/modules/silex				\
	         -c "(load-from-path \"lex.scm\")				\
		     (lex \"$^\" \"$@\" 'counters 'all)" &&			\
	mv "$@" "$@.tmp" &&							\
	echo ';;; -*- buffer-read-only: t; -*-' > "$@" &&			\
	echo ';;; Automatically generated---do not modify.' >> "$@" &&		\
	echo "(define-module (rpc compiler $$module)" >> "$@" &&		\
	echo '  :use-module (system base lalr)' >> "$@" &&			\
	echo '  :use-module (srfi srfi-39)' >> "$@" &&				\
	echo '  :export (*strip-%-comments?*' >> "$@" &&			\
	echo '           lexer-init lexer' >> "$@" &&				\
	echo '           lexer-get-func-column' >> "$@" &&			\
	echo '           lexer-get-func-offset' >> "$@" &&			\
	echo '           lexer-get-line lexer-getc' >> "$@" &&			\
	echo '           lexer-ungetc))' >> "$@" &&				\
	echo '(define *strip-%-comments?*' >> "$@" &&				\
	echo '  (make-parameter #t))' >> "$@" &&				\
	echo '(define (location line column offset type' >> "$@" &&		\
	echo '                   . value)' >> "$@" &&			\
	echo '  (make-lexical-token type' >> "$@" &&				\
	echo '                      (make-source-location #f' >> "$@" &&	\
	echo '                                   line' >> "$@" &&		\
	echo '                                   column' >> "$@" &&		\
	echo '                                   offset' >> "$@" &&		\
	echo '                                   #f)' >> "$@" &&		\
	echo '                      (append value' >> "$@" && \
	echo '                              (list (vector line column)))))' >> "$@" &&				\
	cat "$@.tmp" >> "$@" &&							\
	rm "$@.tmp"


CLEANFILES =

if HAVE_GUILE_VM

GOBJECTS = $(SOURCES:%.scm=%.go)
nobase_nodist_guilemodule_DATA = $(GOBJECTS)

guile_compiler_warnings =			\
  -Wunbound-variable -Warity-mismatch		\
  -Wunused-toplevel -Wunused-toplevel		\
  -Wformat

.scm.go:
	$(MKDIR_P) `dirname $@`
	GUILE_AUTO_COMPILE=0							\
	GUILE_LOAD_COMPILED_PATH="$(builddir):$$GUILE_LOAD_COMPILED_PATH"	\
	$(GUILE_TOOLS) compile -L $(srcdir) $(guile_compiler_warnings)		\
	  -o "$@" "$<"

CLEANFILES += $(GOBJECTS)

endif HAVE_GUILE_VM
