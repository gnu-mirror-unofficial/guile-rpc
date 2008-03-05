#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([guile-rpc], [0.3], [bug-guile-rpc@gnu.org])
AC_CONFIG_SRCDIR([modules/rpc/xdr.scm])
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu no-define check-news])

AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

# Checks for Guile.
GUILE_PROGS
GUILE_SITE_DIR

# Guile-Lint (http://www.geocities.com/user42_kevin/guile-lint/index.html)
AC_PATH_PROG([GUILE_LINT], [guile-lint])
AC_SUBST([GUILE_LINT])
AM_CONDITIONAL([HAVE_GUILE_LINT], test "x$GUILE_LINT" != "x")

# Need the R6RS libs.
GUILE_MODULE_REQUIRED([r6rs bytevector])
GUILE_MODULE_REQUIRED([r6rs io ports])

# SRFI-34 is available in Guile 1.8, just checking to make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35 (available in `guile-library').
GUILE_MODULE_REQUIRED([srfi srfi-35])

# Need SRFI-37 (in Guile 1.8.3 and later).
GUILE_MODULE_REQUIRED([srfi srfi-37])

# To use the compiler `(text parse-lalr)' is needed.
GUILE_MODULE_AVAILABLE([have_text_parse_lalr], [(text parse-lalr)])
AM_CONDITIONAL([HAVE_PARSE_LALR], test "x$have_text_parse_lalr" = "xyes")
if test "x$have_text_parse_lalr" != "xyes"; then
   AC_MSG_WARN([The `(text parse-lalr)' from Guile-Library is not available.])
   AC_MSG_WARN([You will not be able to use the XDR/RPC compiler unless you install it.])
fi

# Optionally, `statprof' can be used when available.
GUILE_MODULE_AVAILABLE([have_statprof], [(statprof)])
AM_CONDITIONAL([HAVE_STATPROF], test "x$have_statprof" = "xyes")


pkgdatadir="$datadir/$PACKAGE_NAME"
if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$pkgdatadir"
fi
AC_SUBST([guilemoduledir])

if test "$guilemoduledir" != "$GUILE_SITE"; then
   # Guile won't be able to locate the module "out of the box", so
   # warn the user.
   AC_MSG_WARN([`guilemoduledir' ($guilemoduledir) is different from `GUILE_SITE' ($GUILE_SITE).])
   AC_MSG_WARN([Make sure to adjust the `GUILE_LOAD_PATH' environment variable accordingly,])
   AC_MSG_WARN([or re-run `configure' with `--with-guilemoduledir=$GUILE_SITE'.])
fi

# Output.
AC_CONFIG_FILES([Makefile
	         doc/Makefile
	         modules/Makefile
		 utils/Makefile
		 tests/Makefile
		 benchmarks/Makefile])

AC_OUTPUT
