#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([GNU Guile-RPC], [0.4], [bug-guile-rpc@gnu.org],
  [guile-rpc], [http://www.gnu.org/software/guile-rpc/])
AC_CONFIG_SRCDIR([modules/rpc/xdr.scm])
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE([gnu no-define color-tests check-news -Wall])

AC_ARG_WITH([guilemoduledir],
  [use the specified installation path for Guile modules],
  [case "x$withval" in
     xyes|xno) guilemoduledir="";;
     *)        guilemoduledir="$withval";;
   esac],
  [guilemoduledir=""])
   

# Checks for Guile.
m4_pattern_forbid([^GUILE_(PKG|PROGS|SITE_DIR)$])
GUILE_PKG			dnl sets 'GUILE_EFFECTIVE_VERSION'
GUILE_PROGS
GUILE_SITE_DIR

# Guile-VM's compiler (aka. Guile 2.0.x).
AM_CONDITIONAL([HAVE_GUILE_VM],
  [$GUILE_TOOLS compile --help 2>&1 >/dev/null])

# Need the R6RS libs.
GUILE_MODULE_REQUIRED([rnrs bytevectors])
GUILE_MODULE_REQUIRED([rnrs io ports])

# SRFI-34 is available in Guile 1.8, just checking to make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35 (available in `guile-library').
GUILE_MODULE_REQUIRED([srfi srfi-35])

# Need SRFI-37 (in Guile 1.8.3 and later).
GUILE_MODULE_REQUIRED([srfi srfi-37])

# To use the compiler `(system base lalr)' is required.
GUILE_MODULE_REQUIRED([system base lalr])

# Optionally, `statprof' can be used when available.
GUILE_MODULE_AVAILABLE([have_statprof], [(statprof)])
AM_CONDITIONAL([HAVE_STATPROF], test "x$have_statprof" = "xyes")


if test "x$guilemoduledir" = "x"; then
  guilemoduledir="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION"
fi
AC_SUBST([guilemoduledir])

if test "$guilemoduledir" != "$GUILE_SITE"; then
   # Guile won't be able to locate the module "out of the box", so
   # warn the user.
   AC_MSG_WARN([`guilemoduledir' ($guilemoduledir) seems to be different from `GUILE_SITE' ($GUILE_SITE).])
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
