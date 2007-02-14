#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT([guile-rpc], [0.0], [ludovic.courtes@laas.fr])
AC_CONFIG_AUX_DIR(build-aux)
AC_CONFIG_MACRO_DIR(m4)

AM_INIT_AUTOMAKE(guile-rpc, 0.0)

AC_CONFIG_SRCDIR([modules/rpc/xdr.scm])


# Checks for Guile.
GUILE_PROGS
GUILE_FLAGS
AC_PATH_PROG([guile_snarf], [guile-snarf], [not-found])
if test "x$guile_snarf" = "xnot-found"; then
   AC_MSG_ERROR([`guile-snarf' not found.  Please install Guile 1.8.x or later.])
fi
GUILE_SITE_DIR

# Need the R6RS libs.
GUILE_MODULE_REQUIRED([r6rs bytevector])
GUILE_MODULE_REQUIRED([r6rs i/o ports])

# Output.
AC_CONFIG_FILES([Makefile
	         modules/Makefile
		 tests/Makefile])

AC_OUTPUT
