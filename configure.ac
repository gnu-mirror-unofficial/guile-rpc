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
GUILE_SITE_DIR

# Need the R6RS libs.
GUILE_MODULE_REQUIRED([r6rs bytevector])
GUILE_MODULE_REQUIRED([r6rs i/o ports])

# SRFI-34 is available in Guile 1.8, just checking to make sure.
GUILE_MODULE_REQUIRED([srfi srfi-34])

# Need SRFI-35 (available in `guile-library').
GUILE_MODULE_REQUIRED([srfi srfi-35])


# Output.
AC_CONFIG_FILES([Makefile
	         doc/Makefile
	         modules/Makefile
		 tests/Makefile])

AC_OUTPUT
