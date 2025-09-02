dnl Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])

AC_INIT([Doxymacs Parser],
        [1.8.0],
        [patrick+doxymacs@pniedzielski.net],
        [doxymacs_parser],
        [https://pniedzielski.github.io/doxymacs/])

AC_CONFIG_SRCDIR(doxymacs_parser.c)

AM_INIT_AUTOMAKE([foreign])     dnl Don't worry about missing GNU files.

AC_PREFIX_DEFAULT(${HOME})

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

dnl Checks for libraries.
PKG_CHECK_MODULES(XML, [libxml-2.0 >= 2.6.13])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

dnl Checks for library functions.
AC_CHECK_FUNCS(strdup)

AC_CONFIG_FILES([Makefile])

AC_OUTPUT
