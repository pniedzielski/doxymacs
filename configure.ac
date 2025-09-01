dnl Process this file with autoconf to produce a configure script.
AC_PREREQ([2.69])

AC_INIT([Doxymacs],
        [1.8.0],
        [patrick+doxymacs@pniedzielski.net],
        [doxymacs],
        [https://pniedzielski.github.io/doxymacs/])

AC_CONFIG_SRCDIR(c/doxymacs_parser.c)

AM_INIT_AUTOMAKE([foreign])     dnl Don't worry about missing GNU files.

AC_PREFIX_DEFAULT(${HOME})

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AM_PATH_LISPDIR

dnl Checks for libraries.
PKG_CHECK_MODULES(XML, [libxml-2.0 >= 2.6.13])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

dnl Checks for library functions.
AC_CHECK_FUNCS(strdup)

dnl Doxymacs specific configure options
AC_ARG_WITH(external-xml-parser,
  AS_HELP_STRING([--with-external-xml-parser],
                 [Use external default xml parser by default.  Default is to use internal xml parser.]),
  DOXYMACS_USE_EXTERNAL_XML_PARSER="t", DOXYMACS_USE_EXTERNAL_XML_PARSER="nil")

AC_SUBST(DOXYMACS_USE_EXTERNAL_XML_PARSER)

dnl Here is a hack to get the REAL bindir without any other
dnl embedded variables.
AC_CONFIG_COMMANDS_PRE([abs_bindir=${bindir} ; while echo ${abs_bindir} | grep '${[[A-Za-z0-9_]]\+}' > /dev/null ; do abs_bindir=`eval echo ${abs_bindir}` ; done ; DOXYMACS_PARSER=${abs_bindir}/doxymacs_parser${EXEEXT}])

AC_SUBST(DOXYMACS_PARSER)

AC_ARG_VAR(EMACS, [How to invoke emacs (e.g. EMACS=emacs).])
AC_ARG_VAR(EMACSLOADPATH, [Default load-path for EMACS.])

AC_CONFIG_FILES([Makefile c/Makefile lisp/Makefile lisp/doxymacs.el])

AC_OUTPUT
