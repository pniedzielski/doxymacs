## Process this file with automake to produce Makefile.in
# $Id: Makefile.am,v 1.1 2003/01/06 00:28:22 ryants Exp $

bin_PROGRAMS = doxymacs_parser

doxymacs_parser_SOURCES = doxymacs_parser.c

INCLUDES = $(XML_CFLAGS)

AM_CFLAGS = -Wall -Werror -fexpensive-optimizations -fomit-frame-pointer

doxymacs_parser_LDFLAGS = $(XML_LIBS)
