# doxymacs_parser Makefile
# Copyright (C) 2001 Ryan T. Sammartino
# <ryants at home dot com>
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
#
# Doxymacs homepage: http://doxymacs.sourceforge.net/
#
# $Id: Makefile,v 1.4 2001/05/27 19:07:51 ryants Exp $

# Double check that these settings make sense for your system.
# (Sorry about not having a configure script... just seems like overkill
# for such a small program)
#
# doxymacs_parser requires libxml version 2 or greater, which can be
# obtained from http://www.libxml.org/.

CC		= 	gcc

CFLAGS		= 	-O2 -g -Wall -Werror \
			-fexpensive-optimizations -fomit-frame-pointer \
			`xml-config --cflags`

LDFLAGS		=	`xml-config --libs`

INSTALL_DIR	=	$(HOME)/bin


# No user serviceable part below this line.

all: doxymacs_parser

doxymacs_parser: doxymacs_parser.o
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

.PHONY: install
install: doxymacs_parser
	if [ ! -d $(INSTALL_DIR) ]; then mkdir -p $(INSTALL_DIR) ; fi
	cp -f doxymacs_parser $(INSTALL_DIR)/doxymacs_parser

.PHONY: clean
clean:
	$(RM) doxymacs_parser.o doxymacs_parser
