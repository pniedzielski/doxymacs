# doxymacs_parser Makefile
# Copyright (C) 2001 Ryan T. Sammartino
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
# $Id: Makefile,v 1.2 2001/05/21 19:42:22 ryants Exp $

CC		= 	gcc

CFLAGS		= 	-O2 -g -Wall \
			-fexpensive-optimizations -fomit-frame-pointer \
			`xml-config --cflags`

LDFLAGS		=	`xml-config --libs`

INSTALL_DIR	=	$(HOME)/bin

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
