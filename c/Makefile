
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
