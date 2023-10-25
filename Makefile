.PHONY: all elisp c clean

EMACS := /usr/bin/emacs
ELISP := doxymacs.el xml-parse.el

CC  := /usr/bin/gcc
INC := /usr/include/libxml2
LNK := /usr/local/lib

all: elisp c
	@#
elisp: clean $(ELISP)
	$(EMACS) --batch -Q -L . -f batch-byte-compile $(ELISP)
c: doxymacs_parser.c
	$(CC) -I$(INC) -L$(LNK) -o doxymacs_parser $< -lxml2
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc
	rm -f doxymacs_parser
