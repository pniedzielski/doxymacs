doxymacs
========

[![Packaging status](https://repology.org/badge/tiny-repos/emacs:doxymacs.svg)](https://repology.org/project/emacs:doxymacs/versions)

doxymacs is an e-lisp package for making doxygen usage easier under GNU
Emacs.

doxymacs homepage: https://pniedzielski.github.io/doxymacs/

Doxymacs has been tested on and works with:
 - GNU Emacs 20.7.1, 21.1.1, 21.2.1, 21.3, 21.4.1, 23.1.1

If you have success or failure with other versions of GNU Emacs, please
let the authors know.

See COPYING for the full text of the license under which is this work
is being made available.

See ChangeLog for recent changes.

See AUTHORS for a list of people to blame for this mess.

See TODO for a list of things that you can help out with.

See INSTALL for instructions on how to install and use this.

Feel free to contact me about any issues you may have, or to volunteer
to help out.

Ryan T. Sammartino
ryan.sammartino at gmail dot com

## License

Copyright © 2015–2025 Patrick M. Niedzielski.
Copyright © 2001–2010 Ryan T. Sammartino.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

## Install

Doxymacs does not depend on any external packages by default; you can use it
with any Emacsen.  However, doxymacs comes with an external parser for Doxygen
tags files written in C.  If your tags file is quite large (say, > 1 MB), you
may find the external parser to be more performant.  The external parser
depends on the following packages:

- autotools https://www.gnu.org/software/autoconf/
- pkg-config https://www.freedesktop.org/wiki/Software/pkg-config/
- libxml2 http://www.libxml.org/

Be sure these are properly configured and installed before proceeding.

- Use the bootstrap script to generate the parser’s build system, if
  `c/configure` does not already exist

   $ cd c
   $ ./bootstrap

- Use the configure script to configure the external parser:

   $ cd c
   $ ./configure
   $ make

For details see [INSTALL](INSTALL).
