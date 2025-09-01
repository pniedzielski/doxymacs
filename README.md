doxymacs
========

[![Packaging status](https://repology.org/badge/tiny-repos/emacs:doxymacs.svg)](https://repology.org/project/emacs:doxymacs/versions)

doxymacs is an e-lisp package for making doxygen usage easier under {X}Emacs.

doxymacs homepage: http://doxymacs.sourceforge.net/

Doxymacs has been tested on and works with:
 - GNU Emacs 20.7.1, 21.1.1, 21.2.1, 21.3, 21.4.1, 23.1.1
 - XEmacs 21.1 (patch 14), 21.4 (patch 4, 5, 6, 17)

If you have success or failure with other versions of {X}Emacs, please
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

Doxymacs depends on the following packages:

- pkg-config https://www.freedesktop.org/wiki/Software/pkg-config/
- libxml2 http://www.libxml.org/

Be sure these are properly configured and installed before proceeding.

- Use the configure script to configure doxymacs:

   $ ./configure
   $ make
   $ make install

  Use ./configure --help for help on customising your configuration.

for details see [INSTALL](INSTALL)
