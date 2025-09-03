Doxymacs is Doxygen + Emacs
===========================

![GitHub License](https://img.shields.io/github/license/pniedzielski/doxymacs)
![GitHub branch status](https://img.shields.io/github/checks-status/pniedzielski/doxymacs/master)
![GitHub top language](https://img.shields.io/github/languages/top/pniedzielski/doxymacs)
[![Packaging status](https://repology.org/badge/tiny-repos/emacs:doxymacs.svg)](https://repology.org/project/emacs:doxymacs/versions)

[Doxymacs][doxymacs] provides a minor mode for GNU Emacs that provides
tighter integration with [Doxygen][doxygen], the multi-language
documentation generator.  It provides a number of features that make
working with Doxygen comments and documentation easier:

  - Lookup documentation for symbols from Emacs in the browser of your
    choice.
  - Easily insert Doxygen style comments into source code (C, C++,
    Javadoc, and Fortran are supported).
  - Fontify Doxygen keywords in comments.
  - Optionally use an “external” (i.e., written in C) XML parser to
    speed up building the completion list.

Doxymacs is free software: you may use and distribute it under the terms
of the GNU General Public License, version 3 or later.

Doxymacs is actively maintained.  It supports GNU Emacs 24.4 and later.

Please direct any bug reports or feature requests to Issues section of
the GitHub project.  If you want to contribute, open up a Pull Request
or mail your patches to the maintainer.

[doxymacs]: https://pniedzielski.github.io/doxymacs/ "Doxymacs Website"
[doxygen]: https://www.doxygen.nl/index.html "Doxygen Website"

Quickstart
----------

Doxymacs is not currently included in any package repository.  In recent
versions of GNU Emacs, you can automatically download Doxymacs from
GitHub.  With GNU Emacs 30 or above, include the following in your Emacs
initialization file:

```elisp
(use-package doxymacs
  :vc (:url "https://github.com/pniedzielski/doxymacs.git"
            :rev :newest
            :lisp-dir "lisp/")
  :hook (c-mode-common-hook . doxymacs-mode)
  :bind (:map c-mode-base-map
              ;; Lookup documentation for the symbol at point.
              ("C-c d ?" . doxymacs-lookup)
              ;; Rescan your Doxygen tags file.
              ("C-c d r" . doxymacs-rescan-tags)
              ;; Prompt you for a Doxygen command to enter, and its
              ;; arguments.
              ("C-c d RET" . doxymacs-insert-command)
              ;; Insert a Doxygen comment for the next function.
              ("C-c d f" . doxymacs-insert-function-comment)
              ;; Insert a Doxygen comment for the current file.
              ("C-c d i" . doxymacs-insert-file-comment)
              ;; Insert a Doxygen comment for the current member.
              ("C-c d ;" . doxymacs-insert-member-comment)
              ;; Insert a blank multi-line Doxygen comment.
              ("C-c d m" . doxymacs-insert-blank-multiline-comment)
              ;; Insert a blank single-line Doxygen comment.
              ("C-c d s" . doxymacs-insert-blank-singleline-comment)
              ;; Insert a grouping comments around the current region.
              ("C-c d @" . doxymacs-insert-grouping-comments))
  :custom
    ;; Configure source code <-> Doxygen tag file <-> Doxygen HTML
    ;; documentation mapping:
    ;;   - Files in /home/me/project/foo/ have their tag file at
    ;;     http://someplace.com/doc/foo/foo.xml, and HTML documentation
    ;;     at http://someplace.com/doc/foo/.
    ;;   - Files in /home/me/project/bar/ have their tag file at
    ;;     ~/project/bar/doc/bar.xml, and HTML documentation at
    ;;     file:///home/me/project/bar/doc/.
    ;; This must be configured for Doxymacs to function!
    (doxymacs-doxygen-dirs
     '(("^/home/me/project/foo/"
        "http://someplace.com/doc/foo/foo.xml"
         "http://someplace.com/doc/foo/")
       ("^/home/me/project/bar/"
        "~/project/bar/doc/bar.xml"
        "file:///home/me/project/bar/doc/"))))
```

To use doxymacs with older versions of Emacs, download a copy of the
source code, and put the following in your Emacs initialization file

```elisp
(add-to-list 'load-path (expand-file-name "/path/to/doxymacs/lisp/"))
(require 'doxymacs)

;; Now configure as you want: set up keybindings, hooks, and
;; doxygen-dirs.
```

History
-------

Doxymacs was originally written in 2001 by Ryan T. Sammartino, and was
hosted on Sourceforge for a long time.  The last version released by
Ryan was version 1.8, released on 2007-06-10.

In 2015, with Ryan’s blessing, Patrick M. Niedzielski took over
maintenance of the project on GitHub, working on modernizing the project
to support recent versions of GNU Emacs.  As part of the 2.0 release,
Patrick merged many of the patches that had been submitted to
Sourceforge but never merged and compiled features from forks on GitHub
with permission from their authors.

Plans for doxymacs include support for tree-sitter enhanced
fontification and indentation within Doxygen comments, improvements to
the XML parsing, and better integration with other Emacs packages.

Installing The External Parser
------------------------------

Doxymacs comes with an external parser for Doxygen tags files written in
C.  If your tags file is quite large, you may find the external parser
to be more performant.  The external parser depends on the following
packages:

  - autotools: https://www.gnu.org/software/autoconf/
  - pkg-config: https://www.freedesktop.org/wiki/Software/pkg-config/
  - libxml2: http://www.libxml.org/

Be sure these are properly configured and installed before proceeding.

  - Set `doxymacs-use-external-xml-parser` to `t` and be sure to set
    `doxymacs-external-xml-parser-executable` to where you would like the
    external XML parser to be installed.

  - Run `M-x doxymacs-install-external-parser`.

If you want, you may also compile the parser manually, by performing the
following steps:

  - Use the bootstrap script to generate the parser’s build system, if
    `c/configure` does not already exist

       $ cd c
       $ ./bootstrap

 - Use the configure script to configure the external parser:

       $ cd c  # If not already in c/
       $ ./configure
       $ make

  - Set `doxymacs-use-external-xml-parser` to `t` and be sure to set
    `doxymacs-external-xml-parser-executable` to the location of the external
    XML parser.

Contributing
------------

To contribute, please open up a Pull Request on GitHub or mail your
patches to the maintainer.

Please structure your commit messages according to [Conventional
Commits][conventional], with descriptive bodies, a `License: GPLv3+`
trailer indicating that your changes are licensed under the GNU General
Public License version 3 or later, and a `Signed-off-by:` trailer
indicating that you have the right to distribute the changes in the
commit under these terms.

[conventional]: https://www.conventionalcommits.org/en/v1.0.0/ "Conventional Commits specification"

License
-------

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

Versions prior to 1.8 of this doxymacs were licensed under the GNU
General Public License version 2 or later.
