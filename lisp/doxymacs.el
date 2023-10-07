;;; -*-emacs-lisp-*-
;;; doxymacs.el --- ELisp package for making doxygen related stuff easier.
;;
;;
;; Copyright (C) 2001-2010 Ryan T. Sammartino
;;
;; Author: Ryan T. Sammartino <ryan.sammartino at gmail dot com>
;;      Kris Verbeeck <kris.verbeeck at advalvas dot be>
;; Created: 24/03/2001
;; Version: @VERSION@
;; Keywords: doxygen documentation
;;
;; This file is NOT part of GNU Emacs or XEmacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;; Doxymacs homepage: http://doxymacs.sourceforge.net/

;; Commentary:
;;
;; Doxymacs depends on the following packages:
;;
;; - W3      http://www.cs.indiana.edu/usr/local/www/elisp/w3/docs.html
;; - tempo   http://www.lysator.liu.se/~davidk/elisp/
;; - libxml2 http://www.libxml.org/
;;
;; Be sure these are properly configured and installed before proceeding.
;;
;; - Use the configure script to configure doxymacs:
;;
;;    $ ./configure
;;    $ make
;;    $ make install
;;
;;   Use ./configure --help for help on customising your configuration.
;;
;;   If you get
;;
;;   !! File error (("Cannot open load file" "url"))
;;
;;   (or something similar) then set the variable EMACSLOADPATH before
;;   doing make:
;;
;;    $ EMACSLOADPATH=... make
;;
;;   where ... is a colon separated list of directories to search for
;;   packages.  To byte compile with XEmacs, set the variable EMACS:
;;
;;    $ EMACS=xemacs make
;;
;;   If you would rather not byte compile the .el files at all, then do:
;;
;;    $ make ELCFILES=
;;
;; - Customise the variable doxymacs-doxygen-dirs.
;;
;; - If your tags file is quite large (say, > 1 MB), consider setting
;;   doxymacs-use-external-xml-parser to t and be sure to set
;;   doxymacs-external-xml-parser-executable to the right value (the
;;   default should usually be fine).  A suitable program is
;;   distributed along with this file in the directory doxymacs/c/.
;;   With an 11 MB XML tag file, the internal process takes 20 minutes
;;   on a PIII 800 with 1 GB of RAM, whereas the external process
;;   takes 12 seconds.
;;
;; - Put (require 'doxymacs) in your .emacs
;;
;; - Invoke doxymacs-mode with M-x doxymacs-mode.  To have doxymacs-mode
;;   invoked automatically when in C/C++ mode, put
;;
;;   (add-hook 'c-mode-common-hook 'doxymacs-mode)
;;
;;   in your .emacs.
;;
;; - If you want Doxygen keywords fontified use M-x doxymacs-font-lock.
;;   To do it automatically, add the following to your .emacs:
;;
;;   (defun my-doxymacs-font-lock-hook ()
;;     (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;;         (doxymacs-font-lock)))
;;   (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
;;
;;   This will add the Doxygen keywords to c-mode and c++-mode only.
;;
;; - Default key bindings are:
;;   - C-c d ? will look up documentation for the symbol under the point.
;;   - C-c d r will rescan your Doxygen tags file.
;;   - C-c d f will insert a Doxygen comment for the next function.
;;   - C-c d i will insert a Doxygen comment for the current file.
;;   - C-c d ; will insert a Doxygen comment for the current member.
;;   - C-c d m will insert a blank multiline Doxygen comment.
;;   - C-c d s will insert a blank singleline Doxygen comment.
;;   - C-c d @ will insert grouping comments around the current region.
;;
;; Doxymacs has been tested on and works with:
;; - GNU Emacs 20.7.1, 21.1.1, 21.2.1, 21.2.92.1, 21.3, 21.4.1, 23.1.1
;; - XEmacs 21.1 (patch 14), 21.4 (patches 4-17)
;;
;; If you have success or failure with other version of {X}Emacs, please
;; let the authors know.

(shell-command "cd ..")
(shell-command "./bootstrap")
(shell-command "./configure")
(shell-command "make --prefix=/home/lorenz/tmp")
(shell-command "make install")

(require 'doxymacs-impl)
(provide 'doxymacs)

;;; doxymacs.el ends here
