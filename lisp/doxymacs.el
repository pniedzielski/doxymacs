;; doxymacs.el
;;
;; $Id: doxymacs.el,v 1.4 2001/03/29 18:44:15 airborne Exp $
;;
;; ELisp package for making doxygen related stuff easier.
;;
;; Copyright (C) 2001 Ryan T. Sammartino
;; http://members.home.net/ryants/
;; ryants@home.com
;;
;; Doxymacs homepage: http://doxymacs.sourceforge.net/
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
;; ChangeLog
;;
;; 28/03/2001 - added doxymacs to the "tools" customisation group.
;;            - removed doxymacs-browser (just use user's default browser)
;;            - minor formatting updates
;; 24/03/2001 - initial version.  Pretty lame.  Need some help.

;; TODO
;;
;; - add ability to get tag file from a URL as well as a local file.
;; - add ability to automagically insert doxygen comments.
;; - if doxymacs-get-matches finds more than one match, present user with 
;;   list of choices
;;   to select from.  Use third element of list to distinguish each choice.
;; - add some default key-bindings 
;; - error checking (invalid tags file format, etc).
;; - test this on other versions of {X}Emacs other than the one I'm 
;;   using (XEmacs 21.1.14)
;; - other stuff?


(provide 'doxymacs)

(require 'custom)
(require 'w3)

(defgroup doxymacs nil
  "Find documentation for symbol at point"
  :group 'tools)

(defcustom doxymacs-doxygen-root
  "file:///home/ryants/projects/doxymacs/example/doc/html/"
  "*Root for doxygen documentation (URL)"
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-doxygen-tags
  "../example/doc/doxy.tag"
  "*File that contains doxygen tags"
  :type 'string
  :group 'doxymacs)

(defvar doxymacs-tags-buffer nil
  "The buffer with our doxytags")



;;doxymacs-load-tag
;;This loads the tags file into the buffer *doxytags*.  
;;Note that the format of this file is expected to be:
;;
;;<symbol><TAB><HTML href><TAB><description>
;;
;;The doxytags.pl PERL script will generate a tag file with the right format.
;;
;;FIXME - this should check that the file we load has the right format, 
;;or something.
(defun doxymacs-load-tags ()
  "Loads a tags file"
  (if (or (eq doxymacs-tags-buffer nil)
	  (eq (buffer-live-p doxymacs-tags-buffer) nil))
      (progn
	(setq doxymacs-tags-buffer (generate-new-buffer "*doxytags*"))
	(let ((currbuff (current-buffer)))
	  (set-buffer doxymacs-tags-buffer)
	  (insert-file-contents doxymacs-doxygen-tags)
	  (set-buffer currbuff)))))

;;doxymacs-get-matches
;;Finds lines in *doxytags* buffer that match symbol.
;;Returns the matches in a list.
(defun doxymacs-get-matches (symbol)
  "Find matches in the tags buffer for the given symbol"
  (save-excursion
    (if (or (eq doxymacs-tags-buffer nil) 
            (eq (buffer-live-p doxymacs-tags-buffer) nil))
        (doxymacs-load-tags))
      (let ((currbuff (current-buffer))
	    (matches nil))
	(set-buffer doxymacs-tags-buffer)
	(goto-char (point-min))
	(setq case-fold-search nil)
	;;The following line contains a <TAB> in the regexp.
	(while (re-search-forward (concat "^" (regexp-quote symbol) "	") 
				  nil t) 
	  (progn
	    (beginning-of-line)
	    (let ((start (point)))
	      (end-of-line)
	      (setq matches (cons 
			     (split-string (buffer-substring start (point))) 
			     matches)))))
	(set-buffer currbuff)
	(reverse matches))))

(defun doxymacs-display-match (match)
  "Displays the given match"
  (browse-url (concat doxymacs-doxygen-root "/" (cadr match))))


;;FIXME
;; If the length of matches is > 1, then display a list of choices to the user.
;; The list should show the third element of each element of matches.
(defun doxymacs-choose-match (matches)
  "Displays the available choices for the user to select"
  (car matches))

(defun doxymacs-search (string)
  "Look up the symbol under the cursor in doxygen"
  (interactive 
   (save-excursion
     (let ((string (read-string "Look up: " (symbol-near-point) nil)))
	 (list string))))
  (let ((matches (doxymacs-get-matches string)))
    (if (eq (length matches) 1)
	(doxymacs-display-match (car matches))
      (doxymacs-display-match (doxymacs-choose-match matches)))))