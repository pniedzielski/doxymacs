;; doxymacs.el
;;
;; $Id: doxymacs.el,v 1.9 2001/04/12 03:13:14 ryants Exp $
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
;; 11/04/2001 - added ability to insert blank doxygen comments with either
;;              Qt or JavaDoc style.
;;            - also did "file" comments
;; 31/03/2001 - added ability to choose which symbol to look up if more than
;;              one match
;;            - slightly changed the format of the list that 
;;              doxymacs-get-matches returns
;; 28/03/2001 - added doxymacs to the "tools" customisation group.
;;            - removed doxymacs-browser (just use user's default browser)
;;            - minor formatting updates
;; 24/03/2001 - initial version.  Pretty lame.  Need some help.

;; TODO
;;
;; - add ability to get tag file from a URL as well as a local file.
;; - add ability to automagically insert doxygen comments.
;; - add some default key-bindings 
;; - error checking (invalid tags file format, etc).
;; - test this on other versions of {X}Emacs other than the one I'm 
;;   using (XEmacs 21.1.14)
;; - other stuff?

;; Front matter and variables

(provide 'doxymacs)

(require 'custom)

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

(defcustom doxymacs-doxygen-style
  "JavaDoc"
  "*The style of comments to insert into code"
  :type '(radio (const :tag "JavaDoc" "JavaDoc") (const :tag "Qt" "Qt"))  
  :group 'doxymacs)

(defvar doxymacs-tags-buffer nil
  "The buffer with our doxytags")


;;These functions have to do with looking stuff up in doxygen generated
;;documentation

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
;;Returns the matches in a list.  The list looks like this:
;;( (qualified_name URL) (qualified_name URL) ... )
(defun doxymacs-get-matches (symbol)
  "Find matches in the tags buffer for the given symbol"
  (save-excursion
    (if (or (eq doxymacs-tags-buffer nil) 
            (eq (buffer-live-p doxymacs-tags-buffer) nil))
        (doxymacs-load-tags))
    (let ((currbuff (current-buffer))
          (matches nil)
          (regexp (concat "^" (regexp-quote symbol) "\t\\(.*\\)\t\\(.*\\)$")))
      (set-buffer doxymacs-tags-buffer)
      (goto-char (point-min))
      (setq case-fold-search nil)
      (while (re-search-forward regexp nil t) 
        (setq matches (cons 
                       (list (match-string 2) (match-string 1))
                       matches)))
      (set-buffer currbuff)
      (reverse matches))))


(defun doxymacs-display-match (match)
  "Displays the given match"
  (browse-url (concat doxymacs-doxygen-root "/" (cadr match))))


;;FIXME
;;I'd like the *Completions* buffer to come up automatically
(defun doxymacs-choose-match (symbol matches)
  "Displays the available choices for the user to select"
  (assoc
   (completing-read
    (concat "More than one match for " symbol ", select one: ")
    matches
    nil 'f)
   matches))
  

(defun doxymacs-search (symbol)
  "Look up the symbol under the cursor in doxygen"
  (interactive 
   (save-excursion
     (let ((symbol (read-string "Look up: " (symbol-near-point) nil)))
	 (list symbol))))
  (let ((matches (doxymacs-get-matches symbol)))
    (if (eq (length matches) 0)
	(progn
	  (beep)
	  (message (concat "Symbol " symbol " not found")))
      (if (eq (length matches) 1)
	  (doxymacs-display-match (car matches))
	(let ((choice (doxymacs-choose-match symbol matches)))
	  (if (eq choice nil)
	      (beep) ;; This might be annoying, but seems to be a standard
	    (doxymacs-display-match choice)))))))


;; These functions have to do with inserting doxygen commands in code

(defun doxymacs-blank-multiline-comment ()
  (if (equal doxymacs-doxygen-style "JavaDoc")
      "/**\n * \n * \n */\n"
    "//! \n/*!\n \n*/\n"))

(defun doxymacs-insert-blank-multiline-comment ()
  "Inserts a mult-line blank doxygen comment at the current point"
  (interactive "*")
  (save-excursion 
    (beginning-of-line)
    (let ((start (point)))
      (insert (doxymacs-blank-multiline-comment))
      (let ((end (point)))
	(indent-region start end nil))))
  (if (equal doxymacs-doxygen-style "JavaDoc")
      (end-of-line 2))
  (end-of-line))

(defun doxymacs-blank-singleline-comment ()
  (if (equal doxymacs-doxygen-style "JavaDoc")
      "/// "
    "//! "))

(defun doxymacs-insert-blank-singleline-comment ()
  "Inserts a single-line blank doxygen comment at current point"
  (interactive "*")
  (save-excursion
   (beginning-of-line)
   (let ((start (point)))
     (insert (doxymacs-blank-singleline-comment))
     (let ((end (point)))
       (indent-region start end nil))))
  (end-of-line))

(defun doxymacs-file-comment ()
  (let ((fname (if (buffer-file-name) 
		   (file-name-nondirectory (buffer-file-name))
		 "")))
	(if (equal doxymacs-doxygen-style "JavaDoc")
	    (format (concat "/**\n"
			    " * @file   %s\n"
			    " * @author %s <%s>\n"
			    " * @date   %s\n"
			    " *\n"
			    " * @brief  \n"
			    " *\n"
			    " *\n"
			    " */")
		    fname
		    (user-full-name)
		    (user-mail-address)
		    (current-time-string))
	  (format (concat "/*!\n"
			  " \\file   %s\n"
			  " \\author %s <%s>\n"
			  " \\date   %s\n"
			  " \n"
			  " \\brief  \n"
			  " \n"
			  " \n"
			  "*/")
		  fname
		  (user-full-name)
		  (user-mail-address)
		  (current-time-string)))))

(defun doxymacs-insert-file-comment ()
  "Inserts doxygen documentation for the current file at current point"
  (interactive "*")
  (save-excursion
    (let ((start (point)))
      (insert (doxymacs-file-comment))
      (let ((end (point)))
	(indent-region start end nil))))
  (end-of-line 6))