;; doxymacs.el
;;
;; $Id: doxymacs.el,v 1.16 2001/04/22 08:19:53 ryants Exp $
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
;; 22/04/2001 - Function documentation.
;; 18/04/2001 - Going with Kris' "new style" look up code.  It's excellent.
;;            - Incorprated Andreas Fuchs' patch for loading tags from a
;;              URL.
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
;; - add ability to automagically insert doxygen comments.
;;   - kind of have that now... would like ability to define 'user' styles
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
;  "file:///home/ryants/projects/doxymacs/example/doc/doxy.tag"
  "../example/doc/doxy.tag"
  "*File name or URL that contains doxygen tags"
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-doxygen-style
  "JavaDoc"
  "*The style of comments to insert into code"
  :type '(radio (const :tag "JavaDoc" "JavaDoc") (const :tag "Qt" "Qt"))  
  :group 'doxymacs)

(defvar doxymacs-tags-buffer nil
  "The buffer with our doxytags")

;; The structure of this list has been chosen for ease of use in the
;; completin functions.  The structure is as follows:
;; ( (symbol-1 . ((description-1a . url-1a) (description-1b . url-1b)))
;;   (symbol-2 . ((description-2a . url-2a)))
;;   ... )
(defvar doxymacs-completion-list nil
  "The list with doxytags completions")

(defvar doxymacs-completion-buffer "*Completions*"
  "The buffer used for displaying multiple completions")


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
	  (if (file-regular-p doxymacs-doxygen-tags)
	      ;;It's a regular file, so just grab it.
	      (progn
		(set-buffer doxymacs-tags-buffer)
		(insert-file-contents doxymacs-doxygen-tags))
	    ;; Otherwise, try and grab it as a URL
	    ;; FIXME  What if something goes wrong?
	    (let ((url-working-buffer 
		   (cdr (url-retrieve doxymacs-doxygen-tags))))
	      (set-buffer url-working-buffer)
	      (url-uncompress)
	      (set-buffer doxymacs-tags-buffer)
	      (insert-buffer url-working-buffer)
	      (kill-buffer url-working-buffer)))
	  (set-buffer currbuff)))))

;; doxymacs-fill-completion-list
;; Parses the *doxytags* buffer and constructs the doxymacs-completion-list 
;; out of it.
(defun doxymacs-fill-completion-list ()
  "For now it (loads and) parses the tags from the *doxytags* buffer"
  (doxymacs-load-tags)
  (let ((currbuff (current-buffer))
        (regexp (concat "^\\(.*\\)\t\\(.*\\)\t\\(.*\\)$")))
    (set-buffer doxymacs-tags-buffer)
    (goto-char (point-min))
    (setq doxymacs-completion-list nil)
    (while (re-search-forward regexp nil t)
      (let* ((symbol (match-string 1))
             (url (match-string 2))
             (desc (match-string 3))
             (check (assoc symbol doxymacs-completion-list)))
        (if check
            ;; There is already a symbol with the same name in the list
            (if (not (assoc desc (cdr check)))
                ;; If there is not yet a symbol with this description, add it
                ;; FIXME: what to do if there is already a symbol??
                (setcdr check (cons (cons desc url)
                                    (cdr check))))
          ;; There is not yet a symbol with this name in the list
          (setq doxymacs-completion-list 
		(cons (cons symbol (list (cons desc url)))
		      doxymacs-completion-list)))))
    (set-buffer currbuff)))

(defun doxymacs-display-url (url)
  "Displays the given match"
  (browse-url (concat doxymacs-doxygen-root "/" url)))

(defun doxymacs-lookup (symbol)
  "Look up the symbol under the cursor in doxygen"
  (interactive 
   (save-excursion
     (if (eq doxymacs-completion-list nil)
	 ;;Build our completion list if not already done
	 (doxymacs-fill-completion-list))
     (let ((symbol (completing-read 
		    "Look up: " 
		    doxymacs-completion-list nil nil (symbol-near-point))))
	 (list symbol))))
  (let ((url (doxymacs-symbol-completion symbol doxymacs-completion-list)))
    (if url
        (doxymacs-display-url url))))

(defun doxymacs-symbol-completion (initial collection &optional pred)
  "Do completion for given symbol"
  (let ((completion (try-completion initial collection pred)))
    (cond ((eq completion t)
           ;; Only one completion found.  Validate it.
           (doxymacs-validate-symbol-completion initial collection pred))
          ((null completion)
           ;; No completion found
           (message "No documentation for '%s'" initial)
           (ding))
          (t
           ;; There is more than one possible completion
           (let ((matches (all-completions initial collection pred)))
             (with-output-to-temp-buffer doxymacs-completion-buffer
               (display-completion-list (sort matches #'string-lessp))))
           (let ((completion (completing-read 
			      "Select: " 
			      collection pred nil initial)))
             (delete-window (get-buffer-window doxymacs-completion-buffer))
             (if completion
                 ;; If there is a completion, validate it.
                 (doxymacs-validate-symbol-completion 
		  completion collection pred)
               ;; Otherwise just return nil
               nil))))))

(defun doxymacs-validate-symbol-completion (initial collection &optional pred)
  "Checks whether the symbol (initial) has multiple descriptions, and if so
continue completion on those descriptions.  In the end it returns the URL for
the completion or nil if canceled by the user."
  (let ((new-collection (cdr (assoc initial collection))))
    (if (> (length new-collection) 1)
        ;; More than one
        (doxymacs-description-completion "" new-collection pred)
      ;; Only one, return the URL
      (cdar new-collection))))

(defun doxymacs-description-completion (initial collection &optional pred)
  "Do completion for given description"
  (let ((matches (all-completions initial collection pred)))
    (with-output-to-temp-buffer doxymacs-completion-buffer
      (display-completion-list (sort matches #'string-lessp))))
  (let ((completion (completing-read "Select: " collection pred nil initial)))
    (delete-window (get-buffer-window doxymacs-completion-buffer))
    (if completion
        ;; Return the URL if there is a completion
        (cdr (assoc completion collection)))))

;;This is mostly a convenience function for the user
(defun doxymacs-rescan-tags ()
  "Rescan the tags file"
  (interactive)
  (doxymacs-fill-completion-list))


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


(defun doxymacs-extract-args-list (args-string)
  "Extracts the arguments from the given list (given as a string)"
  (save-excursion
    (if (equal args-string "")
	nil
      (doxymacs-extract-args-list-helper (split-string args-string ",")))))

(defun doxymacs-extract-args-list-helper (args-list)
  "Recursively get names of arguments"
  (save-excursion
    (if (eq args-list nil)
	nil
      (if (string-match 
	   (concat
	    "\\([a-zA-Z0-9_]+\\)\\s-*" ; arg name
	    "\\(\\[\\s-*[a-zA-Z0-9_]*\\s-*\\]\\)*" ; optional array bounds
	    "\\(=\\s-*.+\\s-*\\)?" ;optional assignment
	    "\\s-*$" ; end
	    )
	   (car args-list))
	  (cons
	   (substring (car args-list) (match-beginning 1) (match-end 1))
	   (doxymacs-extract-args-list-helper (cdr args-list)))
	(cons
	 (car args-list)
	 (doxymacs-extract-args-list-helper (cdr args-list)))))))


;; FIXME
;; This gets confused by the following examples:
;; - void qsort(int (*comp)(void *, void *), int left, int right);
;; - int f(int (*daytab)[5], int x);
;; - Anything that doesn't declare its return value
;; NOTE
;; - It doesn't really matter if the function name is incorrect... the 
;;   important bits are the arguments and the return value... those need
;;   to be correct for sure.
(defun doxymacs-find-next-func ()
  "Returns a list describing next function devlaration, or nil if not found"
  (interactive)
  (save-excursion    
    (if (re-search-forward
	 (concat 
	  ;;I stole the following from func-menu.el
	  "\\(\\(template\\s-+<[^>]+>\\s-+\\)?"   ; template formals
	  "\\([a-zA-Z0-9_*&<,>:]+\\s-+\\)?"       ; type specs
	  "\\([a-zA-Z0-9_*&<,>\"]+\\s-+\\)?"
	  "\\([a-zA-Z0-9_*&<,>]+\\)\\s-+\\)"      ; return val
	  "\\(\\([a-zA-Z0-9_&~:<,>*]\\|\\(\\s +::\\s +\\)\\)+\\)"
	  "\\(o?perator\\s *.[^(]*\\)?\\(\\s-\\|\n\\)*(" ; name
	  "\\([^)]*\\))" ; arg list
	  ) nil t)
	(list (cons 'func (buffer-substring (match-beginning 6)
					    (match-end 6)))
	      (save-match-data 
		(cons 'args (doxymacs-extract-args-list
			     (buffer-substring (match-beginning 11)
					       (match-end 11)))))
	      (cons 'return (buffer-substring (match-beginning 5)
					      (match-end 5))))
      nil)))

(defun doxymacs-parm-comment (parms)
  "Inserts doxygen documentation for the given parms"
  (if (equal parms nil)
      ""
    (if (equal doxymacs-doxygen-style "JavaDoc")
	(concat " * @param " (car parms) "\t\n"
		(doxymacs-parm-comment (cdr parms)))
      (concat "  \\param " (car parms) "\t\n"
	      (doxymacs-parm-comment (cdr parms))))))

(defun doxymacs-func-comment (func)
  "Inserts doxygen documentation for the given func"
  (if (equal doxymacs-doxygen-style "JavaDoc")
      (concat "/**\n"
	      " * \n"
	      " * \n"
	      (when (cdr (assoc 'args func))
		(doxymacs-parm-comment (cdr (assoc 'args func))))
	      (unless (equal (cdr (assoc 'return func)) "void")
		" * @return \n")
	      " */")
    (concat "//! \n"
	    "/*!\n"
	    " \n"
	    (when (cdr (assoc 'args func))
	      (doxymacs-parm-comment (cdr (assoc 'args func))))
	    (unless (equal (cdr (assoc 'return func)) "void")
	      "  \\return \n")
	    " */")))

(defun doxymacs-insert-function-comment ()
  "Inserts doxygen documentation for the next function declaration at 
current point"
  (interactive "*")
  (save-excursion
    (widen)
    (let ((start (point))
	  (next-func (doxymacs-find-next-func)))
      (if (not (equal next-func nil))
	  (insert (doxymacs-func-comment next-func))
	(beep))
      (let ((end (point)))
	(indent-region start end nil))))
  (if (equal doxymacs-doxygen-style "JavaDoc")
      (end-of-line 2))
  (end-of-line))

