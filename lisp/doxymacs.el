;;; doxymacs.el --- ELisp package for making doxygen related stuff easier.
;;
;; Copyright (C) 2001 Ryan T. Sammartino
;;
;; Author: Ryan T. Sammartino <ryants@home.com>
;;      Kris Verbeeck <kris.verbeeck@advalvas.be>
;; Contributor: Andreas Fuchs
;; Created: 24/03/2001
;; Version: 0.1.1
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
;;
;; $Id: doxymacs.el,v 1.28 2001/05/10 03:00:39 ryants Exp $

;; Commentary:
;;
;; - Put this file and xml-parse somewhere in your {X}Emacs load-path.
;; - Customise the variables doxymacs-doxygen-root and doxymacs-doxygen-tags.
;; - Put (require 'doxymacs) in your .emacs
;; - Invoke doxymacs-mode with M-x doxymacs-mode
;; - Default key bindings are:
;;   - C-c d ? will look up documentation for the symbol under the point.
;;   - C-c d r will rescan your Doxygen tags file.
;;   - C-c d f will insert a Doxygen comment for the next function.
;;   - C-c d i will insert a Doxygen comment for the current file.
;;   - C-c d m will insert a blank multiline Doxygen comment.
;;   - C-c d s will insert a blank singleline Doxygen comment.

;; Change log:
;;
;; 09/05/2001 - change C-? to C-c d ?, since hitting DEL also triggers C-?
;;            - update progress while parsing XML file
;;            - version 0.1.1
;; 07/05/2001 - minor mode thanks to Kris, and default key map.
;;            - released as version 0.1.0 (Alpha)
;; 06/05/2001 - Now using tempo templates for the comments... also allow for
;;              user defined templates.
;; 29/04/2001 - The doxytags.pl PERL script is no longer necessary, as we can
;;              now parse the XML file that doxygen creates directly.
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

;; TODO:
;;
;; - better end-user documentation
;; - test this on other versions of {X}Emacs other than the one I'm 
;;   using (XEmacs 21.1.14)
;; - fix all FIXMEs (of course)
;; - other stuff?

;; Front matter and variables

(provide 'doxymacs)

(require 'custom)
(require 'xml-parse)
(require 'url)
(require 'w3-cus)
(require 'tempo)

(defgroup doxymacs nil
  "Find documentation created by Doxygen, and create Doxygen comments"
  :group 'tools)

(defcustom doxymacs-doxygen-root
  ""
  "*Root for doxygen documentation (URL)."
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-doxygen-tags
  ""
  "*File name or URL that contains doxygen tags."
  :type 'string
  :group 'doxymacs)

(defcustom doxymacs-doxygen-style
  "JavaDoc"
  "*The style of comments to insert into code.
See http://www.stack.nl/~dimitri/doxygen/docblocks.html#docblocks for examples
of the two styles."
  :type '(radio (const :tag "JavaDoc" "JavaDoc") (const :tag "Qt" "Qt"))  
  :group 'doxymacs)


;; here's a hack... a one-size-fits-all set user template func.
;; maybe there's a better way of doing this?
(defun doxymacs-set-user-template (symbol value)
  (let* ((symbol-name (prin1-to-string symbol))
	 (template-name (concat "user-"
				(substring symbol-name 
					   (length "doxymacs-")
					   (- 0 (length "-template")))))
	 (func-name (car 
		     (read-from-string 
		      (concat "tempo-template-" template-name)))))
    (if value
	(tempo-define-template template-name value)
      (fmakunbound func-name))))

(defcustom doxymacs-blank-multiline-comment-template
  nil
  "*A tempo template to insert when calling doxymacs-insert-blank-multiline-comment.  
If nil, then a default template based on the current style as indicated
by doxymacs-doxygen-style will be used.  For help with tempo templates,
see "
  :type 'list
  :set 'doxymacs-set-user-template
  :group 'doxymacs)

(defcustom doxymacs-blank-singleline-comment-template
  nil
  "*A tempo template to insert when calling doxymacs-insert-blank-singleline-comment.  
If nil, then a default template based on the current style as indicated
by doxymacs-doxygen-style will be used.  For help with tempo templates,
see "
  :type 'list
  :set 'doxymacs-set-user-template
  :group 'doxymacs)

(defcustom doxymacs-file-comment-template
  nil
  "*A tempo template to insert when calling doxymacs-insert-file-comment.  
If nil, then a default template based on the current style as indicated
by doxymacs-doxygen-style will be used.  For help with tempo templates,
see "
  :type 'list
  :set 'doxymacs-set-user-template
  :group 'doxymacs)

(defcustom doxymacs-function-comment-template
  nil
  "*A tempo template to insert when calling doxymacs-insert-function-comment.  
If nil, then a default template based on the current style as indicated
by doxymacs-doxygen-style will be used.  Note that the function 
doxymacs-find-next-func is available to you... it returns an assoc list
with the function's name (BUG: it may be incorrect for C++ operator-style 
declerations), argument list (BUG: may be incorrect for parameters that
require parentheses), and return value:

(cdr (assoc 'func (doxymacs-find-next-func))) is the function name (string).
(cdr (assoc 'args (doxymacs-find-next-func))) is a list of arguments.
(cdr (assoc 'return (doxymacs-find-next-func))) is the return type (string).

The argument list is a list of strings.  For help with tempo templates,
see "
  :type 'list
  :set 'doxymacs-set-user-template
  :group 'doxymacs)


(defvar doxymacs-tags-buffer nil
  "The buffer with our doxytags")

;; The structure of this list has been chosen for ease of use in the
;; completion functions.  The structure is as follows:
;; ( (symbol-1 . ((description-1a . url-1a) (description-1b . url-1b)))
;;   (symbol-2 . ((description-2a . url-2a)))
;;   ... )
(defvar doxymacs-completion-list nil
  "The list with doxytags completions")

(defvar doxymacs-completion-buffer "*Completions*"
  "The buffer used for displaying multiple completions")



;; Minor mode implementation

(defvar doxymacs-mode nil 
  "nil disables doxymacs, non-nil enables.")

(make-variable-buffer-local 'doxymacs-mode)

(defun doxymacs-mode (&optional arg)
  "Doxymacs Minor mode.
With no argument, this command toggles doxymacs mode.
With a prefix argument ARG, turn doxymacs minor mode on iff ARG is positive."
  (interactive "P")
  (setq doxymacs-mode
        (if (null arg)
            ;; Toggle mode
            (not doxymacs-mode)
          ;; Enable/Disbale according to arg
          (> (prefix-numeric-value arg) 0))))

(defvar doxymacs-mode-map (make-sparse-keymap) 
  "Keymap for doxymacs minor mode.")

(define-key doxymacs-mode-map "\C-cd?"
  'doxymacs-lookup)
(define-key doxymacs-mode-map "\C-cdr"
  'doxymacs-rescan-tags)

(define-key doxymacs-mode-map "\C-cdf"
  'doxymacs-insert-function-comment)
(define-key doxymacs-mode-map "\C-cdi"
  'doxymacs-insert-file-comment)
(define-key doxymacs-mode-map "\C-cdm"
  'doxymacs-insert-blank-multiline-comment)
(define-key doxymacs-mode-map "\C-cds"
  'doxymacs-insert-blank-singleline-comment)


;;;###autoload
(or (assoc 'doxymacs-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(doxymacs-mode " doxy") minor-mode-alist)))

(or (assoc 'doxymacs-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'doxymacs-mode doxymacs-mode-map) 
		minor-mode-map-alist)))


;;These functions have to do with looking stuff up in doxygen generated
;;documentation

;;doxymacs-load-tag
;;This loads the tags file generated by doxygen into the buffer *doxytags*.  
(defun doxymacs-load-tags ()
  "Loads a tags file"
  (if (or (eq doxymacs-tags-buffer nil)
	  (eq (buffer-live-p doxymacs-tags-buffer) nil))
      (progn
	(setq doxymacs-tags-buffer (generate-new-buffer "*doxytags*"))
	(message (concat "Loading " doxymacs-doxygen-tags))
	(let ((currbuff (current-buffer)))
	  (if (file-regular-p doxymacs-doxygen-tags)
	      ;;It's a regular file, so just grab it.
	      (progn
		(set-buffer doxymacs-tags-buffer)
		(insert-file-contents doxymacs-doxygen-tags))
	    ;; Otherwise, try and grab it as a URL
	    (progn
	      (if (url-file-exists doxymacs-doxygen-tags)
		  (progn
		    (set-buffer doxymacs-tags-buffer)
		    (url-insert-file-contents doxymacs-doxygen-tags)
		    (set-buffer-modified-p nil))
		(error (concat 
			"Tag file " doxymacs-doxygen-tags " not found.")))))
	  (set-buffer currbuff)))))

(defun doxymacs-add-to-completion-list (symbol desc url)
  "Add a symbol to our completion list, along with its description and URL"
  (let ((check (assoc symbol doxymacs-completion-list)))
    (if check
	;; There is already a symbol with the same name in the list
	(if (not (assoc desc (cdr check)))
	    ;; If there is not yet a symbol with this desc, add it
	    ;; FIXME: what to do if there is already a symbol??
	    (setcdr check (cons (cons desc url)
				(cdr check))))
      ;; There is not yet a symbol with this name in the list
      (setq doxymacs-completion-list
	    (cons (cons symbol (list (cons desc url)))
		  doxymacs-completion-list)))))


(defun doxymacs-xml-progress-callback (amount-done)
  (message (concat "Parsing " doxymacs-doxygen-tags "... " 
		   (format "%0.1f" amount-done) "%%")))

(defun doxymacs-fill-completion-list ()
  "Load and parse the tags from the *doxytags* buffer, constructing our 
doxymacs-completion-list from it"
  (doxymacs-load-tags)
  (let ((currbuff (current-buffer)))
    (set-buffer doxymacs-tags-buffer)
    (goto-char (point-min))
    (setq doxymacs-completion-list nil)
    (let ((xml (read-xml 'doxymacs-xml-progress-callback))) ;Parse the XML file
      (let* ((compound-list (xml-tag-children xml))
	     (num-compounds (length compound-list))
	     (curr-compound-num 0))
	(if (not (string= (xml-tag-name xml) "tagfile"))
	    (error (concat "Invalid tag file: " doxymacs-doxygen-tags))
	  ;; Go through the compounds, adding them and their members to the
	  ;; completion list.
	  (while compound-list
	    (let* ((curr-compound (car compound-list))
		   (compound-name (cadr (xml-tag-child curr-compound "name")))
		   (compound-kind (xml-tag-attr curr-compound "kind"))
		   (compound-url (cadr 
				  (xml-tag-child curr-compound "filename")))
		   (compound-desc (concat compound-kind " " compound-name)))
	      ;; Add this compound to our completion list
	      (doxymacs-add-to-completion-list compound-name
					       compound-desc
					       compound-url)
	      ;; Add its members
	      (doxymacs-add-compound-members curr-compound
					     compound-name
					     compound-url)

	      
	      ;; On to the next compound
	      (message (concat 
			"Building completion table... "
			(format "%0.1f"
				(* (/ 
				    (float curr-compound-num) 
				    (float num-compounds)) 
				   100))
			"%%"))
	      (setq curr-compound-num (1+ curr-compound-num))
	      (setq compound-list (cdr compound-list)))))))
    ;; Don't need the doxytags buffer anymore
    (message "Done.")
    (kill-buffer doxymacs-tags-buffer)
    (set-buffer currbuff)))

(defun doxymacs-add-compound-members (compound compound-name compound-url)
  "Get the members of the given compound"
  (let ((children (xml-tag-children compound)))
    ;; Run through the children looking for ones with the "member" tag
    (while children
      (let* ((curr-child (car children)))
	(if (string= (xml-tag-name curr-child) "member")
	    ;; Found a member.  Throw it on the list.
	    (let* ((member-name (cadr (xml-tag-child curr-child "name")))
		   (member-anchor (cadr (xml-tag-child curr-child "anchor")))
		   (member-url (concat compound-url "#" member-anchor))
		   (member-args (if (cdr (xml-tag-child curr-child "arglist"))
				    (cadr (xml-tag-child curr-child "arglist"))
				  ""))
		   (member-desc (concat compound-name "::" 
					member-name member-args)))
	      (doxymacs-add-to-completion-list member-name
					       member-desc
					       member-url)))
	(setq children (cdr children))))))

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
  (if (buffer-live-p doxymacs-tags-buffer)
      (kill-buffer doxymacs-tags-buffer))
  (doxymacs-fill-completion-list))


;; These functions have to do with inserting doxygen commands in code

;; FIXME
;; So, in the source code for XEmacs 21.1.14, they commented out the
;; definition of deactivate-mark for some reason... and the tempo package
;; needs it.  So, here is a placeholder just to get it to stop
;; complaining. This is a hack, since I don't know what the proper fix
;; should be.
(if (not (fboundp 'deactivate-mark))
    (defsubst deactivate-mark ()
      (zmacs-deactivate-region))) ;; Is this correct?
;; Also need a hack for mark-active
(if (not (boundp 'mark-active))
    (defvar mark-active nil)) ;; Is this correct? Probably not.


;; Default templates

(tempo-define-template
 "JavaDoc-blank-multiline-comment"
 '("/**" > n "* " p > n "* " > n "*/" > n))

(tempo-define-template 
 "Qt-blank-multiline-comment"
 '("//! " p > n "/*! " > n > n "*/" > n))

(tempo-define-template
 "JavaDoc-blank-singleline-comment"
 '("/// " > p))

(tempo-define-template
 "Qt-blank-singleline-comment"
 '("//! " > p))

(tempo-define-template
 "JavaDoc-file-comment"
 '("/**" > n 
   " * @file   "  
   (if (buffer-file-name) 
       (file-name-nondirectory (buffer-file-name)) 
     "") > n
   " * @author " (user-full-name) " <" (user-mail-address) ">" > n
   " * @date   " (current-time-string) > n
   " * " > n
   " * @brief  " (p "Brief description of this file: ") > n
   " * " > n
   " * " p > n
   " */" > n))

(tempo-define-template
 "Qt-file-comment"
 '("/*!" > n
   " \\file   "
   (if (buffer-file-name) 
       (file-name-nondirectory (buffer-file-name)) 
     "") > n
   " \\author " (user-full-name) " <" (user-mail-address) ">" > n
   " \\date   " (current-time-string) > n
   " " > n
   " \\brief  " (p "Brief description of this file: ") > n
   " " > n
   " " p > n
   "*/" > n))


;; Need to pass in style directly instead of looking at 
;; doxymacs-doxygen-style since user might call the tempo-template-
;; functions directly
(defun doxymacs-parm-tempo-element (parms style)
  "Inserts tempo elements for the given parms in the given style"
  (if parms
      (let ((prompt (concat "Parameter " (car parms) ": ")))
	(cond
	 ((string= style "JavaDoc")
	  (list 'l " * @param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms) style)))
	 ((string= style "Qt")
	  (list 'l " \\param " (car parms) " " (list 'p prompt) '> 'n
		(doxymacs-parm-tempo-element (cdr parms) style)))))
    nil))


(tempo-define-template
 "JavaDoc-function-comment"
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l 
	  "/** " '> 'n
	  " * " 'p '> 'n
	  " * " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)) "JavaDoc")
	  " * " '> 'n
	  (unless (string= (cdr (assoc 'return next-func)) "void")
	    '(l " * @return " (p "Returns: ") > n))
	  " */" '>)
       (progn
	 (beep)
	 nil)))))


(tempo-define-template
 "Qt-function-comment"
 '((let ((next-func (doxymacs-find-next-func)))
     (if next-func
	 (list
	  'l 
	  "//! " 'p '> 'n
	  "/*! " '> 'n
	  " " '> 'n
	  (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)) "Qt")
	  " " '> 'n
	  (unless (string= (cdr (assoc 'return next-func)) "void")
	    '(l "  \\return " (p "Returns: ") > n))
	  " */" '>)
       (progn
	 (beep)
	 nil)))))


;; Wrapper functions for the above... these check to see if the user
;; has his own template for the particular comment that is going to be
;; inserted and calls it if it is defined... otherwise, calls the default
;; template depending on the currently set style.

(defun doxymacs-insert-blank-multiline-comment ()
  "Inserts a multi-line blank doxygen comment at the current point"
  (interactive "*")
  (if (fboundp 'tempo-template-user-blank-multiline-comment)
      ;; Use the user's template
      (tempo-template-user-blank-multiline-comment)
    (cond
     ((string= doxymacs-doxygen-style "JavaDoc")
      (tempo-template-JavaDoc-blank-multiline-comment))
     ((string= doxymacs-doxygen-style "Qt")
      (tempo-template-Qt-blank-multiline-comment)))))
    
(defun doxymacs-insert-blank-singleline-comment ()
  "Inserts a single-line blank doxygen comment at current point"
  (interactive "*")
  (if (fboundp 'tempo-template-user-blank-singleline-comment)
      ;; Use the user's template
      (tempo-template-user-blank-singleline-comment)
    (cond
     ((string= doxymacs-doxygen-style "JavaDoc")
      (tempo-template-JavaDoc-blank-singleline-comment))
     ((string= doxymacs-doxygen-style "Qt")
      (tempo-template-Qt-blank-singleline-comment)))))

(defun doxymacs-insert-file-comment ()
  "Inserts doxygen documentation for the current file at current point"
  (interactive "*")
  (if (fboundp 'tempo-template-user-file-comment)
      ;; Use the user's template
      (tempo-template-user-file-comment)
    (cond
     ((string= doxymacs-doxygen-style "JavaDoc")
      (tempo-template-JavaDoc-file-comment))
     ((string= doxymacs-doxygen-style "Qt")
      (tempo-template-Qt-file-comment)))))

(defun doxymacs-insert-function-comment ()
  "Inserts doxygen documentation for the next function declaration at 
current point"
  (interactive "*")
  (if (fboundp 'tempo-template-user-function-comment)
      ;; Use the user's template
      (tempo-template-user-function-comment)
    (cond
     ((string= doxymacs-doxygen-style "JavaDoc")
      (tempo-template-JavaDoc-function-comment))
     ((string= doxymacs-doxygen-style "Qt")
      (tempo-template-Qt-function-comment)))))


;; These are helper functions that search for the next function
;; declerations/definition and extract its name, return type and 
;; argument list.  Used for documenting functions.

(defun doxymacs-extract-args-list (args-string)
  "Extracts the arguments from the given list (given as a string)"
  (save-excursion
    (if (string= args-string "")
	nil
      (doxymacs-extract-args-list-helper (split-string args-string ",")))))

(defun doxymacs-extract-args-list-helper (args-list)
  "Recursively get names of arguments"
  (save-excursion
    (if args-list
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
	 (doxymacs-extract-args-list-helper (cdr args-list))))
      nil)))


;; FIXME
;; This gets confused by the following examples:
;; - void qsort(int (*comp)(void *, void *), int left, int right);
;; - int f(int (*daytab)[5], int x);
;; - Anything that doesn't declare its return value
;; - Also gets the names of certain C++ operator-style declerations wrong
(defun doxymacs-find-next-func ()
  "Returns a list describing next function declaration, or nil if not found"
  (interactive)
  (save-excursion    
    (if (re-search-forward
	 (concat 
	  ;;I stole the following from func-menu.el
	  "\\(\\(template\\s-+<[^>]+>\\s-+\\)?"   ; template formals
	  "\\([a-zA-Z0-9_*&<,>:]+\\s-+\\)?"       ; type specs
	  "\\([a-zA-Z0-9_*&<,>\"]+\\s-+\\)?"
	  "\\([a-zA-Z0-9_*&<,>]+\\)\\s-+\\)"      ; return type
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

;;; doxymacs.el ends here