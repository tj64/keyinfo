;; * keyinfo.el --- extract keymap-info into org file
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2013
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :credits:  Davis_Herring Drew_Adams
;;   :keywords: emacs keymaps unbound 
;;   :END:

;; ** Commentary

;; This library extracts information about Emacs mode keymaps and unbound keys
;; into an Org-mode file.

;; It is based on Davis Herring's
;; [[http://emacswiki.org/emacs/unbound.el][unbound.el]] and Drew Adam's
;; [[http://www.emacswiki.org/HelpPlus][help-fns+.el]].

;; For installation, download (or clone the gitrepo of) this library and put it
;; in a place where Emacs can find it. Do the same thing with the two required
;; libraries (see links above), then put

;; # #+begin_src emacs-lisp
;; #   (require 'keyinfo)
;; # #+end_src

;; in your Emacs init file. 

;; * Requires

(require 'unbound)
(require 'help-fns+)

;; * Defuns
;; ** Functions

;; from http://emacswiki.org/emacs/ElispCookbook#toc6
(defun keyinfo-chomp (str)
  "Chomp leading and trailing whitespace from STR."
  (save-excursion
    (save-match-data
      (while (string-match
              "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
              str)
        (setq str (replace-match "" t t str)))
      str)))

(defun keyinfo-make-unbound-key-table (complexity &optional mode)
  "Transform unbound-key array from `describe-unbound-keys' into table.
If MODE is non-nil, it is activated in a tmp buffer before
`describe-unbound-keys' is called in that tmp-buffer, otherwise it is called
in the current buffer."
  (let* ((key-array
          ;; original array of unbound-keys
          (if mode
              (with-temp-buffer
                (funcall mode)
                (describe-unbound-keys complexity))
            (describe-unbound-keys complexity)))
         ;; unbound-keys array converted into list
         (key-list
          (split-string
           (replace-regexp-in-string "|" "\\\\vert" key-array)
           "\n" 'OMIT-NULLS))
         ;; unbound-keys alist with keys splitted into prefix and key(s)
         (key-alist
          (let ((alist))
            (dolist (key key-list alist)
              (let ((case-fold-search nil)
                    (key-split
                     (split-string
                      (or
                       (and (string-match "^<[^>]+>" key)
                            (concat "[others]-- " key))
                       (and (string-match "^C-M-S-" key)
                            (replace-regexp-in-string
                             "^C-M-S-" "C-M-S--- " key))
                       (and (string-match "^C-M-" key)
                            (replace-regexp-in-string
                             "^C-M-" "C-M--- " key))
                       (and (string-match "^C-S-" key)
                            (replace-regexp-in-string
                             "^C-S-" "C-S--- " key))
                       (and (string-match "^M-S-" key)
                            (replace-regexp-in-string
                             "^M-S-" "M-S--- " key))
                       (and (string-match "^C-" key)
                            (replace-regexp-in-string
                             "^C-" "C--- " key))
                       (and (string-match "^M-" key)
                            (replace-regexp-in-string
                             "^M-" "M--- " key))
                       (and (string-match "^S-" key)
                            (replace-regexp-in-string
                             "^S-" "S--- " key)))
                      "-- " 'OMIT-NULLS)))
                (push
                 (list
                  (car key-split)
                  (mapconcat 'identity (cdr key-split) " "))
                 alist)))))
         ;; a set with all encountered prefixes
         (prefix-key-set
          (delete-dups
           (let ((prefix))
             (dolist (assc key-alist prefix)
               (push
                (car assc)
                prefix)))))
         ;; an alist with a unique column-number for each prefix
         (columns-alist
          (let ((cols)
                (i 1))
            (dolist (prefix prefix-key-set cols)
              (push
               (cons prefix i)
               cols)
              (setq i (1+ i))))))
    (with-temp-buffer
      ;; create initially empty table with adecuate number of columns
      (org-table-create
       (format "%dx%d" (length prefix-key-set) 2))
      ;; insert column headers
      (dolist (col columns-alist)
        (org-table-put 1 (cdr col) (car col) 'ALIGN))
      (org-table-goto-column 1)
      (org-table-goto-line 2)
      ;; loop over unbound keys alist
      (dolist (key key-alist)
        ;; find the column for insertion
        (let ((col (cdr (assoc (car key) columns-alist))))
          (org-table-goto-line 2)
          ;; find (or create) the cell for insertion
          (while (not
                  (string-empty-p
                   (keyinfo-chomp (org-table-get-field col))))
            (org-table-next-row)
            ;; deal with 'org-table-next-row' special treatment of # and $
            (cond ((string-equal
                    (keyinfo-chomp (org-table-get-field 1)) "#")
                   (org-table-get-field 1 "[#]"))
                  ((string-equal
                    (keyinfo-chomp (org-table-get-field 1)) "$")
                   (org-table-get-field 1 "[$]"))))
          ;; insert key(s) into cell
          (org-table-put
           (org-table-current-line)
           col
           (or (car-safe (cdr-safe key))(cdr key)))))
      ;; align table
      (org-table-align)
      ;; return complete table
      (buffer-string))))

(defun keyinfo-show (&optional out mode num ascii)
  "Insert info about keymap and unbound keys in Org file.
If OUT is non-nil, it should be a relative or absolute file-name
as string. If MODE is non-nil, it should be a string or symbol
naming a (minor-)mode with a defined keymap. If NUM is non-nil
and an integer, unbound keys with complexity up to NUM will be
shown, otherwise up to complexity 8. If ASCII is non-nil, the Org
output file will be exported to an ASCII buffer that might be
saved manually."
  (let* ((complexity  (or num 8))
         (mode (or mode major-mode))
         (mode-strg (if (stringp mode) mode (symbol-name mode)))
         (outbuffer
          (or out
              (find-file-noselect
               (expand-file-name
                (concat
                 "keyinfo-"
                 mode-strg
                 ".org")
                (uniquify-buffer-file-name (current-buffer))))))
         (map (intern (concat mode-strg "-map")))
         (unbound-keys-array nil))
    (if (not (boundp map))
        (message "Mode map %s not bound." map)
      ;; create *Help* buffer with human readable keymap-info
      (describe-keymap map)
      (with-current-buffer outbuffer
        (erase-buffer)
        ;; create title and outline structure
        (insert
         "#+TITLE "
         (format "Keyinfo %s\n" (upcase (symbol-name map)))
         "#+DATE "
         (format "%s\n\n" (time-stamp-string))
         "\\Human readable keymap and unbound keys\\\n\n"
         "* Keymap\n\n"
         "* Unbound Keys\n\n")
        ;; insert *Help* buffer keymap-info into EXAMPLE block
        (goto-char
         (org-find-exact-headline-in-buffer "Keymap" nil 'POS-ONLY))
        (forward-line 2)
        (insert "#+begin_example\n")
        (insert-buffer-substring-no-properties "*Help*")
        (kill-buffer "*Help*")
        (newline)
        (insert "#+end_example\n\n")
        ;; insert compact Org-mode table with unbound keys
        (goto-char
         (org-find-exact-headline-in-buffer "Unbound Keys" nil 'POS-ONLY))
        (forward-line 2)
        (insert
         (format "Unbound keys with complexity at most %d\n\n" complexity))
        (insert "#+begin_example\n")
        (insert (keyinfo-make-unbound-key-table complexity))
        (newline)
        (insert "#+end_example\n")
        (save-buffer)
        ;; (optionally) create and display ASCII buffer with keyinfo
        (or
         (and ascii
              (display-buffer
               (org-export-to-buffer
                'ascii
                (concat (car (split-string (buffer-name) ".org" 'OMIT-NULLS))
                        ":ASCII"))))
         ;; or display Org output buffer
         (display-buffer (current-buffer)))))))

;; ** Commands

(defun keyinfo-show-cmd (ascii out mode num)
  "Calls keyinfo-show with user input.
OUT names the output file, MODE the mode-map to show, NUM the maximal
key-complexity. A prefix arg triggers export to an ASCII-buffer."
  (interactive "P\nFOutput filename: \nSMode name as symbol: \nnMaximal key complexity: ")
  (if (or (require mode nil 'NOERROR)
          (require
           (intern
            (car (split-string (symbol-name mode) "-mode" 'OMIT-NULLS)))
           nil 'NOERROR))
      (keyinfo-show
       (find-file-noselect out)
       mode
       num
       ascii)
    (message "Unknown Emacs mode entered.")))

;; * Run Hooks and Provide

(provide 'keyinfo)

;;; keyinfo.el ends here
