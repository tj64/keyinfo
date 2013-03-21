;; * keyinfo.el --- extract- keymap-info into an org file
;;   :PROPERTIES:
;;   :CUSTOM_ID: tj-keyinfo
;;   :CATEGORY: elisp-hacking
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

;; * Variables
;; ** Consts
;; ** Vars
;; ** Hooks
;; ** Fonts
;; ** Customs
;; *** Custom Groups 

;; (defgroup keyinfo nil
;;   "Library for extracting keymap-info into an org file."
;;   :prefix "keyinfo-"
;;   :group 'lisp 'org)

;; *** Custom Vars

  :group 'keyinfo
  :type '(repeat string))

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


(defun keyinfo-make-unbound-key-table (complexity out-buf-name &optional mode)
  "Transform unbound-key array from `describe-unbound-keys' into table.
If MODE is non-nil, it is activated in a tmp buffer before
`describe-unbound-keys' is called in that tmp-buffer, otherwise it is called
in the current buffer."
  (let* ((key-array
          (if mode
              (with-temp-buffer
                (funcall mode)
                (describe-unbound-keys complexity))
            (describe-unbound-keys complexity)))
         (key-list
          (split-string key-array "\n" 'OMIT-NULLS))
         (key-alist
          (let ((alist))
            (dolist (key key-list alist)
              (let ((key-split (split-string key " " 'OMIT-NULLS)))
              (push
               (cons (car key-split) (cadr key-split))
               alist)))))
         (key-alist-2
          (let ((newlst))
            (dolist (assc key-alist newlst)
              (let ((assc-split
                     (if (not (cdr assc))
                         (split-string (car assc) "-" 'OMIT-NULLS)
                       assc)))
                (push
                 (cons (car assc-split) (cdr assc-split))
                 newlst)))))
         (key-alist-3
          (let ((newlst2)
                (prefix-keys (list "M" "C" "S")))
            (dolist (assc key-alist-2 newlst2)
              (let ((assc-concat
                     (if (and
                          (nth 3 assc) 
                          (member (nth 1 assc) prefix-keys)
                          (member (nth 2 assc) prefix-keys))
                         (cons (car assc) (cadr assc) (cddr assc))
                       assc)))
                (push assc-concat newlst2)))))
         (prefix-key-set
          (delete-dups
           (let ((prefix))
             (dolist (assc key-alist-3 prefix)
               (push
                (car assc)
                prefix)))))
         (columns-alist
          (let ((cols)
                (i 1))
            (dolist (prefix prefix-key-set cols)
              (push
               (cons prefix i)
               cols)
              (setq i (1+ i))))))
    ;; FIXME delete
    (message
     (concat
      "key-alist: %s\n\nprefix-key-set: %s\n\n"
      "columns-alist: %s\n\n key-alist-2: %s \n\n key-alist-3: %s ")
     key-alist prefix-key-set columns-alist key-alist-2 key-alist-3)
    ;; FIXME replace with 'with-temp-buffer' and return buffer-string
    (with-current-buffer (get-buffer-create out-buf-name)
      (erase-buffer)
      (org-table-create
       (format "%dx%d" (length prefix-key-set) 2))
      (dolist (col columns-alist)
        (org-table-put 1 (cdr col) (car col) 'ALIGN))
      (org-table-goto-column 1)
      (org-table-goto-line 2)
      (message "dline: %s, point: %s" (org-table-current-dline) (point))
      (dolist (key key-alist-3)
        (let ((col (cdr (assoc (car key) columns-alist))))
          (org-table-goto-line 2)
          (while (not
                  (string-empty-p
                   (keyinfo-chomp (org-table-get-field col))))
            (org-table-next-row))
          ;; (or (org-table-next-row)
          ;;     (org-table-insert-row)))
          (org-table-put
           (org-table-current-line)
           col
           (or (car-safe (cdr-safe key))(cdr key)))))
      (org-table-align)
      )))




(defun keyinfo-make-keymap-table (&optional mode)
  "Transform keymap buffer-string from `describe-keymap' into table.
If MODE is non-nil, it is activated in a tmp buffer before
`describe-keymap' is called in that tmp-buffer, otherwise it is called
in the current buffer.")


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
      (describe-keymap map)
      (with-current-buffer outbuffer
        (erase-buffer)
        (insert
         "#+TITLE "
         (format "Keyinfo %s\n" (upcase (symbol-name map)))
         "#+DATE "
         (format "%s\n\n" (time-stamp-string))
         "\\Human readable keymap and unbound keys\\\n\n"
         "* Keymap\n\n"
         "* Unbound Keys\n\n")
        (goto-char
         (org-find-exact-headline-in-buffer "Keymap" nil 'POS-ONLY))
        (forward-line 2)
        ;; (org-babel-demarcate-block)
        (insert "#+begin_example\n")
        ;; insert the keymap
        (insert-buffer-substring-no-properties "*Help*")
        (kill-buffer "*Help*")
        (newline)
        (insert "#+end_example\n\n")
        (goto-char
         (org-find-exact-headline-in-buffer "Unbound Keys" nil 'POS-ONLY))
        (forward-line 2)
        ;; insert a table with unbound keys in mode
        (if mode
            (with-temp-buffer
              (and (functionp mode) (funcall mode))
              (describe-unbound-keys complexity))
          (describe-unbound-keys complexity))
        (insert "#+begin_example\n")
        (insert-buffer-substring-no-properties "*Unbound Keys*")
        (kill-buffer "*Unbound Keys*")
        (newline)
        (insert "#+end_example\n")
        (save-buffer)))))

;; ** Commands

(defun keyinfo-show-cmd (out mode num &optional ascii)
  "Calls keyinfo-show with user input.
OUT names the output file, MODE the mode-map to show, NUM the maximal
key-complexity, and ASCII, if non-nil, triggers export to ascii-buffer."
  (interactive
   (concat "FOutput filename: \nSMode name (as symbol): \n"
           "nMaximal key complexity \nP"))
  (if (boundp mode)
      (keyinfo-show
       (find-file-noselect out)
       mode
       num
       ascii)
    (message "Unknown Emacs mode entered.")))

;; * Menus and Keys
;; ** Menus
;; ** Keys
;; * Run Hooks and Provide

